mod error;
mod registry;
mod set;

pub use self::error::TestSuiteError;
pub use self::registry::{DefaultTestSuiteRegistry, TestSuiteRegistry, TestSuiteRegistryExt};
pub use self::set::{TestSuiteKey, TestSuiteSet};

use std::{
    borrow::Cow,
    collections::BTreeSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use intrusive_collections::RBTreeAtomicLink;
use litcheck::{
    diagnostics::{DiagResult, IntoDiagnostic, Report, SourceSpan, WrapErr},
    Input, StaticCow,
};
use parking_lot::Mutex;
use serde::Deserialize;

use crate::{test::TestConfig, Config};

#[derive(Deserialize)]
pub struct TestSuite {
    #[serde(skip, default)]
    link: RBTreeAtomicLink,
    /// The name of the test suite, may not be empty.
    pub name: toml::Spanned<Arc<str>>,
    /// The source path from which the test suite configuration was loaded
    #[serde(skip, default)]
    pub path: Option<Arc<Path>>,
    /// The filesystem path which is the root of the test suite, and will be scanned
    /// for tests.
    ///
    /// If provided, and not absolute, it will be relative to the config file itself.
    ///
    /// Defaults to the directory containing the config file.
    #[serde(default = "empty_spanned_path")]
    pub source_dir: toml::Spanned<StaticCow<Path>>,
    /// The filesystem path which will be the working directory for tests that are
    /// run, and where temporary files will be placed.
    ///
    /// If provided, and not absolute, it will be relative to the config file itself.
    ///
    /// Defaults to a temporary directory created via [std::env::temp_dir]
    #[serde(default = "empty_spanned_path")]
    pub working_dir: toml::Spanned<StaticCow<Path>>,
    /// If a temporary directory is associated with this suite, it will be stored in
    /// this field. When the suite is dropped, the temporary directory will be cleaned
    /// up as well.
    #[serde(skip, default)]
    pub temp_dir: Option<tempdir::TempDir>,
    /// The suite-level configuration used for all tests in this suite.
    ///
    /// Local configuration files can be used to override this configuration
    /// on a directory-by-directory basis.
    #[serde(flatten)]
    pub config: Arc<TestConfig>,
    /// The set of paths relative to `source_dir` which will be searched for tests.
    ///
    /// If empty, all of `source_dir` and its children will be searched.
    #[serde(skip)]
    search_paths: Mutex<BTreeSet<PathBuf>>,
}
impl TestSuite {
    /// Get the name of this suite as a string
    #[inline]
    pub fn name(&self) -> &str {
        //self.id.name()
        self.name.as_ref()
    }

    pub fn span(&self) -> core::ops::Range<usize> {
        self.name.span()
    }

    pub fn id(&self) -> TestSuiteKey {
        TestSuiteKey::new(self.name.clone(), self.path.clone())
    }

    #[inline]
    pub fn source_dir(&self) -> &Path {
        self.source_dir.as_ref()
    }

    #[inline]
    pub fn working_dir(&self) -> &Path {
        self.working_dir.as_ref()
    }

    pub fn search_paths(&self) -> parking_lot::MutexGuard<'_, BTreeSet<PathBuf>> {
        self.search_paths.lock()
    }

    pub fn filter_by_path<P: Into<PathBuf>>(&self, path: P) {
        self.search_paths.lock().insert(path.into());
    }

    /// Load test suite configuration from `input`
    pub fn parse<P: AsRef<Path>>(path: P, config: &Config) -> DiagResult<Arc<Self>> {
        let path = path.as_ref();
        let path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            path.canonicalize().into_diagnostic()?
        };
        let source = Input::from(path.as_path())
            .into_source(false, config.source_manager())
            .into_diagnostic()?;
        let toml = source.as_str();
        let mut suite = toml::from_str::<Self>(toml).map_err(|error| {
            let span = error.span();
            Report::new(TestSuiteError::Syntax {
                span: span.map(|span| SourceSpan::from_range_unchecked(source.id(), span)),
                error,
            })
            .with_source_code(source.clone())
        })?;

        if suite.name().is_empty() {
            return Err(Report::new(TestSuiteError::EmptyName {
                span: SourceSpan::from_range_unchecked(source.id(), suite.span()),
            })
            .with_source_code(source.clone()));
        }
        suite.path = Some(path.clone().into());

        let parent_dir = path.parent().unwrap();

        log::trace!(target: "lit:suite", "parent directory for suite is {}", parent_dir.display());

        if suite.source_dir() == Path::new("") {
            *suite.source_dir.get_mut() = Cow::Owned(parent_dir.to_path_buf());
        } else if suite.source_dir().is_relative() {
            let span = suite.source_dir.span();
            let source_dir = parent_dir
                .join(suite.source_dir.get_ref())
                .canonicalize()
                .map_err(|err| {
                    Report::new(TestSuiteError::CouldNotCanonicalizePath {
                        span: SourceSpan::from_range_unchecked(source.id(), span),
                        path: suite.source_dir.get_ref().to_path_buf(),
                        cause: err,
                    })
                    .with_source_code(source.clone())
                })?;
            *suite.source_dir.get_mut() = Cow::Owned(source_dir);
        } else {
            let span = suite.source_dir.span();
            let source_dir = suite.source_dir.get_ref().canonicalize().map_err(|err| {
                Report::new(TestSuiteError::CouldNotCanonicalizePath {
                    span: SourceSpan::from_range_unchecked(source.id(), span),
                    path: suite.source_dir.get_ref().to_path_buf(),
                    cause: err,
                })
                .with_source_code(source.clone())
            })?;
            if suite.source_dir() != source_dir.as_path() {
                *suite.source_dir.get_mut() = Cow::Owned(source_dir);
            }
        }

        if !suite.source_dir().is_dir() {
            let span = suite.source_dir.span();
            return Err(Report::new(TestSuiteError::InvalidSourceDir {
                span: SourceSpan::from_range_unchecked(source.id(), span),
                source_dir: suite.source_dir.into_inner().into_owned(),
            })
            .with_source_code(source.clone()));
        }

        if suite.working_dir() == Path::new("") {
            let temp_dir = tempdir::TempDir::new(suite.name())
                .into_diagnostic()
                .wrap_err_with(|| {
                    format!(
                        "failed to create temporary directory for test suite at '{}'",
                        &TestSuiteKey::new(suite.name.clone(), suite.path.clone()),
                    )
                })?;
            *suite.working_dir.get_mut() = Cow::Owned(temp_dir.path().to_path_buf());
            suite.temp_dir = Some(temp_dir);
        } else if suite.working_dir().is_relative() {
            let span = suite.working_dir.span();
            let working_dir = parent_dir.join(suite.working_dir.get_ref());
            log::trace!(target: "lit:suite", "working dir (relative) is {}", working_dir.display());
            let working_dir = working_dir.canonicalize().map_err(|err| {
                Report::new(TestSuiteError::CouldNotCanonicalizePath {
                    span: SourceSpan::from_range_unchecked(source.id(), span),
                    path: suite.working_dir.get_ref().to_path_buf(),
                    cause: err,
                })
                .with_source_code(source.clone())
            })?;
            *suite.working_dir.get_mut() = Cow::Owned(working_dir);
        } else {
            let span = suite.working_dir.span();
            let working_dir = suite.working_dir.get_ref().canonicalize().map_err(|err| {
                Report::new(TestSuiteError::CouldNotCanonicalizePath {
                    span: SourceSpan::from_range_unchecked(source.id(), span),
                    path: suite.working_dir.get_ref().to_path_buf(),
                    cause: err,
                })
            })?;
            if suite.working_dir() != working_dir.as_path() {
                *suite.working_dir.get_mut() = Cow::Owned(working_dir);
            }
        }

        if !suite.working_dir().is_dir() {
            let span = suite.working_dir.span();
            let working_dir = suite.working_dir.into_inner();
            return Err(Report::new(TestSuiteError::InvalidWorkingDir {
                span: SourceSpan::from_range_unchecked(source.id(), span),
                working_dir: working_dir.into_owned(),
            })
            .with_source_code(source));
        }

        // Ensure default test features/substitutions are present
        let mut default_config = TestConfig::default();
        default_config.set_default_features(config);
        default_config.set_default_substitutions(config, &suite, path.as_path());
        let suite_config = Arc::make_mut(&mut suite.config);
        suite_config.inherit(&default_config);

        Ok(Arc::new(suite))
    }
}

fn empty_spanned_path() -> toml::Spanned<StaticCow<Path>> {
    toml::Spanned::new(0..0, litcheck::fs::empty_path())
}
