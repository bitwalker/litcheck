use std::{collections::BTreeMap, path::Path};

use litcheck::{
    diagnostics::{DiagResult, Diagnostic, IntoDiagnostic, Report, SourceFile, SourceSpan},
    fs::PatternSet,
    Input, StaticCow,
};
use serde::Deserialize;

use crate::{
    config::{BooleanExpr, FeatureSet, SubstitutionSet},
    format::SelectedTestFormat,
    Config,
};

#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum TestConfigError {
    #[error("invalid syntax in configuration file")]
    #[diagnostic()]
    Syntax {
        #[label("{error}")]
        span: SourceSpan,
        #[source]
        error: toml::de::Error,
    },
}

/// This is the type represents a local lit test configuration file
#[derive(Default, Clone, Deserialize)]
pub struct TestConfig {
    /// The test format which will be used to discover and run tests in the suite
    ///
    /// Defaults to the `ShTest` format, see [crate::formats::ShTest] for details.
    #[serde(default)]
    pub format: SelectedTestFormat,
    /// For test formats which scan directories for tests, this is a list of glob
    /// patterns used to find test files, e.g. `*.rs` would search for any test files
    /// ending in `.rs` in any of the search paths.
    ///
    /// See [glob::Pattern] docs for the full syntax available for glob patterns.
    ///
    /// By default all text files found in the search path for tests are considered
    /// valid tests, unless they are prefixed with `.`, e.g. `.gitignore`.
    #[serde(default)]
    pub patterns: PatternSet,
    /// Like `patterns`, but the given globs are used to reject potential test files matched by
    /// `patterns` which should not be treated as tests.
    ///
    /// You should prefer to exclude subdirectories of a suite by using explicit search paths, and
    /// excluding the directories that you don't want to search (so as to avoid traversing them in
    /// the first place) - but in those cases where it is more convenient to use exclusion patterns,
    /// this option is here to help.
    #[serde(default)]
    pub exclude: PatternSet,
    /// Environment variables to set when executing tests
    #[serde(default)]
    pub env: BTreeMap<StaticCow<str>, StaticCow<str>>,
    /// The set of substitutions which can be used in a test script.
    ///
    /// The substitutions will be replaced prior to running the test.
    #[serde(default)]
    pub substitutions: SubstitutionSet,
    /// The set of feature strings available for these tests
    #[serde(default)]
    pub available_features: FeatureSet,
}
impl TestConfig {
    /// Parse local test suite configuration from `input`, inheriting from `parent` where applicable
    pub fn parse<P: AsRef<Path>>(path: P) -> DiagResult<Box<Self>> {
        let path = path.as_ref();
        let path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            path.canonicalize().into_diagnostic()?
        };
        let source = Input::from(path).into_source(false).into_diagnostic()?;
        toml::from_str::<Self>(source.source())
            .map(Box::new)
            .map_err(|error| {
                let span = error.span().unwrap_or(0..0);
                Report::new(TestConfigError::Syntax {
                    span: SourceSpan::from(span),
                    error,
                })
                .with_source_code(source)
            })
    }

    /// Inherits values from `parent` that are empty/default in `self`.
    pub fn inherit(&mut self, parent: &Self) {
        if matches!(self.format, SelectedTestFormat::Default) {
            self.format = parent.format.clone();
        }

        if self.patterns.is_empty() {
            self.patterns = parent.patterns.clone();
        }

        if self.exclude.is_empty() {
            self.exclude = parent.exclude.clone();
        }

        if !parent.env.is_empty() {
            let env = core::mem::replace(&mut self.env, parent.env.clone());
            self.env.extend(env);
        }

        if !parent.substitutions.is_empty() {
            let subs = core::mem::replace(&mut self.substitutions, parent.substitutions.clone());
            self.substitutions.extend(subs);
        }

        if !parent.available_features.is_empty() {
            let features = core::mem::replace(
                &mut self.available_features,
                parent.available_features.clone(),
            );
            self.available_features.extend(features);
        }
    }

    pub fn set_default_features(&mut self, config: &Config) {
        use target_lexicon::*;

        let host = config.host();
        let target = config.target();

        self.available_features
            .insert(format!("system-{}", &host.operating_system));
        self.available_features.insert(format!("host={}", &host));
        self.available_features
            .insert(format!("target={}", &target));
        if host == target {
            self.available_features.insert("native");
        }
        match target.architecture {
            Architecture::X86_64 | Architecture::X86_64h => {
                self.available_features.insert("target-x86_64");
                match target.operating_system {
                    OperatingSystem::Darwin(_) => {
                        self.available_features.insert("x86_64-apple");
                    }
                    OperatingSystem::Linux => {
                        self.available_features.insert("x86_64-linux");
                    }
                    _ => (),
                }
            }
            Architecture::X86_32(_) => {
                self.available_features.insert("target-x86");
            }
            Architecture::Aarch64(_) => {
                self.available_features.insert("target-aarch64");
            }
            Architecture::Arm(_) => {
                self.available_features.insert("target-arm");
            }
            arch => {
                self.available_features.insert(format!("target-{}", arch));
            }
        }
        match target.operating_system {
            OperatingSystem::Darwin(_) | OperatingSystem::MacOSX { .. } => {
                self.available_features.insert("system-linker-mach-o");
            }
            _ => (),
        }
    }

    pub fn set_default_substitutions(
        &mut self,
        _config: &Config,
        suite: &super::TestSuite,
        source_dir: &Path,
    ) {
        // This binary is a multi-call executable containing both lit and filecheck,
        // so get the path that we were invoked with (or that the OS chose to give us),
        // and depending on how it was invoked,
        let mut exe =
            std::env::current_exe().expect("unable to detect lit/filecheck executable path");
        let (filecheck, not) = if exe.ends_with("litcheck") {
            // We are running lit, so a filename of 'litcheck'
            // means that lit was invoked explicitly, thus we
            // should use a substitution that invokes filecheck
            // explicitly
            let filecheck = StaticCow::Owned(format!("{} filecheck", exe.display()));
            let not = StaticCow::Owned(format!("{} not", exe.display()));
            (filecheck, not)
        } else if exe.ends_with("lit") {
            // We must have been invoked as a symlink, in
            // which case the filecheck symlink is in the
            // same directory, we just need to update the
            // executable name
            exe.set_file_name("filecheck");
            let filecheck = StaticCow::Owned(exe.to_string_lossy().into_owned());
            exe.set_file_name("not");
            let not = StaticCow::Owned(exe.to_string_lossy().into_owned());
            (filecheck, not)
        } else {
            // We're probably running as a test executable right now,
            // so just use 'filecheck' as the substitution
            (StaticCow::Borrowed("filecheck"), StaticCow::Borrowed("not"))
        };
        self.substitutions
            .insert("\\b{start}[Ff]ile[Cc]heck\\b{end}", filecheck);
        self.substitutions.insert("\\b{start}not\\b{end}", not);

        self.substitutions
            .insert(r"%\{pathsep\}", if cfg!(windows) { ";" } else { ":" });

        let test_root = source_dir
            .components()
            .next()
            .unwrap()
            .as_os_str()
            .to_string_lossy()
            .into_owned();
        self.substitutions.insert(r"%\{fs-src-root\}", test_root);
        let temp_root = suite
            .working_dir()
            .components()
            .next()
            .unwrap()
            .as_os_str()
            .to_string_lossy()
            .into_owned();
        self.substitutions.insert(r"%\{fs-tmp-root\}", temp_root);
        self.substitutions
            .insert(r"%\{fs-sep\}", std::path::MAIN_SEPARATOR_STR);
    }

    #[inline]
    pub fn missing_features<F: AsRef<BooleanExpr>>(&self, required: &[F]) -> Option<String> {
        self.available_features.missing_features(required)
    }

    #[inline]
    pub fn unsupported_features<F: AsRef<BooleanExpr>>(&self, unsupported: &[F]) -> Option<String> {
        self.available_features.unsupported_features(unsupported)
    }
}
