use std::{collections::BTreeMap, path::Path, sync::Arc};

use litcheck::{
    diagnostics::{DiagResult, IntoDiagnostic, Report, WrapErr},
    fs::{self, PathPrefixTree},
};

use crate::{
    format::TestFormat,
    suite::{TestSuite, TestSuiteKey, TestSuiteSet},
    test::{Test, TestConfig, TestList},
    Config, LitError,
};

use super::TestSuiteRegistry;

#[derive(Default)]
pub struct DefaultTestSuiteRegistry {
    /// The set of suites under management
    suites: TestSuiteSet,
    /// The set of tests for each suite under management
    tests_by_suite: BTreeMap<TestSuiteKey, TestList>,
    /// A path prefix tree for all directories visited during config discovery
    ///
    /// Using this, we can easily find the nearest test suite for a given path
    config_cache: PathPrefixTree<TestSuiteKey>,
    local_config_cache: PathPrefixTree<Arc<TestConfig>>,
}
impl TestSuiteRegistry for DefaultTestSuiteRegistry {
    fn load(&mut self, config: &Config) -> DiagResult<()> {
        log::debug!(target: "lit:suite:registry", "loading test suites for {config:#?}");

        let cwd = std::env::current_dir().expect("unable to access current working directory");

        self.clear();

        // Load all test suites corresponding to the provided input paths,
        // and add the input path as a filter for the tests in that suite,
        // if applicable.
        for input in config.tests.iter().map(Path::new) {
            let input_path = input
                .canonicalize()
                .into_diagnostic()
                .wrap_err("test input does not exist")?;
            if !input_path.starts_with(&cwd) {
                return Err(Report::new(LitError::TestPath(input_path)));
            }
            let search_root = if input_path.starts_with(&cwd) {
                &cwd
            } else {
                &input_path
            };

            if let Some(suite) = self.find_nearest_suite(&input_path, search_root, config)? {
                log::debug!("resolved input {} to {}", input.display(), &suite.id());
                let suite_source_dir = suite.source_dir();
                let filter_path = input_path.strip_prefix(suite_source_dir).expect(
                    "test suite source directory should have been an ancestor of the input path",
                );
                if filter_path != Path::new("") {
                    log::debug!(
                        "filtering suite '{}' by path: '{}'",
                        &suite.id(),
                        filter_path.display()
                    );
                    suite.filter_by_path(filter_path);
                }
            } else {
                log::warn!("unable to locate test suite for {}", input.display());
            }
        }

        if self.suites.is_empty() {
            return Ok(());
        }

        self.load_tests(config)?;

        Ok(())
    }

    fn is_empty(&self) -> bool {
        self.tests_by_suite.is_empty()
    }

    fn num_suites(&self) -> usize {
        self.tests_by_suite.len()
    }

    fn num_tests(&self) -> usize {
        self.tests_by_suite.values().map(|suite| suite.len()).sum()
    }

    fn size_of_suite(&self, id: &TestSuiteKey) -> usize {
        self.tests_by_suite[id].len()
    }

    #[inline(always)]
    fn get(&self, id: &TestSuiteKey) -> Arc<TestSuite> {
        self.suites.get(id).unwrap()
    }

    /// Get a [TestSuite] given the path at which it's configuration resides
    fn get_by_path(&self, path: &Path) -> Option<Arc<TestSuite>> {
        self.config_cache
            .get(path)
            .and_then(|key| self.suites.get(key))
    }

    fn tests(&self) -> impl Iterator<Item = Arc<Test>> + '_ {
        self.tests_by_suite.values().flat_map(|suite| suite.iter())
    }

    fn suites(&self) -> impl Iterator<Item = Arc<TestSuite>> + '_ {
        self.suites.iter()
    }
}
impl IntoIterator for DefaultTestSuiteRegistry {
    type Item = Arc<Test>;
    type IntoIter = <TestList as IntoIterator>::IntoIter;

    fn into_iter(mut self) -> Self::IntoIter {
        let mut tests = self
            .tests_by_suite
            .pop_first()
            .map(|(_, tests)| tests)
            .unwrap_or_default();
        while let Some((_, ts)) = self.tests_by_suite.pop_first() {
            tests.append(ts);
        }
        tests.into_iter()
    }
}
impl rayon::iter::IntoParallelIterator for DefaultTestSuiteRegistry {
    type Item = Arc<Test>;
    type Iter = <TestList as rayon::iter::IntoParallelIterator>::Iter;

    #[inline]
    fn into_par_iter(self) -> Self::Iter {
        Self::into_iter(self)
    }
}
impl DefaultTestSuiteRegistry {
    fn clear(&mut self) {
        self.local_config_cache.clear();
        self.config_cache.clear();
        self.tests_by_suite.clear();
        self.suites.clear();
    }

    /// Search for the nearest `lit.suite.toml` to which `path` belongs.
    ///
    /// If `path` is a file path, the directory containing that file is the first place
    /// searched, followed by each ancestor in the directory tree until a config is found.
    ///
    /// If `path` is a directory, the directory itself is searched, followed by each
    /// ancestor in the directory tree until a config is found.
    pub fn find_nearest_suite<P: AsRef<Path>>(
        &mut self,
        path: P,
        cwd: &Path,
        config: &Config,
    ) -> DiagResult<Option<Arc<TestSuite>>> {
        let path = path.as_ref();
        log::debug!(target: "lit:suite:registry", "finding nearest suite to {}", path.display());
        // If the path is a directory, we need to check the cache for an exact
        // match, and failing that, check the directory itself for a lit.suite.toml.
        // If successful, we're done; otherwise, we fallback to the same search
        // used for file paths
        if path.is_dir() {
            if let Some(cache_key) = self.config_cache.get(path) {
                log::debug!(target: "lit:suite:registry", "found in cache: '{}'", cache_key.name());
                return Ok(Some(self.get(cache_key)));
            }

            let mut found =
                fs::search_directory(path, false, |entry| entry.file_name() == "lit.suite.toml");

            if let Some(entry) = found.next() {
                let entry = entry
                    .into_diagnostic()
                    .wrap_err("found test suite config, but it could not be read")?;
                log::debug!(target: "lit:suite:registry", "found lit.suite.toml at {}", entry.path().display());
                return self.load_without_cache(entry.path(), config).map(Some);
            }
        }

        self.find_nearest_suite_containing(path, cwd, config)
    }

    fn find_nearest_suite_containing(
        &mut self,
        path: &Path,
        cwd: &Path,
        config: &Config,
    ) -> DiagResult<Option<Arc<TestSuite>>> {
        log::debug!(target: "lit:suite:registry", "finding nearest suite containing {}", path.display());
        // If `dir` is not explicitly represented in `config_paths`, then it hasn't
        // been visited to check for a `lit.suite.toml` file yet, and we can't rely
        // on the nearest ancestor to be correct. Otherwise, the nearest ancestor
        // refers to the nearest path in `config_paths`
        if let Some(cache_key) = self.config_cache.nearest_ancestor(path) {
            log::debug!(target: "lit:suite:registry", "found nearest ancestor suite in cache: '{}'", cache_key.data.name());
            return Ok(Some(self.get(cache_key.into_value())));
        }

        let dir = if path.is_dir() {
            path
        } else {
            path.parent().expect("expected canonical file path")
        };

        // Search the ancestors of `path` until we find a `lit.suite.toml` whose source
        // directory contains `path`.
        log::debug!(target: "lit:suite:registry", "searching ancestors of {}", dir.display());
        for ancestor in dir.ancestors() {
            let is_cwd = ancestor == cwd;
            let mut found = fs::search_directory(ancestor, false, |entry| {
                entry.file_name() == "lit.suite.toml"
            });
            if let Some(entry) = found.next() {
                let entry = entry.into_diagnostic()?;
                log::debug!(target: "lit:suite:registry", "found possible ancestor at {}", entry.path().display());
                let suite = self.load_without_cache(entry.path(), config)?;
                // If the test suite source directory contains `dir`, we've found a match;
                // otherwise, we must continue searching upwards.
                if dir.starts_with(suite.source_dir()) {
                    log::debug!(target: "lit:suite:registry", "ancestor is a match!");
                    return Ok(Some(suite));
                }
            }

            // If we've reached the current working directory, ascend no further
            if is_cwd {
                log::debug!(target: "lit:suite:registry", "terminating search as we've reached the current working directory");
                break;
            }
        }
        log::debug!(target: "lit:suite:registry", "no matching ancestor found");

        Ok(None)
    }

    fn load_without_cache(&mut self, path: &Path, config: &Config) -> DiagResult<Arc<TestSuite>> {
        let suite = TestSuite::parse(path, config)?;
        self.suites.insert(suite.clone());
        self.tests_by_suite.insert(suite.id(), TestList::default());
        // Make lookups for the config path easy
        self.config_cache.insert(path, suite.id());
        // Ensure we can look up test suites from test sources
        self.config_cache.insert(suite.source_dir(), suite.id());

        Ok(suite)
    }

    /// Visits each test suite under management, and runs test discovery for that suite
    /// using the provided configuration.
    ///
    /// This is safe to call multiple times, each subsequent load will clear all previously
    /// loaded tests, and run discovery on an empty test set for each suite.
    fn load_tests(&mut self, config: &Config) -> DiagResult<()> {
        log::debug!(target: "lit:suite:registry", "loading tests into registry..");

        for suite in self.suites.iter() {
            log::debug!(target: "lit:suite:registry", "loading tests for '{}'", &suite.id());
            // Now that all specified test suites are loaded and have filters applied,
            // perform test discovery for each suite taking those filters into account.
            let tests = self
                .tests_by_suite
                .get_mut(&suite.id())
                .expect("invalid test suite key");
            tests.clear();

            // If we have no specific selections, select everything
            let search_paths = {
                let mut lock = suite.search_paths();
                core::mem::take(&mut *lock)
            };
            if search_paths.is_empty() {
                log::debug!(
                    target: "lit:suite:registry",
                    "no search paths given, searching entire suite source directory for tests"
                );
                let found = suite.config.format.registry().all(suite.clone())?;
                log::debug!(target: "lit:suite:registry", "found {} tests", found.len());
                tests.append(found);
            } else {
                log::debug!(
                    target: "lit:suite:registry",
                    "{} search paths were given, searching just those paths for tests",
                    search_paths.len()
                );
                for path in search_paths.iter() {
                    log::debug!(target: "lit:suite:registry", "searching {} for tests..", path.display());
                    // Look for local configuration that applies to these tests,
                    // using the suite config if no local config is present
                    let local_config =
                        get_local_config(&suite, path, config, &mut self.local_config_cache)?;
                    let found = local_config.format.registry().find(
                        path,
                        suite.clone(),
                        local_config.clone(),
                    )?;
                    log::debug!(target: "lit:suite:registry", "found {} tests", found.len());
                    tests.append(found);
                }
            }

            *suite.search_paths() = search_paths;
        }

        log::debug!(target: "lit:suite:registry", "loading complete!");

        Ok(())
    }
}

fn get_local_config(
    suite: &TestSuite,
    path_in_suite: &Path,
    lit: &Config,
    local_config_cache: &mut PathPrefixTree<Arc<TestConfig>>,
) -> DiagResult<Arc<TestConfig>> {
    let source_dir = suite.source_dir();
    let path = source_dir.join(path_in_suite);
    let path = if path.is_dir() {
        path
    } else {
        path.parent().unwrap().to_path_buf()
    };

    // If we already have a cache entry for the given path, we're done
    if let Some(config) = local_config_cache.get(&path) {
        return Ok(config.clone());
    }

    // Otherwise, we are going to reify a config for each level in the directory tree
    // between the nearest ancestor for which we have already computed a configuration -
    // using the suite config and source directory as a fallback - and `path_in_suite`.
    // We start at that point and descend to `path_in_suite`, computing the effective
    // configuration at each step if we encounter a `lit.local.toml` at that level.
    let (mut config, search_root_in_suite) =
        if let Some(entry) = local_config_cache.nearest_ancestor(&path) {
            (
                entry.data.clone(),
                entry.path.strip_prefix(source_dir).unwrap().to_path_buf(),
            )
        } else {
            (suite.config.clone(), path_in_suite.to_path_buf())
        };

    let mut path = source_dir.to_path_buf();
    for component in search_root_in_suite.components() {
        path.push(AsRef::<Path>::as_ref(&component));

        let mut found =
            fs::search_directory(&path, false, |entry| entry.file_name() == "lit.local.toml");
        if let Some(entry) = found.next() {
            let entry = entry.into_diagnostic()?;
            let mut cfg = TestConfig::parse(entry.path())?;
            cfg.inherit(&config);
            cfg.set_default_substitutions(lit, suite, &path);
            cfg.set_default_features(lit);
            let cfg = Arc::<TestConfig>::from(cfg);
            local_config_cache.insert(&path, cfg.clone());
            config = cfg;
        }
    }

    // Return the constructed configuration, or fallback to cloning the suite config
    local_config_cache.insert(&path, config.clone());
    Ok(config)
}
