use std::{path::Path, sync::Arc};

use litcheck::{
    diagnostics::{DiagResult, IntoDiagnostic},
    fs,
};

use crate::{
    test::{Test, TestConfig, TestList, TestSuite},
    Config,
};

use super::{TestRegistry, TestSuiteRegistry};

/// The [DefaultTestRegistry] is used by default to locate tests for the builtin test formats.
///
/// It has the following behavior:
///
/// * Tests are discovered by traversing the contents of the test suite's `source_dir`, using the
///   patterns in the [TestSuite] `patterns` field to determine if a file is a valid test
/// * Tests are then selected/rejected based on the `filter`/`filter_out` fields of [Config], if
///   unset, the default is to select all tests.
/// * Hidden files are ignored
pub struct DefaultTestRegistry;

impl TestRegistry for DefaultTestRegistry {
    fn all(
        &self,
        suite: Arc<TestSuite>,
        config: &Config,
        suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList> {
        log::debug!(target: "lit:test:registry", "finding all tests for suite '{}'", suite.name());
        let source_dir = suite.source_dir();
        get_file_based_tests(
            source_dir,
            suite.clone(),
            config,
            suite.config.clone(),
            suites,
        )
    }

    fn find(
        &self,
        path_in_suite: &Path,
        suite: Arc<TestSuite>,
        config: &Config,
        test_config: Arc<TestConfig>,
        suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList> {
        log::debug!(target: "lit:test:registry", "finding tests for suite '{}' in {}", suite.name(), path_in_suite.display());
        let source_dir = suite.source_dir();
        let absolute_path = source_dir.join(path_in_suite);
        if absolute_path.is_dir() {
            return get_file_based_tests(&absolute_path, suite, config, test_config, suites);
        }
        log::debug!(
            target: "lit:test:registry",
            "checking if {} is a valid test file using default registry",
            absolute_path.display()
        );

        let mut tests = TestList::default();
        if is_file_based_test(&absolute_path, &suite, &test_config) {
            log::debug!(
                target: "lit:test:registry",
                "{} is a valid test, adding it to registry",
                absolute_path.display()
            );
            tests.push_back(Arc::new(Test::new(
                path_in_suite.to_path_buf(),
                suite,
                test_config,
            )));
        }
        Ok(tests)
    }
}

/// Get all of the file-based tests for `suite` and `config`, starting from `search_path`
fn get_file_based_tests(
    search_path: &Path,
    suite: Arc<TestSuite>,
    config: &Config,
    test_config: Arc<TestConfig>,
    suites: &mut dyn TestSuiteRegistry,
) -> DiagResult<TestList> {
    debug_assert!(search_path.is_absolute());

    log::debug!(
        target: "lit:test:registry",
        "searching for tests in {} using default registry",
        search_path.display()
    );

    let mut nested = Vec::new();
    let searcher = fs::Searcher::<_>::new(search_path, true, |entry| {
        if entry.file_type().is_dir() {
            let path = entry.path();
            if path == search_path || path == suite.source_dir() {
                return true;
            }
            // Do not recurse into directories which contain a `lit.suite.toml`
            if entry
                .path()
                .join("lit.suite.toml")
                .try_exists()
                .is_ok_and(|exists| exists)
            {
                nested.push(entry.path().to_path_buf());
                return false;
            }
            true
        } else {
            log::trace!(target: "lit:test:registry", "checking if file is valid test: {}", entry.path().display());
            is_file_based_test(entry.path(), &suite, &test_config)
        }
    });

    // Collect tests from search results
    let mut tests = TestList::default();
    for result in searcher.into_walker() {
        let test_entry = result.into_diagnostic()?;
        if test_entry.file_type().is_dir() {
            continue;
        }
        let test_path = test_entry.into_path();
        log::trace!(target: "lit:test:registry", "found test file: {}", test_path.display());
        let relative_path = test_path
            .strip_prefix(search_path)
            .expect("expected path relative to search_path");
        tests.push_back(Arc::new(Test::new(
            relative_path.to_path_buf(),
            suite.clone(),
            test_config.clone(),
        )));
    }

    // Visit any nested suites that were found
    for nested in nested {
        if nested == search_path || nested == suite.source_dir() {
            log::trace!(target: "lit:test:registry", "ignoring nested path {}", nested.display());
            continue;
        }
        suites.load_from_path(&nested, config)?;
    }

    Ok(tests)
}

/// Returns true if `path` matches the criteria for file-based tests belonging to `suite`
fn is_file_based_test(path: &Path, suite: &TestSuite, config: &TestConfig) -> bool {
    let is_hidden = path
        .file_name()
        .map(|name| name.as_encoded_bytes().starts_with(b"."))
        .unwrap_or(false);

    if is_hidden || !path.is_file() {
        return false;
    }

    let path_in_suite = path.strip_prefix(suite.source_dir()).unwrap_or(path);
    let is_match = config
        .patterns
        .iter()
        .any(|p| p.matches_path(path_in_suite));

    if is_match && !config.exclude.is_empty() {
        // If we have exclusion patterns configured, reject paths that match any of them
        !config.exclude.iter().any(|p| p.matches_path(path_in_suite))
    } else {
        is_match
    }
}
