mod default;

pub use self::default::DefaultTestRegistry;

use std::{path::Path, sync::Arc};

use litcheck::diagnostics::DiagResult;

use crate::{
    suite::{TestSuite, TestSuiteRegistry},
    test::{Test, TestConfig, TestList},
    Config,
};

pub trait TestRegistry {
    /// Discover all tests belonging to `suite`, based on its provided configuration.
    ///
    /// If nested suites are discovered in the process, the provided [TestSuiteRegistry] can be used
    /// to register and perform test discovery for them.
    fn all(
        &self,
        suite: Arc<TestSuite>,
        config: &Config,
        suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList>;

    /// Search for tests belonging to `suite` which are located under `path_in_suite`,
    /// a file/directory path which must be located within `suite`.
    ///
    /// If nested suites are discovered in the process, the provided [TestSuiteRegistry] can be used
    /// to register and perform test discovery for them.
    ///
    /// The search takes into account the current configuration to ensure that only those
    /// tests that should be discoverable are found.
    ///
    /// The default implementation of this function generates a single test from the given
    /// path, under the assumption that it is already valid. It is up to the test format to
    /// either override the default implementation, or raise an error if the test is invalid
    /// under these assumptions.
    fn find(
        &self,
        path_in_suite: &Path,
        suite: Arc<TestSuite>,
        _config: &Config,
        test_config: Arc<TestConfig>,
        _suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList> {
        let mut tests = TestList::default();

        tests.push_back(Arc::new(Test::new(
            path_in_suite.to_path_buf(),
            suite,
            test_config,
        )));

        Ok(tests)
    }
}
impl<T: TestRegistry> TestRegistry for &T {
    #[inline]
    fn all(
        &self,
        suite: Arc<TestSuite>,
        config: &Config,
        suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList> {
        (**self).all(suite, config, suites)
    }
    #[inline]
    fn find(
        &self,
        path_in_suite: &Path,
        suite: Arc<TestSuite>,
        config: &Config,
        test_config: Arc<TestConfig>,
        suites: &mut dyn TestSuiteRegistry,
    ) -> DiagResult<TestList> {
        (**self).find(path_in_suite, suite, config, test_config, suites)
    }
}
