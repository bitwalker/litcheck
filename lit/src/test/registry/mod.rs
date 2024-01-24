mod default;

pub use self::default::DefaultTestRegistry;

use std::{path::Path, sync::Arc};

use litcheck::diagnostics::DiagResult;

use crate::{
    suite::TestSuite,
    test::{Test, TestConfig, TestList},
};

pub trait TestRegistry {
    /// Get the all tests belonging to `suite`, based on its provided configuration
    fn all(&self, suite: Arc<TestSuite>) -> DiagResult<TestList>;

    /// Search for tests belonging to `suite` which are located under `path_in_suite`,
    /// a file/directory path which must be located within `suite`.
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
        config: Arc<TestConfig>,
    ) -> DiagResult<TestList> {
        let mut tests = TestList::default();

        tests.push_back(Arc::new(Test::new(
            path_in_suite.to_path_buf(),
            suite,
            config,
        )));

        Ok(tests)
    }
}
impl<T: TestRegistry> TestRegistry for &T {
    #[inline]
    fn all(&self, suite: Arc<TestSuite>) -> DiagResult<TestList> {
        (**self).all(suite)
    }
    #[inline]
    fn find(
        &self,
        path_in_suite: &Path,
        suite: Arc<TestSuite>,
        config: Arc<TestConfig>,
    ) -> DiagResult<TestList> {
        (**self).find(path_in_suite, suite, config)
    }
}
