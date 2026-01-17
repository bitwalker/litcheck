mod default;

pub use self::default::DefaultTestSuiteRegistry;

use std::{path::Path, sync::Arc};

use litcheck::diagnostics::DiagResult;

use crate::{
    suite::{TestSuite, TestSuiteKey},
    test::Test,
    Config,
};

pub trait TestSuiteRegistry {
    /// Load all available test suites
    fn load(&mut self, config: &Config) -> DiagResult<()>;

    /// Load test suites recursively from `path`
    fn load_from_path(&mut self, path: &Path, config: &Config) -> DiagResult<()>;

    /// Returns true if there are no suites/tests in the registry
    fn is_empty(&self) -> bool;

    /// Returns the number of test suites in the registry
    fn num_suites(&self) -> usize;

    /// Returns the number of tests in the registry
    fn num_tests(&self) -> usize;

    /// Returns the number of tests in the registry belonging to a specific suite
    fn size_of_suite(&self, id: &TestSuiteKey) -> usize;

    /// Get a [TestSuite] using its [TestSuiteKey]
    fn get(&self, id: &TestSuiteKey) -> Arc<TestSuite>;

    /// Get a [TestSuite] given the path at which it's configuration resides
    fn get_by_path(&self, path: &Path) -> Option<Arc<TestSuite>>;
}

pub trait TestSuiteRegistryExt: TestSuiteRegistry + IntoIterator<Item = Arc<Test>> {
    /// Get an iterator over the tests in the registry
    fn tests(&self) -> impl Iterator<Item = Arc<Test>> + '_;

    /// Get an iterator over the tests in the registry
    fn suites(&self) -> impl Iterator<Item = Arc<TestSuite>> + '_;
}
