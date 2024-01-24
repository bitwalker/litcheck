mod config;
mod list;
mod registry;
mod result;
mod status;

pub use self::config::{TestConfig, TestConfigError};
pub use self::list::TestList;
pub use self::registry::{DefaultTestRegistry, TestRegistry};
pub use self::result::TestResult;
pub use self::status::TestStatus;

use std::{
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use crate::suite::TestSuite;

/// [Test] represents information about a single test instance
pub struct Test {
    link: intrusive_collections::LinkedListAtomicLink,
    /// The test suite which this test belongs to
    pub suite: Arc<TestSuite>,
    /// The test configuration which applies to this test
    pub config: Arc<TestConfig>,
    /// The full name of this test
    pub name: Arc<str>,
    /// The path relative to `suite_path`
    pub path: PathBuf,
    /// The absolute path to this test
    pub absolute_path: PathBuf,
    /// If true, ignore `xfails`
    pub xfail_not: bool,
    /// The previous test failure state, if applicable.
    pub previous_failure: Option<TestResult>,
    /// The previous test elapsed time, if applicable.
    pub previous_elapsed: Option<Duration>,
}
impl Test {
    pub fn new(path_in_suite: PathBuf, suite: Arc<TestSuite>, config: Arc<TestConfig>) -> Self {
        let absolute_path = suite.source_dir.get_ref().join(&path_in_suite);
        let name =
            Arc::from(format!("{}::{}", suite.name(), path_in_suite.display()).into_boxed_str());
        Self {
            link: Default::default(),
            suite,
            config,
            name,
            path: path_in_suite,
            absolute_path,
            xfail_not: false,
            previous_failure: None,
            previous_elapsed: None,
        }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn source_path(&self) -> &Path {
        self.absolute_path.as_path()
    }
}
