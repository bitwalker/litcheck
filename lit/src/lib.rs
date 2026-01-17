//#![feature(iter_intersperse)]

pub mod config;
pub mod format;
pub mod formats;
pub mod suite;
pub mod test;
pub(crate) mod utils;

pub use self::config::Config;
pub use self::format::TestFormat;
pub use self::suite::{
    DefaultTestSuiteRegistry, TestSuite, TestSuiteError, TestSuiteRegistry, TestSuiteRegistryExt,
};
pub use self::test::{Test, TestConfig, TestConfigError, TestRegistry, TestResult, TestStatus};

use litcheck::diagnostics::Diagnostic;

pub type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, core::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum LitError {
    #[error("did not discover any tests for provided path(s)")]
    #[diagnostic()]
    NoTests,
    #[error("no tests selected (of {available} available)")]
    #[diagnostic()]
    NoTestsSelected { available: usize },
    #[error("one or more tests failed")]
    #[diagnostic()]
    TestsFailed,
    #[error(transparent)]
    #[diagnostic(transparent)]
    TestSuite(#[from] TestSuiteError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    TestConfig(#[from] TestConfigError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    TestScript(#[from] formats::InvalidTestScriptError),
    #[error("invalid test path '{}': paths must be located under the current working directory", .0.display())]
    TestPath(std::path::PathBuf),
}
