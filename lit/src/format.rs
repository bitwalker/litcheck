use std::fmt;

use litcheck::diagnostics::DiagResult;
use serde::Deserialize;

use crate::{
    test::{DefaultTestRegistry, Test, TestRegistry, TestResult},
    Config,
};

pub trait TestFormat: fmt::Debug {
    /// Returns the proper name of this test format
    fn name(&self) -> &'static str;

    /// Get the test registry for this format
    fn registry(&self) -> &dyn TestRegistry;

    /// Executes `test` using this test format, and the provided `config`.
    fn execute(&self, test: &Test, config: &Config) -> DiagResult<TestResult>;
}

#[derive(Default, Clone, Deserialize)]
pub enum SelectedTestFormat {
    #[default]
    Default,
    /// Parses each test file for a `RUN:` directive, which is expected to contain
    /// a shell command to run, the exit status of which will determine the result
    /// of that particular test.
    #[serde(rename = "shtest", alias = "sh")]
    ShTest(crate::formats::ShTest),
    /// Runs a test file as an executable with no inputs, and expects the test to
    /// exit with a zero status if successful, or non-zero if failed.
    #[serde(rename = "executable", alias = "exe")]
    Executable(crate::formats::ExecutableTest),
}
impl fmt::Debug for SelectedTestFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Default => f.write_str("Default"),
            Self::ShTest(ref format) => write!(f, "{format:#?}"),
            Self::Executable(ref format) => write!(f, "{format:#?}"),
        }
    }
}
impl TestFormat for SelectedTestFormat {
    fn name(&self) -> &'static str {
        match self {
            Self::Default => "shtest",
            Self::ShTest(ref format) => format.name(),
            Self::Executable(ref format) => format.name(),
        }
    }

    fn registry(&self) -> &dyn TestRegistry {
        match self {
            Self::Default => &DefaultTestRegistry,
            Self::ShTest(ref format) => format.registry(),
            Self::Executable(ref format) => format.registry(),
        }
    }

    fn execute(&self, test: &Test, config: &Config) -> DiagResult<TestResult> {
        match self {
            Self::Default => {
                let format = crate::formats::ShTest::default();
                format.execute(test, config)
            }
            Self::ShTest(ref format) => format.execute(test, config),
            Self::Executable(ref format) => format.execute(test, config),
        }
    }
}
