use std::{path::Path, time::Duration};

use litcheck::diagnostics::DiagResult;
use serde::Deserialize;

use crate::{
    config::BooleanExpr,
    format::TestFormat,
    test::{DefaultTestRegistry, Test, TestRegistry, TestResult, TestStatus},
    Config,
};

#[derive(Default, Clone, Debug, Deserialize)]
pub struct ExecutableTest {
    /// When non-empty, this specifies a set of boolean expressions which
    /// are evaluated like the UNSUPPORTED directive in `ShTest`. If any
    /// expression evaluates to true, the test is considered unsupported.
    #[serde(default)]
    unsupported: Vec<BooleanExpr>,
}
impl TestFormat for ExecutableTest {
    #[inline(always)]
    fn name(&self) -> &'static str {
        "executable"
    }

    #[inline(always)]
    fn registry(&self) -> &dyn TestRegistry {
        &DefaultTestRegistry
    }

    fn execute(&self, test: &Test, config: &Config) -> DiagResult<TestResult> {
        const ARGV: &[&Path] = &[];

        if let Some(unsupported_features) = test.config.unsupported_features(&self.unsupported) {
            return Ok(TestResult::new(TestStatus::Unsupported)
                .with_stderr(unsupported_features.into_bytes()));
        }

        if config.no_execute {
            return Ok(TestResult::new(TestStatus::Pass));
        }

        let mut command = crate::utils::ShellCommand::new(test.source_path());
        command
            .args(ARGV)
            .current_dir(test.suite.working_dir())
            .envs(test.config.env.iter().map(|(k, v)| (&**k, &**v)))
            .paths(config.search_paths.as_slice())?;

        Ok(command.wait_with_timeout(
            config
                .timeout
                .map(Duration::from_secs)
                .unwrap_or(Duration::ZERO),
        ))
    }
}
