mod engine;
mod script;

pub use self::script::{InvalidTestScriptError, TestScript};

use std::path::Path;

use litcheck::{
    diagnostics::{DiagResult, IntoDiagnostic, WrapErr},
    reporting::PrintDiagnostic,
    Input,
};
use serde::Deserialize;

use crate::{
    config::{ScopedSubstitutionSet, SubstitutionSet},
    format::TestFormat,
    test::{DefaultTestRegistry, Test, TestRegistry, TestResult, TestStatus},
    Config,
};

/// ShTest is a format with one file per test.
///
/// This is the primary format for regression tests as described in the LLVM
/// [testing guide](http://llvm.org/docs/TestingGuide.md).
///
/// The ShTest files contain some number of shell-like command pipelines, along
/// with assertions about what should be in the output.
#[derive(Default, Clone, Debug, Deserialize)]
pub struct ShTest {
    /// Users can override the default shell, `sh`, to be used when invoking commands.
    #[serde(default)]
    shell: Option<String>,
    #[serde(default)]
    pipefail: bool,
    #[serde(default)]
    extra_substitutions: SubstitutionSet,
}
impl ShTest {
    #[allow(unused)]
    pub fn new(shell: String) -> Self {
        Self {
            shell: Some(shell),
            ..Default::default()
        }
    }

    #[inline]
    fn shell(&self) -> &str {
        self.shell.as_deref().unwrap_or("sh")
    }
}
impl TestFormat for ShTest {
    #[inline(always)]
    fn name(&self) -> &'static str {
        "shtest"
    }

    #[inline(always)]
    fn registry(&self) -> &dyn TestRegistry {
        &DefaultTestRegistry
    }

    fn execute(&self, test: &Test, config: &Config) -> DiagResult<TestResult> {
        let script_source = Input::from(test.source_path())
            .into_source(false, config.source_manager())
            .into_diagnostic()
            .wrap_err("failed to read test file")?;
        let mut script = match TestScript::parse(script_source.clone()) {
            Ok(script) => script,
            Err(err) => {
                let buf = format!("{}", PrintDiagnostic::new(err));
                return Ok(TestResult::new(TestStatus::Unresolved).with_stderr(buf.into_bytes()));
            }
        };

        // Enforce requires
        if let Some(missing_features) = test.config.missing_features(&script.requires) {
            return Ok(
                TestResult::new(TestStatus::Unsupported).with_stderr(missing_features.into_bytes())
            );
        }

        // Enforce unsupported
        if let Some(unsupported_features) = test.config.unsupported_features(&script.unsupported) {
            return Ok(TestResult::new(TestStatus::Unsupported)
                .with_stderr(unsupported_features.into_bytes()));
        }

        if config.options.no_execute {
            log::debug!(target: "lit:shtest", "--no-execute was set, automatically passing test");
            return Ok(TestResult::new(TestStatus::Pass));
        }

        // Get the file and directory paths for this test, relative to the test suite directory
        let test_filename = test.path.file_name().unwrap();
        let test_dir = test.path.parent().unwrap_or_else(|| Path::new(""));
        // Generate a temporary directory for this test, relative to the suite working directory
        let test_temp_dir = test.suite.working_dir().join(test_dir);
        let test_name = test_filename.to_str().unwrap();
        let temp = std::fs::create_dir_all(&test_temp_dir)
            .and_then(|_| tempdir::TempDir::new_in(&test_temp_dir, test_name))
            .expect("failed to create temporary directory");
        // Generate a temporary filename for this test, by appending .tmp to the source filename
        let temp_dir = temp.path();
        let base_temp_file = temp_dir.join(test_filename);
        let temp_file = base_temp_file.with_extension(format!(
            "{}.tmp",
            base_temp_file.extension().unwrap().to_str().unwrap()
        ));

        // Construct substitutions
        let mut substitutions = ScopedSubstitutionSet::new(&test.config.substitutions);
        substitutions.extend(
            self.extra_substitutions
                .iter()
                .map(|(k, v)| (k.clone(), v.clone())),
        );
        substitutions.extend([
            ("%s", test.absolute_path.to_string_lossy().into_owned()),
            (
                "%S",
                test.absolute_path
                    .parent()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
            ),
            ("%t", temp_file.to_string_lossy().into_owned()),
            (
                "%basename_t",
                temp_file
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
            ),
        ]);
        substitutions.insert("%%", "%");
        script
            .apply_substitutions(&mut substitutions)
            .map_err(|err| err.with_source_code(script_source.clone()))?;

        log::trace!(target: "lit:shtest", "compiled test script and applied substitutions: {script:#?}");

        let mut result = engine::run_test(&script, test, config, self)
            .map_err(|err| err.with_source_code(script_source))?;
        let expected_to_fail = if test.xfail_not {
            false
        } else {
            script
                .xfails
                .iter()
                .any(|condition| condition.evaluate(&test.config.available_features))
        };

        if expected_to_fail {
            match result.status() {
                TestStatus::Pass | TestStatus::FlakyPass => {
                    result.status = TestStatus::Xpass;
                }
                TestStatus::Fail => {
                    result.status = TestStatus::Xfail;
                }
                _ => (),
            }
        }

        Ok(result)
    }
}
