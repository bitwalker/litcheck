use std::time::Duration;

use litcheck::diagnostics::{DiagResult, Span};

use crate::{
    test::{Test, TestResult, TestStatus},
    Config,
};

use super::{
    script::directives::{Directive, ScriptCommand},
    *,
};

pub fn run_test(
    script: &TestScript,
    test: &Test,
    lit: &Config,
    config: &ShTest,
) -> DiagResult<TestResult> {
    log::debug!("running test via shtest: {}", test.name());
    // Re-run failed tests up to test.allowed_retries times.
    let max_retries = script.allowed_retries.map(Span::into_inner).unwrap_or(0);
    let mut retries = 0;
    let mut result: TestResult;
    loop {
        result = run_once(script, test, lit, config)?;
        if result.status != TestStatus::Fail {
            break;
        }
        retries += 1;
        if retries > max_retries {
            break;
        }
    }

    // If we had to run the test more than once, count it as a flaky pass. These
    // will be printed separately in the test summary.
    if retries > 0 && result.status == TestStatus::Pass {
        result.status = TestStatus::FlakyPass;
    }

    Ok(result)
}

fn run_once(
    script: &TestScript,
    test: &Test,
    lit: &Config,
    config: &ShTest,
) -> DiagResult<TestResult> {
    let script_bytes = write_script(script, config.pipefail);
    let mut command = crate::utils::ShellCommand::new(config.shell());
    command
        .current_dir(test.suite.working_dir())
        .envs(test.config.env.iter().map(|(k, v)| (&**k, &**v)))
        .paths(lit.search_paths.as_slice())?
        .pipe_stdin(script_bytes);

    Ok(command.wait_with_timeout(
        lit.timeout
            .map(Duration::from_secs)
            .unwrap_or(Duration::ZERO),
    ))
}

fn write_script(script: &TestScript, pipefail: bool) -> Vec<u8> {
    use std::fmt::Write;

    let mut buf = String::with_capacity(1024);
    if pipefail {
        buf.push_str("set -o pipefail;\n");
    }
    buf.push_str("set -x;\n");
    for (index, test) in script
        .commands
        .iter()
        .filter_map(|c| match c {
            ScriptCommand::Run(ref cmd) => Some(cmd),
            _ => None,
        })
        .enumerate()
    {
        if index > 0 {
            buf.push_str(" && ");
        }
        buf.push_str("{\n  ");
        let quoted = shlex::try_quote(&test.command).expect("bad shell command");
        buf.push_str("{ set +x; } 2>/dev/null && ");
        write!(
            &mut buf,
            "echo 'RUN at line {}': {} >&2 && ",
            test.line(),
            &quoted
        )
        .unwrap();
        buf.push_str("{ set -x; } 2>/dev/null && {\n");
        buf.push_str(&test.command);
        buf.push_str("\n};\n}");
    }
    buf.push('\n');
    buf.into_bytes()
}
