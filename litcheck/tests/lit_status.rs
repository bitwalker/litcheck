//! Exercise outcome classification through the CLI, including retry execution.

#![cfg(unix)]

use std::{fs, process::Command};

fn lit_run(fixture: &str) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_litcheck"));
    command
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."))
        .args(["lit", "run", "--verbose", "--timeout", "5", fixture])
        .env_remove("LIT_FILTER")
        .env_remove("LIT_FILTER_OUT")
        .env_remove("FILECHECK_OPTS")
        .env_remove("LITCHECK_TRACE")
        .env_remove("LITCHECK_TRACE_TIMING");
    command
}

#[test]
fn shell_test_statuses_and_exit_codes_reflect_actual_execution() {
    let marker = std::env::temp_dir().join(format!("litcheck-status-{}", std::process::id()));
    let _ = fs::remove_file(&marker);
    for (fixture, status, exit_code) in [
        ("xfail", "XFAIL", 0),
        ("xpass", "XPASS", 1),
        ("requires", "UNSUPPORTED", 0),
        ("unsupported", "UNSUPPORTED", 0),
        ("no-run", "UNRESOLVED", 1),
        ("retry-pass", "FLAKYPASS", 0),
        ("retry-fail", "FAIL", 1),
    ] {
        let output = lit_run(&format!("tests/lit/status/{fixture}.txt"))
            .env("LIT_MARKER", &marker)
            .output()
            .expect("run status fixture");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert_eq!(output.status.code(), Some(exit_code), "{text}");
        assert!(
            text.lines()
                .any(|line| line == format!("{status}: status::{fixture}.txt")),
            "{text}"
        );
        assert!(
            !marker.exists(),
            "an unsupported test executed its RUN line"
        );
    }
}

#[test]
fn inherited_environment_reaches_shell_commands_after_substitution() {
    let output = lit_run("tests/lit/environment")
        .env("BAR", "bar")
        .output()
        .expect("run environment fixture");
    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("PASS: lit-env::"));
}
