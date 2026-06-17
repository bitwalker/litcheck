//! Integration tests verifying the process exit code of the `litcheck` binary.

use std::process::Command;

/// Build a [Command] that invokes the `litcheck` binary on the given lit test fixture.
///
/// The command runs from the workspace root, as lit requires test paths to be
/// located under the current working directory.
fn lit_run(fixture: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_litcheck"));
    cmd.current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."));
    cmd.args(["lit", "run", &format!("tests/lit/{fixture}")]);
    cmd
}

#[test]
fn lit_run_exits_with_success_when_all_tests_pass() {
    let output = lit_run("sanity").output().expect("failed to run litcheck");
    assert!(
        output.status.success(),
        "expected exit code 0 for a passing test suite, got {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn lit_run_exits_with_failure_when_a_test_fails() {
    let output = lit_run("exit-code-fail")
        .output()
        .expect("failed to run litcheck");
    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit code 1 for a failing test suite\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}
