//! Executable test formats must work without requesting a stdin pipe.

#![cfg(unix)]

use std::process::{Command, Output};

fn run(fixture: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."))
        .args(["lit", "run", "--show-all", &format!("tests/lit/{fixture}")])
        .output()
        .expect("failed to run litcheck")
}

fn assert_result(fixture: &str, exit_code: i32, classification: &str) {
    let output = run(fixture);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        output.status.code(),
        Some(exit_code),
        "stdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert!(
        stdout.contains(classification),
        "expected {classification:?}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
}

#[test]
fn executable_success_reports_pass() {
    assert_result("executable-pass", 0, "PASS:");
}

#[test]
fn executable_failure_reports_fail() {
    assert_result("executable-fail", 1, "FAIL:");
}

#[test]
fn shell_scripts_still_receive_piped_input() {
    assert_result("sanity", 0, "PASS:");
}
