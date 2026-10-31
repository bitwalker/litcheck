//! Time limits must bound the shell process, not just classify its eventual exit.

#![cfg(unix)]

use std::process::Command;
use std::time::{Duration, Instant};

#[test]
fn timed_out_shell_test_reports_timeout_promptly() {
    let started = Instant::now();
    let output = Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."))
        .args([
            "lit",
            "run",
            "--show-all",
            "--timeout",
            "1",
            "tests/lit/timeout",
        ])
        .output()
        .expect("failed to run litcheck");
    let elapsed = started.elapsed();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        elapsed < Duration::from_millis(2500),
        "one-second deadline took {elapsed:?}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    );
    assert_eq!(output.status.code(), Some(1), "{stdout}\n{stderr}");
    assert!(stdout.contains("TIMEOUT:"), "{stdout}\n{stderr}");
    assert!(stdout.contains("timeout-stdout"), "{stdout}\n{stderr}");
    assert!(stdout.contains("timeout-stderr"), "{stdout}\n{stderr}");
}
