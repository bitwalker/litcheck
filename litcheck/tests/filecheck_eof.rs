use std::{
    fs,
    io::Write,
    process::{Command, Output, Stdio},
    thread,
    time::{Duration, Instant},
};

fn check_with_deadline(checks: &str, input: &str, full_lines: bool) -> Output {
    let path = std::env::temp_dir().join(format!("litcheck-eof-{}.txt", std::process::id()));
    fs::write(&path, checks).unwrap();
    let mut command = Command::new(env!("CARGO_BIN_EXE_litcheck"));
    command.args(["filecheck", "-DX=foo"]);
    if full_lines {
        command.arg("--match-full-lines");
    }
    let mut child = command
        .arg(&path)
        .env_remove("FILECHECK_OPTS")
        .env_remove("LITCHECK_TRACE")
        .env_remove("LITCHECK_TRACE_TIMING")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(input.as_bytes())
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(2);
    while child.try_wait().unwrap().is_none() {
        if Instant::now() >= deadline {
            child.kill().unwrap();
            child.wait().unwrap();
            fs::remove_file(&path).unwrap();
            panic!("FileCheck did not terminate for {checks:?} on {input:?}");
        }
        thread::sleep(Duration::from_millis(10));
    }
    let output = child.wait_with_output().unwrap();
    fs::remove_file(&path).unwrap();
    output
}

#[test]
fn dynamic_search_retries_overlapping_prefixes_and_stops_at_eof() {
    for (checks, input) in [
        ("CHECK: oo[[X]]\n", "ooofoo"),
        ("CHECK: éé[[X]]\n", "éééfoo"),
    ] {
        let output = check_with_deadline(checks, input, false);
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    for (checks, full_lines) in [
        ("CHECK: {{^}}[[X]]\n", false),
        ("CHECK: [[X]]\n", true),
        ("CHECK: {{(?:)}}[[X]]\n", false),
    ] {
        for input in ["bar\n", "é\n", "bar\r\n"] {
            let output = check_with_deadline(checks, input, full_lines);
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert_eq!(output.status.code(), Some(1), "{stderr}");
            assert!(stderr.contains("no matches were found"), "{stderr}");
        }
    }
}
