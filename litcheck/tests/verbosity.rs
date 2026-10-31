use std::{fs, process::Command};

#[test]
fn repeated_verbose_flags_expand_failure_context_unless_dumping_is_disabled() {
    let root = std::env::temp_dir().join(format!("litcheck-verbosity-{}", std::process::id()));
    fs::create_dir_all(&root).unwrap();
    let checks = root.join("checks.txt");
    let input = root.join("input.txt");
    fs::write(&checks, "CHECK: absent\n").unwrap();
    fs::write(&input, "a line\n".repeat(30)).unwrap();
    for (args, expanded, dumping) in [
        (vec!["-v"], false, true),
        (vec!["-vv"], true, true),
        (vec!["-v", "-v"], true, true),
        (vec!["-vv", "--dump-input=never"], false, false),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_litcheck"))
            .args(["filecheck", "--input-file"])
            .arg(&input)
            .arg(&checks)
            .args(args)
            .env_remove("FILECHECK_OPTS")
            .env_remove("LITCHECK_TRACE")
            .env_remove("LITCHECK_TRACE_TIMING")
            .output()
            .unwrap();
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(output.status.code(), Some(1), "{stderr}");
        assert_eq!(
            stderr.contains("searched this region of the input (lines 1-30)"),
            expanded,
            "{stderr}"
        );
        assert_eq!(
            stderr.contains("searched 30 lines of input without a match"),
            dumping && !expanded,
            "{stderr}"
        );
    }
    fs::remove_dir_all(root).unwrap();
}
