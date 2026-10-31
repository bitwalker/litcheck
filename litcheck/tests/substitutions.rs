//! Script-defined substitutions are literal; configured substitutions support regex captures.

#![cfg(unix)]

use std::process::{Command, Output};

fn run_fixture(fixture: &str) -> Output {
    Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."))
        .args([
            "lit",
            "run",
            "--show-all",
            &format!("tests/lit/define-substitutions/{fixture}.txt"),
        ])
        .env_remove("LIT_FILTER")
        .env_remove("LIT_FILTER_OUT")
        .env_remove("LITCHECK_TRACE")
        .env_remove("FILECHECK_OPTS")
        .output()
        .expect("run substitution fixture")
}

fn output_text(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn define_and_redefine_execute_literal_replacements_and_preserve_escaping() {
    let output = run_fixture("execute");
    let text = output_text(&output);
    assert!(output.status.success(), "{text}");
    assert!(
        text.contains("PASS: define-substitutions::execute.txt"),
        "{text}"
    );
}

#[test]
fn invalid_script_substitutions_abort_execution() {
    for fixture in ["duplicate", "undefined"] {
        let output = run_fixture(fixture);
        let text = output_text(&output);
        assert_eq!(output.status.code(), Some(1), "{text}");
        assert!(!text.contains("PASS:"), "{text}");
    }
}
