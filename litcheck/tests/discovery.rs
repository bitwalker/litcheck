use std::path::Path;
use std::process::{Command, Output};

fn run_selection(selection: &str) -> Output {
    let suite = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/lit/path-selection");
    Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(suite)
        .args(["lit", "run", "--show-all", selection])
        .output()
        .expect("failed to run litcheck")
}

fn output_text(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn selected_subdirectories_execute_their_own_tests() {
    for (selection, expected_test) in [
        ("sub", "path-selection::sub/a.txt"),
        ("sub/deep", "path-selection::sub/deep/a.txt"),
    ] {
        let output = run_selection(selection);
        let text = output_text(&output);
        assert_eq!(output.status.code(), Some(1), "{text}");
        assert!(text.contains(&format!("FAIL: {expected_test}")), "{text}");
        assert!(!text.contains("PASS: path-selection::a.txt"), "{text}");
    }
}

#[test]
fn selected_directory_tests_do_not_require_root_namesakes() {
    let output = run_selection("unique");
    let text = output_text(&output);
    assert!(output.status.success(), "{text}");
    assert!(
        text.contains("PASS: path-selection::unique/only.txt"),
        "{text}"
    );
}

#[test]
fn directory_and_file_selections_preserve_the_same_test_identity() {
    for selection in ["sub/deep", "sub/deep/a.txt"] {
        let output = run_selection(selection);
        let text = output_text(&output);
        assert_eq!(output.status.code(), Some(1), "{text}");
        assert!(
            text.contains("FAIL: path-selection::sub/deep/a.txt"),
            "{text}"
        );
    }
}
