use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, TestFailed, source_file};

fn verify(config: &Config, checks: &str, input: &str) -> DiagResult<()> {
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, config).verify(input).map(|_| ())
}

const CAPTURE_CHECKS: &str = "CHECK: capture <[[V:.*]]>\nCHECK-NEXT: use <[[V]]> end\n";

#[test]
fn captured_metacharacters_match_their_literal_bytes() {
    let config = Config::default();
    for captured in ["a.b", "[", "]", "a+b", "(x)", r"a\b", "a|b", ".*"] {
        let input = format!("capture <{captured}>\nuse <{captured}> end\n");
        verify(&config, CAPTURE_CHECKS, &input).unwrap();
    }
}

#[test]
fn captured_metacharacters_do_not_match_regex_alternatives() {
    let config = Config::default();
    for (captured, different) in [
        ("a.b", "axb"),
        ("[", "x"),
        ("a+b", "aaab"),
        ("(x)", "x"),
        (r"a\b", "ab"),
        ("a|b", "a"),
        (".*", "anything"),
    ] {
        let input = format!("capture <{captured}>\nuse <{different}> end\n");
        let error = verify(&config, CAPTURE_CHECKS, &input).unwrap_err();
        assert!(error.downcast::<TestFailed>().is_ok());
    }
}

#[test]
fn standalone_string_substitutions_are_literal() {
    let checks = "CHECK: [[V:a.b]]\nCHECK-NEXT: [[V]]\n";
    verify(&Config::default(), checks, "a.b\na.b\n").unwrap();
    assert!(verify(&Config::default(), checks, "a.b\naxb\n").is_err());
}

#[test]
fn string_substitutions_preserve_case_matching_options() {
    let mut config = Config::default();
    for (captured, different_case) in [("A.B", "a.b"), ("Ä.B", "ä.b")] {
        let input = format!("capture <{captured}>\nuse <{different_case}> end\n");
        config.options.ignore_case = false;
        assert!(verify(&config, CAPTURE_CHECKS, &input).is_err());
        config.options.ignore_case = true;
        verify(&config, CAPTURE_CHECKS, &input).unwrap();
    }
}

#[test]
fn string_substitutions_preserve_captured_whitespace() {
    let mut config = Config::default();
    for strict_whitespace in [false, true] {
        config.options.strict_whitespace = strict_whitespace;
        verify(&config, CAPTURE_CHECKS, "capture <a\tb>\nuse <a\tb> end\n").unwrap();
        assert!(verify(&config, CAPTURE_CHECKS, "capture <a\tb>\nuse <a b> end\n").is_err());
    }
}

#[test]
fn empty_string_substitutions_match_without_consuming_input() {
    verify(
        &Config::default(),
        CAPTURE_CHECKS,
        "capture <>\nuse <> end\n",
    )
    .unwrap();
}

#[test]
fn numeric_substitutions_preserve_explicit_formatting() {
    let checks = "CHECK: N=[[#N:]]\nCHECK-NEXT: value=[[#%#.4X,N]]!\n";
    verify(&Config::default(), checks, "N=15\nvalue=0x000F!\n").unwrap();
    assert!(verify(&Config::default(), checks, "N=15\nvalue=15!\n").is_err());
}
