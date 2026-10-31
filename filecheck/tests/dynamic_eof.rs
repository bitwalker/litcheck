use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, source_file};

fn verify(checks: &str, input: &str) -> DiagResult<()> {
    let config = Config::default();
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config).verify(input).map(|_| ())
}

#[test]
fn numeric_capture_constraints_run_after_consuming_the_last_byte() {
    for checks in ["CHECK: [[#VALUE:12]]\n", "CHECK: [[#==12]]\n"] {
        verify(checks, "12").unwrap();
    }
    for checks in ["CHECK: [[#VALUE:13]]\n", "CHECK: [[#==13]]\n"] {
        assert!(verify(checks, "12").is_err());
    }
}

#[test]
fn substitution_can_be_followed_by_an_end_assertion_at_eof() {
    verify("CHECK: [[X:foo]]\nCHECK: [[X]]{{$}}\n", "foo\nfoo").unwrap();
}

#[test]
fn substitution_can_be_followed_by_an_end_assertion_in_a_bounded_line() {
    for input in ["foo\nfoo\n", "foo\r\nfoo\r\n"] {
        verify("CHECK: [[X:foo]]\nCHECK-NEXT: {{^}}[[X]]{{$}}\n", input).unwrap();
    }
}

#[test]
fn dynamic_patterns_can_match_an_empty_suffix() {
    verify("CHECK: [[X:foo]]\nCHECK: [[X]]{{!?}}\n", "foo\nfoo").unwrap();
    assert!(verify("CHECK: [[X:foo]]\nCHECK: [[X]]!\n", "foo\nfoo").is_err());
}
