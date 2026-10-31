use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, source_file};

fn verify(checks: &str, input: &str, strict_whitespace: bool) -> DiagResult<()> {
    let mut config = Config::default();
    config.options.strict_whitespace = strict_whitespace;
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config).verify(input).map(|_| ())
}

#[test]
fn regex_line_anchors_accept_crlf_input() -> DiagResult<()> {
    for strict in [false, true] {
        for (checks, input) in [
            ("CHECK: {{^foo$}}\n", "foo\r\n"),
            ("CHECK-LABEL: {{^foo$}}\n", "foo\r\n"),
            (
                "CHECK: [[X:foo]]\nCHECK: {{^}}[[X]]{{$}}\n",
                "foo\r\nfoo\r\n",
            ),
            (
                "CHECK-DAG: {{^foo$}}\nCHECK-DAG: {{^bar$}}\n",
                "bar\r\nfoo\r\n",
            ),
            ("CHECK-DAG: [[X:^foo$]]\nCHECK: [[X]]\n", "foo\r\nfoo\r\n"),
            ("CHECK-DAG: [[X:^f.*o$]]\nCHECK: [[X]]\n", "foo\r\nfoo\r\n"),
        ] {
            verify(checks, input, strict)?;
        }
    }
    Ok(())
}

#[test]
fn check_file_crlf_is_not_part_of_literal_patterns() -> DiagResult<()> {
    for strict in [false, true] {
        for checks in [
            "CHECK: foo\r\nCHECK-NEXT: bar\r\n",
            "CHECK{LITERAL}: foo\r\nCHECK-NEXT{LITERAL}: bar\r\n",
            "CHECK: foo\r\nCHECK-NEXT: bar\n",
            "CHECK: foo\nCHECK-NEXT: bar\r\n",
        ] {
            for input in ["foo\nbar\n", "foo\r\nbar\r\n"] {
                verify(checks, input, strict)?;
            }
        }
    }
    Ok(())
}

#[test]
fn crlf_check_files_preserve_empty_line_checks() -> DiagResult<()> {
    let checks = "CHECK: foo\r\nCHECK-EMPTY:\r\nCHECK-NEXT: bar\r\n";
    for strict in [false, true] {
        for input in ["foo\n\nbar\n", "foo\r\n\r\nbar\r\n"] {
            verify(checks, input, strict)?;
        }
    }
    Ok(())
}
