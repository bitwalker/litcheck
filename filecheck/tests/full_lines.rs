use litcheck_filecheck::{Config, Test, source_file};

fn matches(checks: &str, input: &str, strict: bool) -> bool {
    let mut config = Config::default();
    config.options.match_full_lines = true;
    config.options.strict_whitespace = strict;
    let checks = source_file!(config, checks);
    let input = source_file!(config, input);
    Test::new(checks, &config).verify(input).is_ok()
}

#[test]
fn positive_patterns_require_both_line_boundaries() {
    for checks in ["CHECK: line", "CHECK-LABEL: line", "CHECK-DAG: line"] {
        assert!(matches(checks, "line\n", false), "{checks}");
        assert!(!matches(checks, "a line\n", false), "{checks}");
        assert!(!matches(checks, "line suffix\n", false), "{checks}");
    }
}

#[test]
fn horizontal_padding_is_optional_unless_strict() {
    assert!(matches("CHECK: line", " \tline\t \n", false));
    assert!(!matches("CHECK: line", " line\n", true));
    assert!(!matches("CHECK: line", "line \n", true));
    assert!(matches("CHECK:  line ", " line \n", true));
    assert!(matches("CHECK: two words", "two\t words\n", false));
    assert!(matches("CHECK:   line \t", "line\n", false));
    assert!(matches("CHECK:  {{line}} ", " line \n", true));
}

#[test]
fn full_lines_preserve_regex_alternation_and_literal_modifiers() {
    assert!(matches("CHECK: {{a|b}}", "b\n", false));
    assert!(!matches("CHECK: {{a|b}}", "ab\n", false));
    assert!(matches("CHECK{LITERAL}: {{a|b}}", "{{a|b}}\n", false));
    assert!(!matches("CHECK{LITERAL}: {{a|b}}", "x{{a|b}}\n", false));
}

#[test]
fn negative_patterns_still_match_substrings() {
    assert!(!matches(
        "CHECK: start\nCHECK-NOT: bad",
        "start\na bad thing\n",
        false
    ));
}

#[test]
fn full_lines_preserve_captures_and_dynamic_substitutions_at_eof() {
    for strict in [false, true] {
        for newline in ["\n", "\r\n"] {
            assert!(matches(
                "CHECK: [[N:[0-9]+]]\nCHECK-NEXT: [[N]]",
                &format!("12{newline}12"),
                strict,
            ));
            assert!(matches(
                "CHECK-DAG: [[#N:]]\nCHECK: [[#N+1]]",
                &format!("12{newline}13"),
                strict,
            ));
        }
    }
}
