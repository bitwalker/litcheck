use litcheck_filecheck::{Config, Test, source_file};

fn verify(checks: &str, input: &str) -> bool {
    let config = Config::default();
    let checks = source_file!(config, checks);
    let input = source_file!(config, input);
    Test::new(checks, &config).verify(input).is_ok()
}

#[test]
fn same_can_match_zero_width_at_the_end_of_a_line() {
    for input in ["a\n", "a\r\n", "a"] {
        assert!(verify("CHECK: a\nCHECK-SAME: {{$}}", input));
    }
}

#[test]
fn same_does_not_search_past_the_line_or_label_boundary() {
    assert!(!verify("CHECK: a\nCHECK-SAME: b", "a\nb\n"));
    assert!(!verify(
        "CHECK-LABEL: first\nCHECK: a\nCHECK-SAME: b\nCHECK-LABEL: second",
        "first\na second b\n",
    ));
}
