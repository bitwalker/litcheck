use litcheck_filecheck::{CheckFailedError, Config, Test, TestFailed, source_file};

fn verify(checks: &str, input: &str) -> Result<(), TestFailed> {
    let config = Config::default();
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config)
        .verify(input)
        .map(|_| ())
        .map_err(|error| error.downcast::<TestFailed>().unwrap())
}

#[test]
fn explicit_negatives_cover_unmatched_label_line_text() {
    let checks = "CHECK-NOT: forbidden\nCHECK-LABEL: one:\nCHECK-NOT: forbidden\nCHECK-LABEL: two:\nCHECK-NOT: forbidden\n";
    verify(checks, "one:\ntwo:").unwrap();
    for input in [
        "forbidden one:\ntwo:",
        "one: forbidden\ntwo:",
        "one:\ntwo: forbidden",
    ] {
        let failure = verify(checks, input).unwrap_err();
        assert!(failure.errors().iter().any(|error| matches!(
            error,
            CheckFailedError::MatchFoundButExcluded { span, input_file, .. }
                if input_file.source_slice(*span) == Some("forbidden")
        )));
    }
}

#[test]
fn positive_checks_can_match_unmatched_parts_of_label_lines() {
    verify("CHECK-LABEL: one:\nCHECK-SAME: value\n", "one: value").unwrap();
    verify(
        "CHECK-LABEL: one:\nCHECK: value\nCHECK-LABEL: two:\n",
        "one: value two:",
    )
    .unwrap();
}

#[test]
fn same_checks_cannot_cross_the_next_label() {
    let checks = "CHECK-LABEL: one:\nCHECK: value\nCHECK-SAME: stolen\nCHECK-LABEL: two:\n";
    assert!(verify(checks, "one: value two: stolen\n").is_err());
}

#[test]
fn next_checks_cannot_cross_the_next_label() {
    let checks = "CHECK-LABEL: one:\nCHECK-NEXT: stolen\nCHECK-LABEL: two:\n";
    assert!(verify(checks, "one:\nprefix two: stolen\n").is_err());
    // No next line belongs to the first block. Report a failure rather than panicking.
    assert!(verify(checks, "one: two: stolen\n").is_err());
}

#[test]
fn next_checks_can_match_before_the_next_label_on_its_line() {
    let checks = "CHECK-LABEL: one:\nCHECK-NEXT: value\nCHECK-LABEL: two:\n";
    for input in ["one:\nvalue two:", "one:\r\nvalue two:"] {
        verify(checks, input).unwrap();
    }
}

#[test]
fn dependent_checks_follow_the_end_of_a_multiline_label() {
    let checks = "CHECK-LABEL: {{one\\ntwo}}\nCHECK-SAME: suffix\nCHECK-NEXT: value\n";
    verify(checks, "one\ntwo suffix\nvalue\n").unwrap();
    assert!(verify("CHECK-LABEL: {{one\\ntwo}}\nCHECK: two\n", "one\ntwo\n").is_err());
}

#[test]
fn empty_checks_stay_within_label_bounds() {
    let checks = "CHECK-LABEL: one:\nCHECK-EMPTY:\nCHECK-LABEL: two:\n";
    verify(checks, "one:\n\ntwo:\n").unwrap();
    assert!(verify(checks, "one: two:\n\n").is_err());
    assert!(verify(checks, "one:\ntwo:\n\n").is_err());
    assert!(
        verify(
            "CHECK-LABEL: one:\nCHECK-EMPTY:\nCHECK-LABEL: {{$}}\n",
            "one:\n\n",
        )
        .is_err()
    );
    assert!(verify("CHECK-LABEL: one:\nCHECK-EMPTY:\n", "one:").is_err());
}

#[test]
fn empty_checks_accept_a_label_suffix_before_the_empty_line() {
    let checks = "CHECK-LABEL: one:\nCHECK-EMPTY:\nCHECK-NEXT: value\n";
    verify(checks, "one: suffix\n\nvalue\n").unwrap();
}
