use litcheck_filecheck::{CheckFailedError, Config, Test, TestFailed, source_file};

fn excluded_matches(checks: &str, input: &str) -> Vec<String> {
    let config = Config::default();
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    let failure = Test::new(checks, &config)
        .verify(input)
        .unwrap_err()
        .downcast::<TestFailed>()
        .unwrap();
    failure
        .errors()
        .iter()
        .map(|error| match error {
            CheckFailedError::MatchFoundButExcluded {
                span, input_file, ..
            } => input_file.source_slice(*span).unwrap().to_owned(),
            error => panic!("unexpected failure: {error:?}"),
        })
        .collect()
}

#[test]
fn all_forbidden_patterns_are_reported_before_stopping_the_block() {
    let checks = "CHECK-NOT: bar\nCHECK-NOT: foo\nCHECK: barrier\nCHECK-NOT: baz\n";
    assert_eq!(
        excluded_matches(checks, "foo\nbar\nbarrier\nbaz\n"),
        ["bar", "foo"]
    );
}

#[test]
fn exclusions_report_overlapping_patterns_independently() {
    assert_eq!(
        excluded_matches("CHECK-NOT: aba\nCHECK-NOT: ba\n", "ababa\n"),
        ["aba", "ba"]
    );
}

#[test]
fn failed_exclusions_recover_at_the_next_label() {
    let checks = "CHECK: start\nCHECK-NOT: bar\nCHECK-NOT: foo\nCHECK: barrier\nCHECK-NOT: baz\nCHECK-LABEL: next:\nCHECK-NOT: qux\nCHECK: tail\n";
    assert_eq!(
        excluded_matches(checks, "start\nfoo\nbar\nbarrier\nbaz\nnext:\nqux\ntail\n"),
        ["bar", "foo", "qux"]
    );
}

#[test]
fn exclusion_captures_do_not_change_other_exclusion_patterns() {
    let checks = "CHECK: [[X:old]]\nCHECK-NOT: [[X:forbidden]]\nCHECK-NOT: [[X]]\nCHECK: barrier\n";
    assert_eq!(
        excluded_matches(checks, "old\nforbidden\nbarrier\n"),
        ["forbidden"]
    );
}

#[test]
fn negative_numeric_capture_errors_are_not_treated_as_absence() {
    let config = Config::default();
    let checks = source_file!(config, "checks", "CHECK-NOT: [[#N:]]\n");
    let input = source_file!(
        config,
        "input",
        "999999999999999999999999999999999999999999999999999999\n"
    );
    let failure = Test::new(checks, &config)
        .verify(input)
        .unwrap_err()
        .downcast::<TestFailed>()
        .unwrap();
    assert!(
        matches!(
            failure.errors(),
            [CheckFailedError::MatchFoundConstraintFailed { .. }]
        ),
        "unexpected failure: {:?}",
        failure.errors()
    );
}
