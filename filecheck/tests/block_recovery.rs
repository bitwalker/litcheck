use litcheck_filecheck::{CheckFailedError, Config, Test, TestFailed, source_file};

#[test]
fn missing_labels_do_not_hide_failures_in_known_blocks() {
    let config = Config::default();
    let checks = source_file!(
        config,
        "checks",
        "CHECK-LABEL: first:\nCHECK: missing value\nCHECK-LABEL: missing:\n"
    );
    let input = source_file!(config, "input", "first:\nother value\n");
    let failure = Test::new(checks, &config)
        .verify(input)
        .unwrap_err()
        .downcast::<TestFailed>()
        .unwrap();
    let missing_patterns = failure
        .errors()
        .iter()
        .filter_map(|error| match error {
            CheckFailedError::MatchNoneButExpected {
                span, match_file, ..
            } => match_file.source_slice(*span),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(missing_patterns, ["missing value", "missing:"]);
}
