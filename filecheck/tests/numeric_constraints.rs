use litcheck_filecheck::{Config, Test, TestFailed, pattern::MatchInfo, source_file};

fn verify(checks: &str, input: &str) -> Result<Vec<MatchInfo<'static>>, TestFailed> {
    let config = Config::default();
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config)
        .verify(input)
        .map_err(|error| error.downcast::<TestFailed>().unwrap())
}

#[test]
fn captured_numeric_equality_rejects_mismatches() {
    let checks = "CHECK: [[#N: == 7]]\nCHECK: [[#N]]\n";
    assert!(verify(checks, "8\n8\n").is_err());
}

#[test]
fn captured_numeric_equality_binds_matching_values() {
    verify("CHECK: [[#N: == 7]]\nCHECK: [[#N]]\n", "7\n7\n").unwrap();
}

#[test]
fn captured_numeric_equality_searches_later_candidates() {
    let matches = verify("CHECK: [[#N: == 7]]\nCHECK: [[#N]]\n", "8\n7\n7\n").unwrap();
    assert_eq!(matches[0].span.start().to_usize(), 2);
    assert_eq!(matches[1].span.start().to_usize(), 4);
}

#[test]
fn rejected_numeric_candidates_roll_back_earlier_bindings() {
    let checks = "CHECK: seed [[#N:]]\nCHECK: [[#M: == N]] [[#N:]] [[#C: == 7]]\nCHECK: [[#N]]\n";
    let matches = verify(checks, "seed 7\n7 8 8\n7 9 7\n9\n").unwrap();
    assert_eq!(matches[1].span.start().to_usize(), 13);
    assert_eq!(matches[2].span.start().to_usize(), 19);
}
