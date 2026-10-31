use litcheck::Symbol;
use litcheck_filecheck::{CheckFailedError, Config, Test, TestFailed, source_file};

fn verify(checks: &str, input: &str) -> Result<(), TestFailed> {
    let mut config = Config::default();
    config
        .options
        .implicit_check_not
        .push(Symbol::intern("forbidden"));
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config)
        .verify(input)
        .map(|_| ())
        .map_err(|error| error.downcast::<TestFailed>().unwrap())
}

fn assert_excluded(checks: &str, input: &str) {
    let failure = verify(checks, input).expect_err("forbidden text should be rejected");
    assert!(
        failure.errors().iter().any(|error| matches!(
            error,
            CheckFailedError::MatchFoundButExcluded { span, input_file, .. }
                if input_file.source_slice(*span) == Some("forbidden")
        )),
        "expected an exclusion diagnostic: {failure:?}"
    );
}

#[test]
fn consecutive_dags_remain_unordered_with_implicit_negatives() {
    let checks = "CHECK: start\nCHECK-DAG: alpha\nCHECK-DAG: beta\nCHECK: end\n";
    verify(checks, "start\nbeta\nalpha\nend\n").unwrap();
}

#[test]
fn implicit_negatives_surround_a_dag_group() {
    let checks = "CHECK: start\nCHECK-DAG: alpha\nCHECK-DAG: beta\nCHECK: end\n";
    verify(checks, "start\nbeta\nalpha\nend\n").unwrap();
    for input in [
        "start\nforbidden\nbeta\nalpha\nend\n",
        "start\nbeta\nalpha\nforbidden\nend\n",
        "start\nbeta\nalpha\nend\nforbidden\n",
    ] {
        assert_excluded(checks, input);
    }
}

#[test]
fn explicit_negatives_still_separate_dag_groups() {
    let checks =
        "CHECK: start\nCHECK-DAG: alpha\nCHECK-NOT: forbidden\nCHECK-DAG: beta\nCHECK: end\n";
    verify(checks, "start\nalpha\nbeta\nend\n").unwrap();
    assert!(verify(checks, "start\nbeta\nalpha\nend\n").is_err());
    assert_excluded(checks, "start\nalpha\nforbidden\nbeta\nend\n");
}
