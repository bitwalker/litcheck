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

#[test]
fn implicit_negatives_cover_every_interval_in_a_label_body() {
    let checks = "CHECK-LABEL: function:\nCHECK: first\nCHECK: last\n";
    verify(checks, "function:\nfirst\nlast\n").unwrap();
    for input in [
        "forbidden\nfunction:\nfirst\nlast\n",
        "function:\nforbidden\nfirst\nlast\n",
        "function:\nfirst\nforbidden\nlast\n",
        "function:\nfirst\nlast\nforbidden\n",
    ] {
        assert_excluded(checks, input);
    }
}

#[test]
fn implicit_negatives_cover_successive_label_bodies() {
    let checks = "CHECK-LABEL: one:\nCHECK: first\nCHECK-LABEL: two:\nCHECK: last\n";
    verify(checks, "one:\nfirst\ntwo:\nlast\n").unwrap();
    for input in [
        "one:\nfirst\nforbidden\ntwo:\nlast\n",
        "one:\nfirst\ntwo:\nforbidden\nlast\n",
        "one:\nfirst\ntwo:\nlast\nforbidden\n",
    ] {
        assert_excluded(checks, input);
    }
}

#[test]
fn implicit_negatives_cover_empty_label_bodies() {
    for checks in [
        "CHECK-LABEL: one:\n",
        "CHECK-LABEL: one:\nCHECK-LABEL: two:\n",
    ] {
        verify(checks, "one:\ntwo:\n").unwrap();
        assert_excluded(checks, "one:\nforbidden\ntwo:\n");
        assert_excluded(checks, "one:\ntwo:\nforbidden\n");
    }
    assert!(verify("CHECK-LABEL: one:\nCHECK-LABEL: absent:\n", "one:\n").is_err());
}

#[test]
fn implicit_negatives_preserve_dependent_checks() {
    for start in ["CHECK: function:", "CHECK-LABEL: function:"] {
        let checks = format!(
            "{start}\nCHECK: first\nCHECK-SAME: suffix\nCHECK-NEXT: second\nCHECK-EMPTY:\nCHECK-NEXT: last\n"
        );
        verify(&checks, "function:\nfirst suffix\nsecond\n\nlast\n").unwrap();
        assert_excluded(
            &checks,
            "function:\nfirst forbidden suffix\nsecond\n\nlast\n",
        );
        assert!(verify(&checks, "function:\nfirst\nsuffix\nsecond\n\nlast\n").is_err());
        assert!(verify(&checks, "function:\nfirst suffix\n\nsecond\n\nlast\n").is_err());
        assert!(verify(&checks, "function:\nfirst suffix\nsecond\nnonempty\nlast\n").is_err());
    }
}

#[test]
fn implicit_negatives_preserve_labeled_dag_groups() {
    let checks = "CHECK-LABEL: function:\nCHECK-DAG: alpha\nCHECK-DAG: beta\nCHECK: end\n";
    verify(checks, "function:\nbeta\nalpha\nend\n").unwrap();
    assert_excluded(checks, "function:\nbeta\nalpha\nforbidden\nend\n");
}

#[test]
fn implicit_negatives_surround_count_as_one_matching_unit() {
    for start in ["CHECK: function:", "CHECK-LABEL: function:"] {
        let checks = format!("{start}\nCHECK-COUNT-2: alpha\nCHECK: last\n");
        // A count spans all of its repetitions; implicit exclusions apply outside that span.
        verify(&checks, "function:\nalpha\nforbidden\nalpha\nlast\n").unwrap();
        assert_excluded(&checks, "function:\nforbidden\nalpha\nalpha\nlast\n");
        assert_excluded(&checks, "function:\nalpha\nalpha\nforbidden\nlast\n");
        assert_excluded(&checks, "function:\nalpha\nalpha\nlast\nforbidden\n");
    }
}

#[test]
fn ordinary_and_explicit_negative_controls_reject_forbidden_text() {
    let input = "function:\nfirst\nforbidden\nlast\n";
    assert_excluded("CHECK: function:\nCHECK: first\nCHECK: last\n", input);
    assert_excluded(
        "CHECK-LABEL: function:\nCHECK: first\nCHECK-NOT: forbidden\nCHECK: last\n",
        input,
    );
}

#[test]
fn implicit_negatives_allow_forbidden_text_matched_by_positive_checks() {
    let checks = "CHECK-LABEL: function:\nCHECK: first forbidden\nCHECK: last\n";
    verify(checks, "function:\nfirst forbidden\nlast\n").unwrap();
}

#[test]
fn implicit_negatives_cover_unmatched_text_on_label_lines() {
    for newline in ["\n", "\r\n"] {
        let checks = "CHECK-LABEL: one:\nCHECK-LABEL: two:\n";
        verify(checks, &format!("one:{newline}two:")).unwrap();
        for input in [
            format!("forbidden one:{newline}two:"),
            format!("one: forbidden{newline}two:"),
            format!("one:{newline}forbidden two:"),
            format!("one:{newline}two: forbidden"),
        ] {
            assert_excluded(checks, &input);
        }
    }
}

#[test]
fn implicit_negatives_cover_gaps_between_same_line_labels() {
    let checks = "CHECK-LABEL: one:\nCHECK-LABEL: two:\n";
    assert_excluded(checks, "one: forbidden two:");
    verify(checks, "one: two:").unwrap();
}

#[test]
fn implicit_negatives_allow_text_consumed_by_label_matches() {
    verify("CHECK-LABEL: forbidden\n", "forbidden").unwrap();
    verify("CHECK-LABEL: {{start\\nend}}\n", "start\nend").unwrap();
    assert_excluded("CHECK-LABEL: {{start\\nend}}\n", "start\nend forbidden");
    assert_excluded("CHECK-LABEL: {{^}}\n", "forbidden\n");
}
