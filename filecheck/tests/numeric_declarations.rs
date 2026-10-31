use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, source_file};

fn verify(definitions: &[&str], checks: &str, input: &str) -> DiagResult<()> {
    verify_with_scope(definitions, checks, input, false)
}

fn verify_with_scope(
    definitions: &[&str],
    checks: &str,
    input: &str,
    scoped: bool,
) -> DiagResult<()> {
    let mut config = Config::default();
    config.options.enable_var_scope = scoped;
    config.options.variables = definitions
        .iter()
        .map(|value| value.parse().unwrap())
        .collect();
    verify_config(config, checks, input)
}

fn verify_config(config: Config, checks: &str, input: &str) -> DiagResult<()> {
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config).verify(input).map(|_| ())
}

#[test]
fn implicit_negatives_validate_numeric_declarations_like_explicit_negatives() {
    let mut config = Config::default();
    config.options.implicit_check_not = vec!["[[#N:]] [[#N+1]]".into()];
    assert!(verify_config(config, "CHECK: safe\n", "safe\n").is_err());
}

#[test]
fn labels_clear_local_declarations_when_variable_scope_is_enabled() {
    verify_with_scope(
        &[],
        "CHECK-LABEL: first\nCHECK: [[#N:]]\nCHECK-LABEL: second\nCHECK: [[N:word]]\n",
        "first\n12\nsecond\nword\n",
        true,
    )
    .unwrap();
    assert!(
        verify_with_scope(
            &["#N=12"],
            "CHECK-LABEL: first\nCHECK: [[#N:]]\nCHECK-LABEL: second\nCHECK: [[N:word]]\n",
            "first\n12\nsecond\nword\n",
            true,
        )
        .is_err()
    );
}

#[test]
fn negative_capture_declarations_do_not_escape_their_pattern() {
    verify(&[], "CHECK-NOT: [[#N:]]\nCHECK: [[N:word]]\n", "word\n").unwrap();
    verify(
        &[],
        "CHECK-NOT: [[#N:]]\nCHECK-NOT: [[N:word]]\nCHECK: end\n",
        "end\n",
    )
    .unwrap();
}

#[test]
fn forward_cli_dependencies_supply_the_established_numeric_format() {
    verify(
        &["#VALUE=BASE+1", "#%X,BASE=12"],
        "CHECK: [[#%X,VALUE:]]\n",
        "D\n",
    )
    .unwrap();
}

#[test]
fn lazy_cli_format_inference_keeps_unformatted_operands_neutral() {
    let definitions = ["#%X,BASE=12", "#OFFSET=1", "#VALUE=BASE+OFFSET"];
    verify(&definitions, "CHECK: [[#%X,VALUE:]]\n", "D\n").unwrap();
    assert!(verify(&definitions, "CHECK: [[#%u,VALUE:]]\n", "13\n").is_err());
}

#[test]
fn cli_definitions_reject_string_numeric_name_conflicts_in_both_orders() {
    for definitions in [["#N=12", "N=word"], ["N=word", "#N=12"]] {
        assert!(verify(&definitions, "CHECK: input\n", "input\n").is_err());
    }
}

#[test]
fn captures_reject_string_numeric_name_conflicts_in_both_orders() {
    for (checks, input) in [
        ("CHECK: [[#N:]]\nCHECK: [[N:word]]\n", "12\nword\n"),
        ("CHECK: [[N:word]]\nCHECK: [[#N:]]\n", "word\n12\n"),
    ] {
        assert!(verify(&[], checks, input).is_err());
    }
}

#[test]
fn captures_reject_name_conflicts_with_cli_definitions() {
    assert!(verify(&["#N=12"], "CHECK: [[N:word]]\n", "word\n").is_err());
    assert!(verify(&["N=word"], "CHECK: [[#N:]]\n", "12\n").is_err());
}

#[test]
fn numeric_expressions_reject_variables_defined_earlier_in_the_directive() {
    for checks in [
        "CHECK: [[#N:]] [[#N+1]]\n",
        "CHECK: [[#N:1]] [[#add(N,1)]]\n",
        "CHECK: [[#N:1]] [[#M:N+1]]\n",
    ] {
        assert!(verify(&[], checks, "1 2\n").is_err());
    }
}

#[test]
fn numeric_redefinitions_reject_a_different_format() {
    assert!(verify(&[], "CHECK: [[#N:]]\nCHECK: [[#%X,N:]]\n", "12\nC\n").is_err());
    assert!(verify(&["#N=12"], "CHECK: [[#%X,N:]]\n", "C\n").is_err());
}

#[test]
fn compatible_redefinitions_and_string_same_directive_uses_remain_valid() {
    verify(&[], "CHECK: [[#N:]]\nCHECK: [[#N:N+1]]\n", "1\n2\n").unwrap();
    verify(&[], "CHECK: [[#%X,N:]]\nCHECK: [[#%X,N:]]\n", "C\nD\n").unwrap();
    verify(&[], "CHECK: [[N:word]] [[N]]\n", "word word\n").unwrap();
    verify(
        &[],
        "CHECK: [[N:word]]\nCHECK: [[N:other]]\n",
        "word\nother\n",
    )
    .unwrap();
}
