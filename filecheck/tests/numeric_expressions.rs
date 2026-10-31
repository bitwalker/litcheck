use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, source_file};

fn verify(checks: &str, input: &str) -> DiagResult<()> {
    let config = Config::default();
    let checks = source_file!(config, "checks", checks);
    let input = source_file!(config, "input", input);
    Test::new(checks, &config).verify(input).map(|_| ())
}

#[test]
fn chained_subtraction_matches_the_left_associative_value() {
    verify(
        "CHECK: N=[[#N:]]\nCHECK: value=[[#N-1-2]]!\n",
        "N=10\nvalue=7!\n",
    )
    .unwrap();
}

#[test]
fn chained_subtraction_rejects_the_right_associative_value() {
    assert!(
        verify(
            "CHECK: N=[[#N:]]\nCHECK: value=[[#N-1-2]]!\n",
            "N=10\nvalue=11!\n",
        )
        .is_err()
    );
}

#[test]
fn parentheses_preserve_explicit_right_associativity() {
    let checks = "CHECK: N=[[#N:]]\nCHECK: value=[[#N-(1-2)]]!\n";
    verify(checks, "N=10\nvalue=11!\n").unwrap();
    assert!(verify(checks, "N=10\nvalue=7!\n").is_err());
}

#[test]
fn mixed_addition_and_subtraction_are_left_associative() {
    for (expression, value) in [("N-1+2", 11), ("N+1-2", 9), ("N-1+2-3", 8)] {
        let checks = format!("CHECK: N=[[#N:]]\nCHECK: value=[[#{expression}]]!\n");
        let input = format!("N=10\nvalue={value}!\n");
        verify(&checks, &input).unwrap();
    }
}

#[test]
fn parenthesized_leading_operands_use_the_same_associativity() {
    for expression in ["(N)-1-2", "(N+1)-2-2"] {
        let checks = format!("CHECK: N=[[#N:]]\nCHECK: value=[[#{expression}]]!\n");
        verify(&checks, "N=10\nvalue=7!\n").unwrap();
        assert!(verify(&checks, "N=10\nvalue=11!\n").is_err());
    }
}

#[test]
fn capture_constraints_preserve_expression_associativity_and_bind_the_result() {
    for constraint in ["N-1-2", "==N-1-2"] {
        let checks = format!(
            "CHECK: N=[[#N:]]\nCHECK: value=[[#RESULT:{constraint}]]!\nCHECK: saved=[[#RESULT]]!\n"
        );
        verify(&checks, "N=10\nvalue=7!\nsaved=7!\n").unwrap();
    }
}

#[test]
fn equality_constraints_use_the_same_expression_rules() {
    let checks = "CHECK: N=[[#N:]]\nCHECK: value=[[#==N-1-2]]!\n";
    verify(checks, "N=10\nvalue=7!\n").unwrap();
    assert!(verify(checks, "N=10\nvalue=11!\n").is_err());
}

#[test]
fn builtin_operands_use_the_same_expression_rules() {
    verify(
        "CHECK: N=[[#N:]]\nCHECK: value=[[#add(N,0)-1-2]]!\n",
        "N=10\nvalue=7!\n",
    )
    .unwrap();
}

#[test]
fn numeric_line_expressions_are_left_associative() {
    let checks = "// first\n// second\n// third\nCHECK: value=[[#@LINE-1-2]]!\n";
    verify(checks, "value=1!\n").unwrap();
    assert!(verify(checks, "value=5!\n").is_err());
}

#[test]
fn legacy_line_expressions_are_left_associative() {
    let checks = "// first\n// second\n// third\nCHECK: value=[[@LINE-1-2]]!\n";
    verify(checks, "value=1!\n").unwrap();
    assert!(verify(checks, "value=5!\n").is_err());
}
