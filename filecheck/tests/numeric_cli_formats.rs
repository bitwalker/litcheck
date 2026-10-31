use litcheck::diagnostics::DiagResult;
use litcheck_filecheck::{Config, Test, source_file};

fn verify(definitions: &[&str], input: &str) -> DiagResult<()> {
    let mut config = Config::default();
    config.options.variables = definitions
        .iter()
        .map(|definition| definition.parse().unwrap())
        .collect();
    let checks = source_file!(config, "checks", "CHECK: value=[[#VALUE]]!\n");
    let input = source_file!(config, "input", input);
    Test::new(checks, &config).verify(input).map(|_| ())
}

#[test]
fn explicit_cli_format_applies_to_literal_values() {
    let definitions = ["#%X,VALUE=12"];
    verify(&definitions, "value=C!\n").unwrap();
    assert!(verify(&definitions, "value=12!\n").is_err());
}

#[test]
fn explicit_cli_format_applies_to_expression_results() {
    let definitions = ["#BASE=8", "#%X,VALUE=BASE+4"];
    verify(&definitions, "value=C!\n").unwrap();
    assert!(verify(&definitions, "value=12!\n").is_err());
}

#[test]
fn cli_expression_inherits_its_operand_format() {
    let definitions = ["#%X,BASE=8", "#VALUE=BASE+4"];
    verify(&definitions, "value=C!\n").unwrap();
    assert!(verify(&definitions, "value=12!\n").is_err());
}

#[test]
fn explicit_cli_format_overrides_a_referenced_variable_format() {
    verify(&["#%x,BASE=12", "#%X,VALUE=BASE"], "value=C!\n").unwrap();
}

#[test]
fn explicit_cli_format_overrides_conflicting_operand_formats() {
    verify(
        &["#%x,BASE=8", "#%u,OFFSET=4", "#%X,VALUE=BASE+OFFSET"],
        "value=C!\n",
    )
    .unwrap();
}

#[test]
fn cli_expression_preserves_hex_prefix_and_precision() {
    let definitions = ["#BASE=8", "#%#.4X,VALUE=BASE+4"];
    verify(&definitions, "value=0x000C!\n").unwrap();
    assert!(verify(&definitions, "value=C!\n").is_err());
}
