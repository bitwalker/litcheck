#![feature(assert_matches)]

use std::assert_matches::assert_matches;

use litcheck::diagnostics::DiagResult;
use litcheck_filecheck as filecheck;

use filecheck::{CheckFailedError, Config, Test, TestFailed};

#[test]
fn integration_test_sanity() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/sanity/check.txt"
    ));

    const INPUT_FILE: &str = r"
Some random


content to show output
and some rules
";

    let config = Config::default();
    let mut test = Test::new(MATCH_FILE, &config);
    test.verify(INPUT_FILE)?;

    Ok(())
}

#[test]
fn integration_test_sanity_check_same_violation_test() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/sanity/check.txt"
    ));

    const INPUT_FILE: &str = r"
Some random


content to
and some rules
";

    let config = Config::default();
    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(INPUT_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}
