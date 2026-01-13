use std::sync::Arc;

use litcheck::{assert_matches, diagnostics::DiagResult};
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

#[test]
fn integration_test_check_dag_not_dag_not_search_end() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-not-dag.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("NotSearchEnd").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let matches = test.verify(MATCH_FILE)?;
    assert_eq!(matches.len(), 9);

    Ok(())
}

#[test]
fn integration_test_check_dag_not_dag_multi_dag_search_start() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-not-dag.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("Dag2SearchStart").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let matches = test.verify(MATCH_FILE)?;
    assert_eq!(matches.len(), 8);

    Ok(())
}

#[test]
fn integration_test_check_dag_example() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag.txt"
    ));

    let config = Config::default();
    let mut test = Test::new(MATCH_FILE, &config);
    let matches = test.verify(MATCH_FILE)?;
    assert_eq!(matches.len(), 12);

    Ok(())
}

#[test]
fn integration_test_check_dag_xfail_x1() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X1").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}

#[test]
fn integration_test_check_dag_xfail_x2() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X2").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}

#[test]
fn integration_test_check_dag_xfail_x3() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X3").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}

#[test]
fn integration_test_check_dag_xfail_x4() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X4").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchFoundButExcluded { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}

// Tests a tree like:
//
//       NOT
//     /    \
//    DAG    DAG
//
// In such cases, the NOT pattern must not match between
// the matches of the left and right DAG patterns, but
// ALSO we must validate that the left and right DAG
// patterns are matched left-to-right, not right-to-left.
//
// This necessarily implies that a tree like this should
// search the same region for all three patterns, and work
// out whether the matches overlap/occur as expected.
//
// For deeper trees, this same logic applies in a recursive
// fashion, using a depth-first strategy. This is what gives
// the CHECK-DAG directive it's ability to match directed
// acyclic graphs, as implied by the name.
#[test]
fn integration_test_check_dag_xfail_x5() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X5").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchFoundButExcluded { .. }, CheckFailedError::MatchFoundButDiscarded { .. }] =
        error.errors()
    {
        Ok(())
    } else {
        Err(error.into())
    }
}

#[test]
fn integration_test_check_dag_xfail_x6() -> DiagResult<()> {
    const MATCH_FILE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/llvm/check-dag-xfails.txt"
    ));

    let mut config = Config::default();
    config.check_prefixes.clear();
    config
        .check_prefixes
        .push(Arc::from(String::from("X6").into_boxed_str()));

    let mut test = Test::new(MATCH_FILE, &config);
    let result = test.verify(MATCH_FILE);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}
