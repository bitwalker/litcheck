use litcheck::{Symbol, assert_matches, diagnostics::DiagResult};
use litcheck_filecheck as filecheck;

use filecheck::{CheckFailedError, Config, Options, Test, TestFailed, source_file};

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
    let match_file = source_file!(config, MATCH_FILE);
    let input_file = source_file!(config, INPUT_FILE);
    let mut test = Test::new(match_file, &config);
    test.verify(input_file)?;

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
    let match_file = source_file!(config, MATCH_FILE);
    let input_file = source_file!(config, INPUT_FILE);
    let mut test = Test::new(match_file, &config);
    let result = test.verify(input_file);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchGroupFailed { .. }] = error.errors() {
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
    config.options.check_prefixes.clear();
    config
        .options
        .check_prefixes
        .push(Symbol::intern("NotSearchEnd"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let matches = test.verify(match_file)?;
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
    config.options.check_prefixes.clear();
    config
        .options
        .check_prefixes
        .push(Symbol::intern("Dag2SearchStart"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let matches = test.verify(match_file)?;
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
    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let matches = test.verify(match_file)?;
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X1"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X2"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X3"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchFoundButDiscarded { .. }] = error.errors() {
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X4"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X5"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [
        CheckFailedError::MatchFoundButExcluded { .. },
        CheckFailedError::MatchFoundButDiscarded { .. },
    ] = error.errors()
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
    config.options.check_prefixes.clear();
    config.options.check_prefixes.push(Symbol::intern("X6"));

    let match_file = source_file!(config, MATCH_FILE);
    let mut test = Test::new(match_file.clone(), &config);
    let result = test.verify(match_file);
    assert_matches!(&result, Err(_));

    let error = result.unwrap_err().downcast::<TestFailed>().unwrap();
    if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
        Ok(())
    } else {
        Err(error.into())
    }
}

#[test]
fn midenc_hir_example_with_local_bindings_and_var_scoping_disabled() -> DiagResult<()> {
    const SOURCE: &str = "\
public builtin.function @test::spill_nested_scf_if(v0: ptr<element, u8>) -> u32 {
^block1(v0: ptr<element, u8>):
     v2 = hir.ptr_to_int v0 : u32;
     v3 = arith.constant 32 : u32;
     v4 = arith.add v2, v3 : u32 #[overflow = unchecked];
     v5 = hir.int_to_ptr v4 : ptr<element, u128>;
     v6 = hir.load v5 : u128;
     v7 = arith.constant 64 : u32;
     v8 = arith.add v2, v7 : u32 #[overflow = unchecked];
     v9 = hir.int_to_ptr v8 : ptr<element, u128>;
     v10 = hir.load v9 : u128;
     v11 = arith.constant 1 : u64;
     hir.store_local v5 #[local = lv0];
     v12 = hir.exec @test/example(v9, v6, v10, v10, v11) : u32
     v13 = arith.constant 0 : u32;
     v14 = arith.eq v12, v13 : i1;
     v15 = scf.if v14 : u32 {
     ^block3:
         v18 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v18, v4;
         scf.yield v12;
     } else {
     ^block4:
         v19 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v19, v4;
         scf.yield v12;
     };
     cf.br ^block2(v15);
 ^block2(v1: u32):
     builtin.ret v1;
};
";
    const CHECKS: &str = r#"; COM: Spill of v5
; CHECK:     hir.store_local v5 #[local = [[L:lv[0-9]+]]];
; CHECK-NEXT:     {{v[0-9]+}} = hir.exec @test/example

; COM: First reload of v5
; CHECK-LABEL:     ^block3
; CHECK:         [[R1:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[L]]];
; CHECK-NEXT:         hir.store [[R1]], v[[#]]

; COM: Second reload of v5
; CHECK-LABEL:     ^block4
; CHECK:         [[R2:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[L]]];
; CHECK-NEXT:         hir.store [[R2]], v[[#]]
"#;

    let config = Config {
        options: Options {
            enable_var_scope: false,
            ..Options::default()
        },
        ..Config::default()
    };
    let match_file = source_file!(config, CHECKS);
    let input_file = source_file!(config, SOURCE);
    let mut test = Test::new(match_file, &config);
    test.verify(input_file)?;
    Ok(())
}

#[test]
#[should_panic = "reference to undefined variable 'L'"]
fn midenc_hir_example_with_local_bindings_and_var_scoping_enabled() {
    const SOURCE: &str = "\
public builtin.function @test::spill_nested_scf_if(v0: ptr<element, u8>) -> u32 {
^block1(v0: ptr<element, u8>):
     v2 = hir.ptr_to_int v0 : u32;
     v3 = arith.constant 32 : u32;
     v4 = arith.add v2, v3 : u32 #[overflow = unchecked];
     v5 = hir.int_to_ptr v4 : ptr<element, u128>;
     v6 = hir.load v5 : u128;
     v7 = arith.constant 64 : u32;
     v8 = arith.add v2, v7 : u32 #[overflow = unchecked];
     v9 = hir.int_to_ptr v8 : ptr<element, u128>;
     v10 = hir.load v9 : u128;
     v11 = arith.constant 1 : u64;
     hir.store_local v5 #[local = lv0];
     v12 = hir.exec @test/example(v9, v6, v10, v10, v11) : u32
     v13 = arith.constant 0 : u32;
     v14 = arith.eq v12, v13 : i1;
     v15 = scf.if v14 : u32 {
     ^block3:
         v18 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v18, v4;
         scf.yield v12;
     } else {
     ^block4:
         v19 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v19, v4;
         scf.yield v12;
     };
     cf.br ^block2(v15);
 ^block2(v1: u32):
     builtin.ret v1;
};
";
    const CHECKS: &str = r#"; COM: Spill of v5
; CHECK:     hir.store_local v5 #[local = [[L:lv[0-9]+]]];
; CHECK-NEXT:     {{v[0-9]+}} = hir.exec @test/example

; COM: First reload of v5
; CHECK-LABEL:     ^block3
; CHECK:         [[R1:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[L]]];
; CHECK-NEXT:         hir.store [[R1]], v[[#]]

; COM: Second reload of v5
; CHECK-LABEL:     ^block4
; CHECK:         [[R2:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[L]]];
; CHECK-NEXT:         hir.store [[R2]], v[[#]]
"#;

    let config = Config {
        options: Options {
            enable_var_scope: true,
            ..Options::default()
        },
        ..Config::default()
    };
    let match_file = source_file!(config, CHECKS);
    let input_file = source_file!(config, SOURCE);
    let mut test = Test::new(match_file, &config);
    test.verify(input_file).unwrap();
}

#[test]
fn midenc_hir_example_with_mixed_bindings_and_var_scoping_enabled() -> DiagResult<()> {
    const SOURCE: &str = "\
public builtin.function @test::spill_nested_scf_if(v0: ptr<element, u8>) -> u32 {
^block1(v0: ptr<element, u8>):
     v2 = hir.ptr_to_int v0 : u32;
     v3 = arith.constant 32 : u32;
     v4 = arith.add v2, v3 : u32 #[overflow = unchecked];
     v5 = hir.int_to_ptr v4 : ptr<element, u128>;
     v6 = hir.load v5 : u128;
     v7 = arith.constant 64 : u32;
     v8 = arith.add v2, v7 : u32 #[overflow = unchecked];
     v9 = hir.int_to_ptr v8 : ptr<element, u128>;
     v10 = hir.load v9 : u128;
     v11 = arith.constant 1 : u64;
     hir.store_local v5 #[local = lv0];
     v12 = hir.exec @test/example(v9, v6, v10, v10, v11) : u32
     v13 = arith.constant 0 : u32;
     v14 = arith.eq v12, v13 : i1;
     v15 = scf.if v14 : u32 {
     ^block3:
         v18 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v18, v4;
         scf.yield v12;
     } else {
     ^block4:
         v19 = hir.load_local  : ptr<element, u128> #[local = lv0];
         hir.store v19, v4;
         scf.yield v12;
     };
     cf.br ^block2(v15);
 ^block2(v1: u32):
     builtin.ret v1;
};
";
    const CHECKS: &str = r#"; COM: Spill of v5
; CHECK:     hir.store_local v5 #[local = [[$L:lv[0-9]+]]];
; CHECK-NEXT:     {{v[0-9]+}} = hir.exec @test/example

; COM: First reload of v5
; CHECK-LABEL:     ^block3
; CHECK:         [[R1:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[$L]]];
; CHECK-NEXT:         hir.store [[R1]], v[[#]]

; COM: Second reload of v5
; CHECK-LABEL:     ^block4
; CHECK:         [[R2:v[0-9]+]] = hir.load_local  : ptr<element, u128> #[local = [[$L]]];
; CHECK-NEXT:         hir.store [[R2]], v[[#]]
"#;

    let config = Config {
        options: Options {
            enable_var_scope: true,
            ..Options::default()
        },
        ..Config::default()
    };
    let match_file = source_file!(config, CHECKS);
    let input_file = source_file!(config, SOURCE);
    let mut test = Test::new(match_file, &config);
    test.verify(input_file)?;
    Ok(())
}
