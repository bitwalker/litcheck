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

/// Regression test: diagnostics which reference a location in the *input* file must be
/// rendered against the input file, not the check file.
///
/// Rules are applied through `Context::protect`, which previously initialized the guard's
/// `input_file` from the match file, so every input-file span was resolved against the
/// check file's source, producing garbled (or entirely absent) snippets.
#[test]
fn integration_test_input_file_diagnostics_use_input_source() {
    const CHECKS: &str = "CHECK: entry:\nCHECK-SAME: WRONGLINE\n";
    const INPUT: &str = "define void @foo() {\nentry:\n  WRONGLINE is over here\n  ret void\n}\n";

    let config = Config::default();
    let match_file = source_file!(config, CHECKS);
    let input_file = source_file!(config, INPUT);
    let input_id = input_file.id();

    let mut test = Test::new(match_file.clone(), &config);
    let error = test
        .verify(input_file)
        .unwrap_err()
        .downcast::<TestFailed>()
        .unwrap();

    match error.errors() {
        [
            CheckFailedError::MatchFoundButWrongLine {
                span, input_file, ..
            },
        ] => {
            assert_eq!(
                span.source_id(),
                input_id,
                "match span should refer to the input file"
            );
            assert_eq!(
                input_file.id(),
                input_id,
                "the source rendered for this diagnostic should be the input file"
            );
            // The span must actually name the matched text within that source.
            assert_eq!(
                input_file.source_slice(*span),
                Some("WRONGLINE"),
                "span must resolve to the matched text in the rendered source"
            );
        }
        errors => panic!("unexpected errors: {errors:#?}"),
    }
}

/// A `CHECK-LABEL` that is not followed by any other directive is valid, and must be
/// checked rather than silently dropped.
///
/// `compile_lines` only flushed a pending label into the block list when the block body
/// was non-empty, so a label with no body was overwritten by the next label (or dropped
/// at EOF). That left the compiled program with zero sections, and `check_blocks` then
/// indexed `program.sections[0]` unconditionally and panicked.
#[test]
fn integration_test_check_label_without_body() -> DiagResult<()> {
    let config = Config::default();

    // A lone CHECK-LABEL, matching.
    let match_file = source_file!(config, "CHECK-LABEL: alpha\n");
    let input_file = source_file!(config, "alpha\nbeta\n");
    let matches = Test::new(match_file, &config).verify(input_file)?;
    assert_eq!(matches.len(), 1, "the label itself is a positive check");

    // Two adjacent CHECK-LABELs, both matching. Neither may be dropped.
    let match_file = source_file!(config, "CHECK-LABEL: alpha\nCHECK-LABEL: gamma\n");
    let input_file = source_file!(config, "alpha\nbeta\ngamma\n");
    let matches = Test::new(match_file, &config).verify(input_file)?;
    assert_eq!(matches.len(), 2, "both labels must be checked");

    Ok(())
}

/// The second of two adjacent `CHECK-LABEL`s must still be reported as unmatched, rather
/// than being dropped during compilation and silently passing.
#[test]
fn integration_test_check_label_without_body_reports_failure() {
    let config = Config::default();
    let match_file = source_file!(config, "CHECK-LABEL: alpha\nCHECK-LABEL: absent\n");
    let input_file = source_file!(config, "alpha\nbeta\n");

    let error = Test::new(match_file, &config)
        .verify(input_file)
        .unwrap_err()
        .downcast::<TestFailed>()
        .unwrap();

    assert_matches!(
        error.errors(),
        [CheckFailedError::MatchNoneButExpected { .. }]
    );
}

mod searched_region {
    use super::*;
    use filecheck::{Dump, SearchedRegion};

    fn failures(checks: &str, input: &str, dump_input: Dump) -> Vec<CheckFailedError> {
        let config = Config {
            options: Options {
                dump_input,
                ..Options::default()
            },
            ..Config::default()
        };
        let match_file = source_file!(config, checks);
        let input_file = source_file!(config, input);
        Test::new(match_file, &config)
            .verify(input_file)
            .unwrap_err()
            .downcast::<TestFailed>()
            .unwrap()
            .errors
    }

    fn regions(error: &CheckFailedError) -> &[SearchedRegion] {
        match error {
            CheckFailedError::MatchNoneButExpected { searched, .. } => searched,
            other => panic!("expected MatchNoneButExpected, got: {other:#?}"),
        }
    }

    /// A region small enough to display is reported as a single marker covering the whole
    /// region, so the text that was actually searched is visible.
    #[test]
    fn small_region_is_reported_in_full() {
        let input = "alpha\nbeta\ngamma\n";
        let errors = failures("CHECK: nope\n", input, Dump::Fail);

        match regions(&errors[0]) {
            [
                SearchedRegion::Marker {
                    span, input_file, ..
                },
            ] => {
                assert_eq!(
                    input_file.source_slice(*span),
                    Some(input),
                    "the marker should cover exactly the region that was searched"
                );
            }
            other => panic!("expected a single marker, got: {other:#?}"),
        }
    }

    /// A region too large to display is reported by marking its endpoints, so that a failed
    /// match against a large input does not print every line in between.
    #[test]
    fn large_region_is_reported_by_its_endpoints() {
        let input = (0..300).fold(String::new(), |mut acc, i| {
            acc.push_str(&format!("line {i}\n"));
            acc
        });
        let errors = failures("CHECK: nope\n", &input, Dump::Fail);

        match regions(&errors[0]) {
            [
                SearchedRegion::Marker { span: start, .. },
                SearchedRegion::Marker { span: end, .. },
            ] => {
                assert!(start.is_empty(), "endpoints are point spans, not ranges");
                assert!(end.is_empty(), "endpoints are point spans, not ranges");
                assert!(start.start() < end.start());
                // The trailing marker points at the last line, not past the end of input.
                assert!(end.start().to_usize() < input.len());
            }
            other => panic!("expected two endpoint markers, got: {other:#?}"),
        }
    }

    /// Failing against an empty region is worth saying out loud: it means the pattern never
    /// had a chance to match, rather than having been searched for and not found.
    #[test]
    fn empty_region_is_reported_as_a_note() {
        // The CHECK consumes the only line, leaving nothing for the CHECK-NEXT to search.
        let errors = failures("CHECK: alpha\nCHECK-NEXT: beta\n", "alpha\n", Dump::Fail);
        assert_matches!(regions(&errors[0]), [SearchedRegion::Note(_)]);
    }

    /// When several checks fail against the same region, only the first renders it.
    #[test]
    fn repeated_regions_are_reported_once() {
        let errors = failures(
            "CHECK-DAG: absent_one\nCHECK-DAG: absent_two\nCHECK-DAG: absent_three\n",
            "alpha\nbeta\ngamma\n",
            Dump::Fail,
        );
        assert_eq!(errors.len(), 3);

        assert_matches!(regions(&errors[0]), [SearchedRegion::Marker { .. }]);
        for error in &errors[1..] {
            assert_matches!(regions(error), [SearchedRegion::Note(_)]);
        }
    }

    /// `--dump-input=never` suppresses the annotation entirely.
    #[test]
    fn dump_input_never_suppresses_the_region() {
        let errors = failures("CHECK: nope\n", "alpha\nbeta\n", Dump::Never);
        assert!(
            regions(&errors[0]).is_empty(),
            "no region should be attached when --dump-input=never"
        );
    }
}
