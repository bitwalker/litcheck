use litcheck::diagnostics::DiagResult;
use regex::Regex;

use crate::config::BooleanExpr;

use super::*;

#[test]
fn test_script_parse_real() -> DiagResult<()> {
    const SCRIPT: &str = r#"
; RUN: midenc compile --stdout --emit=hir %s | filecheck %s

module test

pub fn fib(u32) -> u32 {
block0(v0: u32):
    v1 = const.u32 0 : u32;
    v2 = const.u32 1 : u32;
    br block1(v1, v2, v1)

block1(v3: u32, v4: u32, v5: u32):
    v6 = lt v5, v0 : i1
    cond_br v6, block2, block3(v3)

block2:
    v7 = add.checked v3, v4 : u32
    v8 = incr.wrapping v5: u32
    br block1(v4, v7, v8)

block3(v9: u32):
    ret v9
}

; CHECK-LABEL: pub fn fib(u32) -> u32 {
; CHECK-NEXT: block0(v0: u32):
; CHECK-NEXT:     v1 = const.u32 0 : u32;
; CHECK-NEXT:     v2 = const.u32 1 : u32;
; CHECK-NEXT:     br block1(v1, v2, v1)
; CHECK-EMPTY:
; CHECK-NEXT: block1(v3: u32, v4: u32, v5: u32):
; CHECK-NEXT:     v6 = lt v5, v0 : i1
; CHECK-NEXT:     cond_br v6, block2, block3(v3)
; CHECK-EMPTY:
; CHECK-NEXT: block2:
; CHECK-NEXT:     v7 = add.checked v3, v4 : u32
; CHECK-NEXT:     v8 = incr.wrapping v5: u32
; CHECK-NEXT:     br block1(v4, v7, v8)
; CHECK-EMPTY:
; CHECK-NEXT: block3(v9: u32):
; CHECK-NEXT:     ret v9
; CHECK-NEXT: }
"#;

    let script = TestScript::parse_str(SCRIPT)?;
    assert_eq!(script.commands.len(), 1);
    assert_eq!(
        script.commands[0].unwrap_run().command,
        "midenc compile --stdout --emit=hir %s | filecheck %s"
    );
    assert!(script.unsupported.is_empty());
    assert!(script.requires.is_empty());
    assert!(script.xfails.is_empty());

    Ok(())
}

#[test]
fn test_script_parse_run_with_continuation() -> DiagResult<()> {
    const SCRIPT: &str = "
; RUN: midenc compile --stdout --emit=ast %s\\
; RUN:        | filecheck --check-prefix=AST %s
; RUN: midenc compile --stdout --emit=hir %s\\
; RUN:        | filecheck --check-prefix=HIR\\
; RUN:          %s

module test

pub fn fib(u32) -> u32 {
block0(v0: u32):
    ret v0
}
";
    let script = TestScript::parse_str(SCRIPT)?;
    assert_eq!(script.commands.len(), 2);
    assert_eq!(
        script.commands[0].unwrap_run().command,
        "midenc compile --stdout --emit=ast %s
        | filecheck --check-prefix=AST %s"
    );
    assert_eq!(
        script.commands[1].unwrap_run().command,
        "midenc compile --stdout --emit=hir %s
        | filecheck --check-prefix=HIR
          %s"
    );
    assert!(script.unsupported.is_empty());
    assert!(script.requires.is_empty());
    assert!(script.xfails.is_empty());

    Ok(())
}

#[test]
fn test_script_parse_define_redefine() -> DiagResult<()> {
    const SCRIPT: &str = "
; RUN: midenc compile --stdout --emit=ast %s | filecheck %s

;; DEFINE: %{foo} =
;; REDEFINE: %{foo} = bar
module test

pub fn fib(u32) -> u32 {
block0(v0: u32):
    ret v0
}
";
    let script = TestScript::parse_str(SCRIPT)?;
    assert_eq!(script.commands.len(), 3);
    let define = script.commands[1].unwrap_sub();
    assert_eq!(define.key, "%{foo}");
    assert_eq!(define.value, "");
    assert!(!define.replace);
    let redefine = script.commands[2].unwrap_sub();
    assert_eq!(redefine.key, "%{foo}");
    assert_eq!(redefine.value, "bar");
    assert!(redefine.replace);
    assert!(script.unsupported.is_empty());
    assert!(script.requires.is_empty());
    assert!(script.xfails.is_empty());

    Ok(())
}

#[test]
fn test_script_parse_unsupported_xfail_requires() -> DiagResult<()> {
    const SCRIPT: &str = "
; RUN: midenc compile --stdout --emit=ast %s | filecheck %s
; REQUIRES: eval && opt-{{.*}}
; UNSUPPORTED: target=miden{{.*}}
; XFAIL: *

module test

pub fn fib(u32) -> u32 {
block0(v0: u32):
    ret v0
}
";
    let script = TestScript::parse_str(SCRIPT)?;
    assert_eq!(script.commands.len(), 1);
    assert_eq!(script.requires.len(), 1);
    assert_eq!(
        script.requires[0],
        BooleanExpr::And(
            Box::new(BooleanExpr::Var("eval".to_string())),
            Box::new(BooleanExpr::Pattern(Regex::new(r"opt\-(.*)").unwrap()))
        )
    );
    assert_eq!(script.unsupported.len(), 1);
    assert_eq!(
        script.unsupported[0],
        BooleanExpr::Pattern(Regex::new("target=miden(.*)").unwrap())
    );
    assert_eq!(script.xfails.len(), 1);
    assert_eq!(script.xfails[0], BooleanExpr::Lit(true));

    Ok(())
}
