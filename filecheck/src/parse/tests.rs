use pretty_assertions::assert_eq;

use litcheck::assert_matches;

use crate::{
    ast::*,
    common::*,
    parse::{ParserError, Token},
    source_file,
};

macro_rules! literal {
    ($s:literal) => {
        CheckPattern::Literal(Span::new(SourceSpan::UNKNOWN, Cow::Borrowed($s)))
    };

    ($s:expr) => {
        CheckPattern::Literal(Span::new(SourceSpan::UNKNOWN, $s.into()))
    };
}

macro_rules! regex {
    ($s:literal) => {
        CheckPattern::Regex(RegexPattern::new(Span::new(
            SourceSpan::UNKNOWN,
            Cow::Borrowed($s),
        )))
    };

    ($s:expr) => {
        CheckPattern::Regex(RegexPattern::new(Span::new(SourceSpan::UNKNOWN, $s.into())))
    };
}

macro_rules! literal_part {
    ($s:literal) => {
        CheckPatternPart::Literal(Span::new(SourceSpan::UNKNOWN, Cow::Borrowed($s)))
    };

    ($s:expr) => {
        CheckPatternPart::Literal(Span::new(SourceSpan::UNKNOWN, $s.into()))
    };
}

#[test]
fn check_line_expression_lexer_test() -> DiagResult<()> {
    let input = r#"
// RUN: gcc %s -o %t && %t \
// RUN:    | filecheck %s

#include <stdio.h>
int main() {
  // CHECK: Hello from line [[# @LINE + 1 ]]
  printf("Hello from line %d\n", __LINE__);
  return 0;
}
"#;
    let context = TestContext::new();
    let input = source_file!(context.config, input);

    let mut lexer = context.lex(&input);

    assert_eq!(
        lexer.next()?,
        Some(Token::Comment(Cow::Borrowed("gcc %s -o %t && %t \\")))
    );
    assert_eq!(lexer.next()?, Some(Token::Lf));
    assert_eq!(
        lexer.next()?,
        Some(Token::Comment(Cow::Borrowed("   | filecheck %s")))
    );
    assert_eq!(lexer.next()?, Some(Token::Lf));
    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("Hello from line ")));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_eq!(lexer.next()?, Some(Token::At));
    assert_eq!(lexer.next()?, Some(Token::Line));
    assert_eq!(lexer.next()?, Some(Token::Plus));
    assert_eq!(lexer.next()?, Some(Token::Num(1)));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_line_expression_parser_test() {
    let input = r#"
// RUN: gcc %s -o %t && %t \
// RUN:    | filecheck %s

#include <stdio.h>
int main() {
  // CHECK: Hello from line [[# @LINE + 1 ]]
  printf("Hello from line %d\n", __LINE__);
  return 0;
}
"#;
    let mut context = TestContext::new();
    let input = source_file!(context.config, input);

    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 1);

    let check_line = &lines[0];
    let pattern_parts = vec![
        literal_part!("Hello from line "),
        CheckPatternPart::Match(Match::Numeric {
            span: SourceSpan::UNKNOWN,
            format: Default::default(),
            capture: None,
            constraint: Constraint::Eq,
            expr: Some(Expr::Binary {
                span: SourceSpan::UNKNOWN,
                op: BinaryOp::Add,
                lhs: Box::new(Expr::Var(VariableName::Pseudo(Span::new(
                    SourceSpan::UNKNOWN,
                    symbols::Line,
                )))),
                rhs: Box::new(Expr::Num(Number::new(SourceSpan::UNKNOWN, 1))),
            }),
        }),
    ];
    let pattern = CheckPattern::Match(Span::new(SourceSpan::UNKNOWN, pattern_parts));
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
        pattern,
    )
    .with_comment(Cow::Borrowed("gcc %s -o %t && %t \\"))
    .with_comment(Cow::Borrowed("   | filecheck %s"));
    assert_eq!(check_line, &expected);
}

#[test]
fn check_literal_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: main
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("main")));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_literal_parser_test() {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: main
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 1);

    let check_line = &lines[0];
    assert_eq!(
        check_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
            literal!("main"),
        )
    );
}

#[test]
fn check_regex_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: {{main.*[[:space:]]i32[[:space:]]}}
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::RegexStart));
    assert_eq!(
        lexer.next()?,
        Some(Token::Raw("main.*[[:space:]]i32[[:space:]]"))
    );
    assert_eq!(lexer.next()?, Some(Token::RegexEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_regex_parser_test() {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: {{main.*[[:space:]]i32[[:space:]]}}
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 1);

    let check_line = &lines[0];
    assert_eq!(
        check_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
            regex!("main.*[[:space:]]i32[[:space:]]")
        )
    );
}

#[test]
fn check_capture_and_substitution_lexer_test() -> DiagResult<()> {
    let input = r#"
fn foo() -> i32 { return 0; }

fn main() -> i32 { foo(); }

// CHECK: [[FUN:fn [a-z][a-zA-Z0-9_]+]]
// CHECK: [[FUN]](
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Ident("FUN")));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("fn [a-z][a-zA-Z0-9_]+")));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Ident("FUN")));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Raw("(")));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_capture_and_substitution_parser_test() {
    let input = r#"
fn foo() -> i32 { return 0; }

fn main() -> i32 { foo(); }

// CHECK: [[FUN:fn [a-z][a-zA-Z0-9_]+]]
// CHECK: [[FUN]](
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 2);

    let fun = Symbol::intern("FUN");
    let capture_line = &lines[0];
    assert_eq!(
        capture_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
            CheckPattern::Match(Span::new(
                SourceSpan::UNKNOWN,
                vec![CheckPatternPart::Match(Match::Substitution {
                    span: SourceSpan::UNKNOWN,
                    name: VariableName::User(Span::new(SourceSpan::UNKNOWN, fun)),
                    pattern: Some(Span::new(
                        SourceSpan::UNKNOWN,
                        Cow::Borrowed("fn [a-z][a-zA-Z0-9_]+")
                    )),
                })]
            ))
        )
    );

    let substitute_line = &lines[1];
    assert_eq!(
        substitute_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
            CheckPattern::Match(Span::new(
                SourceSpan::UNKNOWN,
                vec![
                    CheckPatternPart::Match(Match::Substitution {
                        span: SourceSpan::UNKNOWN,
                        name: VariableName::User(Span::new(SourceSpan::UNKNOWN, fun)),
                        pattern: None,
                    }),
                    literal_part!("("),
                ]
            ))
        )
    );
}

#[test]
fn check_numeric_capture_and_substitution_lexer_test() -> DiagResult<()> {
    let input = r#"
fn foo() -> i32 { return 0; }
fn main() -> i64 { foo(); }

// CHECK: i[[#BITS:]]
// CHECK-NEXT: i[[#mul(BITS, 2)]]
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("i")));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_eq!(lexer.next()?, Some(Token::Ident("BITS")));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Next)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("i")));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_eq!(lexer.next()?, Some(Token::Mul));
    assert_eq!(lexer.next()?, Some(Token::LParen));
    assert_eq!(lexer.next()?, Some(Token::Ident("BITS")));
    assert_eq!(lexer.next()?, Some(Token::Comma));
    assert_eq!(lexer.next()?, Some(Token::Num(2)));
    assert_eq!(lexer.next()?, Some(Token::RParen));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_numeric_capture_and_substitution_parser_test() {
    let input = r#"
fn foo() -> i32 { return 0; }
fn main() -> i64 { foo(); }

// CHECK: i[[#BITS:]]
// CHECK-NEXT: i[[#mul(BITS, 2)]]
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 2);

    let bits = Symbol::intern("BITS");
    let capture_line = &lines[0];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
        CheckPattern::Match(Span::new(
            SourceSpan::UNKNOWN,
            vec![
                literal_part!("i"),
                CheckPatternPart::Match(Match::Numeric {
                    span: SourceSpan::UNKNOWN,
                    format: None,
                    capture: Some(VariableName::User(Span::new(SourceSpan::UNKNOWN, bits))),
                    constraint: Constraint::Eq,
                    expr: None,
                }),
            ],
        )),
    );

    assert_eq!(capture_line, &expected);

    let substitute_line = &lines[1];
    assert_eq!(
        substitute_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Next),
            CheckPattern::Match(Span::new(
                SourceSpan::UNKNOWN,
                vec![
                    literal_part!("i"),
                    CheckPatternPart::Match(Match::Numeric {
                        span: SourceSpan::UNKNOWN,
                        format: None,
                        capture: None,
                        constraint: Constraint::Eq,
                        expr: Some(Expr::Binary {
                            span: SourceSpan::UNKNOWN,
                            op: BinaryOp::Mul,
                            lhs: Box::new(Expr::Var(VariableName::User(Span::new(
                                SourceSpan::UNKNOWN,
                                bits
                            )))),
                            rhs: Box::new(Expr::Num(Number::new(SourceSpan::UNKNOWN, 2))),
                        })
                    }),
                ]
            ))
        )
    );
}

#[test]
fn check_numeric_capture_and_substitution_with_formatting_lexer_test() -> DiagResult<()> {
    let input = r#"
fn foo() -> i32 { return 0; }
fn main() -> i64 { foo(); }

// CHECK: i[[#%.2a,BITS:]]
// CHECK-NEXT: i[[#mul(BITS, 2)]]
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("i")));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_eq!(lexer.next()?, Some(Token::Percent));
    assert_eq!(lexer.next()?, Some(Token::Dot));
    assert_eq!(lexer.next()?, Some(Token::Num(2)));
    assert_eq!(lexer.next()?, Some(Token::Ident("a")));
    assert_eq!(lexer.next()?, Some(Token::Comma));
    assert_eq!(lexer.next()?, Some(Token::Ident("BITS")));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Next)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("i")));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_eq!(lexer.next()?, Some(Token::Mul));
    assert_eq!(lexer.next()?, Some(Token::LParen));
    assert_eq!(lexer.next()?, Some(Token::Ident("BITS")));
    assert_eq!(lexer.next()?, Some(Token::Comma));
    assert_eq!(lexer.next()?, Some(Token::Num(2)));
    assert_eq!(lexer.next()?, Some(Token::RParen));
    assert_eq!(lexer.next()?, Some(Token::MatchEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_numeric_capture_and_substitution_with_formatting_parser_test() {
    let input = r#"
fn foo() -> i32 { return 0; }
fn main() -> i64 { foo(); }

// CHECK: i[[#%.2u,BITS:]]
// CHECK-NEXT: i[[#%.2u,mul(BITS, 2)]]
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 2);

    let bits = Symbol::intern("BITS");
    let capture_line = &lines[0];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
        CheckPattern::Match(Span::new(
            SourceSpan::UNKNOWN,
            vec![
                literal_part!("i"),
                CheckPatternPart::Match(Match::Numeric {
                    span: SourceSpan::UNKNOWN,
                    format: Some(NumberFormat::Unsigned { precision: 2 }),
                    capture: Some(VariableName::User(Span::new(SourceSpan::UNKNOWN, bits))),
                    constraint: Constraint::Eq,
                    expr: None,
                }),
            ],
        )),
    );

    assert_eq!(capture_line, &expected);

    let substitute_line = &lines[1];
    assert_eq!(
        substitute_line,
        &CheckLine::new(
            SourceSpan::UNKNOWN,
            CheckType::new(SourceSpan::UNKNOWN, Check::Next),
            CheckPattern::Match(Span::new(
                SourceSpan::UNKNOWN,
                vec![
                    literal_part!("i"),
                    CheckPatternPart::Match(Match::Numeric {
                        span: SourceSpan::UNKNOWN,
                        format: Some(NumberFormat::Unsigned { precision: 2 }),
                        capture: None,
                        constraint: Constraint::Eq,
                        expr: Some(Expr::Binary {
                            span: SourceSpan::UNKNOWN,
                            op: BinaryOp::Mul,
                            lhs: Box::new(Expr::Var(VariableName::User(Span::new(
                                SourceSpan::UNKNOWN,
                                bits,
                            )))),
                            rhs: Box::new(Expr::Num(Number::new(SourceSpan::UNKNOWN, 2))),
                        })
                    }),
                ]
            ))
        )
    );
}

#[test]
fn check_with_modifiers_lexer_test() -> DiagResult<()> {
    let input = r#"
Input: [[[10, 20]], [[30, 40]]]
Output %r10: [[10, 20]]
Output %r10: [[30, 40]]

; CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]
; CHECK-DAG{LITERAL}: [[30, 40]]
; CHECK-DAG{LITERAL}: [[10, 20]]
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);
    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Modifier(CheckModifier::LITERAL)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("[[[10, 20]], [[30, 40]]]")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Modifier(CheckModifier::LITERAL)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("[[30, 40]]")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Modifier(CheckModifier::LITERAL)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("[[10, 20]]")));
    Ok(())
}

#[test]
fn check_with_modifiers_parser_test() {
    let input = r#"
Input: [[[10, 20]], [[30, 40]]]
Output %r10: [[10, 20]]
Output %r10: [[30, 40]]

; CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]
; CHECK-DAG{LITERAL}: [[30, 40]]
; CHECK-DAG{LITERAL}: [[10, 20]]
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 3);

    let line = &lines[0];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Plain)
            .with_modifiers(Span::new(SourceSpan::UNKNOWN, CheckModifier::LITERAL)),
        literal!("[[[10, 20]], [[30, 40]]]"),
    );
    assert_eq!(line, &expected);

    let line = &lines[1];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Dag)
            .with_modifiers(Span::new(SourceSpan::UNKNOWN, CheckModifier::LITERAL)),
        literal!("[[30, 40]]"),
    );
    assert_eq!(line, &expected);

    let line = &lines[2];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Dag)
            .with_modifiers(Span::new(SourceSpan::UNKNOWN, CheckModifier::LITERAL)),
        literal!("[[10, 20]]"),
    );
    assert_eq!(line, &expected);
}

#[test]
fn check_comment_lexer_test() -> DiagResult<()> {
    let input = r#"
Input: [[[10, 20]], [[30, 40]]]
Output %r10: [[10, 20]]
Output %r10: [[30, 40]]

; COM: CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]
; CHECK-DAG{LITERAL}: [[30, 40]]
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(
        lexer.next()?,
        Some(Token::Comment(Cow::Borrowed(
            "CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]"
        )))
    );
    assert_eq!(lexer.next()?, Some(Token::Lf));
    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Modifier(CheckModifier::LITERAL)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("[[30, 40]]")));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_comment_parser_test() {
    let input = r#"
Input: [[[10, 20]], [[30, 40]]]
Output %r10: [[10, 20]]
Output %r10: [[30, 40]]

; COM: CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]
; CHECK-DAG{LITERAL}: [[30, 40]]
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 1);

    let line = &lines[0];
    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Dag)
            .with_modifiers(Span::new(SourceSpan::UNKNOWN, CheckModifier::LITERAL)),
        literal!("[[30, 40]]"),
    )
    .with_comment(Cow::Borrowed("CHECK{LITERAL}: [[[10, 20]], [[30, 40]]]"));
    assert_eq!(line, &expected);
}

#[test]
fn check_like_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECKS: main
// COM-CHECK: main
// COM_CHECK: main
// COMCHECK: main
// CHECK-COM: main
// CHECK_COM: main
// CHECKCOM: main
// COM: main
// CHECK: main
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Comment("main".into())));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("main")));
    assert_eq!(lexer.next()?, Some(Token::Lf));
    Ok(())
}

#[test]
fn check_like_parser_test() {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECKS: main
// COM-CHECK: main
// COM_CHECK: main
// COMCHECK: main
// CHECK-COM: main
// CHECK_COM: main
// CHECKCOM: main
"#;
    let mut context = TestContext::new();

    let input = source_file!(context.config, input);
    assert_matches!(
        context.parse_err(&input),
        Err(ParserError::UnusedCheckPrefixes(_))
    );
}

#[test]
fn comment_like_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// COM-CHECK: CHECK: main
// COM_CHECK: CHECK: main
// COMCHECK: CHECK: main
// CHECK-COM: CHECK: main
// CHECK_COM: CHECK: main
// CHECKCOM: CHECK: main
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    for _ in 0..6 {
        assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
        assert_eq!(lexer.next()?, Some(Token::Colon));
        assert_eq!(lexer.next()?, Some(Token::Raw("main")));
        assert_eq!(lexer.next()?, Some(Token::Lf));
    }

    assert_eq!(lexer.next()?, None);
    Ok(())
}

#[test]
fn check_blocks_lexer_test() -> DiagResult<()> {
    let input = r"
CHECK-LABEL: block0(
CHECK-DAG: v1 = const.i32 1
CHECK-DAG: v2 = const.i32 0
CHECK-LABEL: block1(
CHECK-DAG: v8 = add v6, v7
";
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Label)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("block0(")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("v1 = const.i32 1")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("v2 = const.i32 0")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Label)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("block1(")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Dag)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("v8 = add v6, v7")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, None);
    Ok(())
}

#[test]
fn check_count_test() -> DiagResult<()> {
    let input = r"
Loop 1
lOop 2
loOp 3
looP 4
loop 5
LOOP 6
BREAK

# CNT-COUNT-6: LOop {{[0-9]}}
# CNT-NOT: loop
# CNT-NEXT: break
";
    let mut context = TestContext::new();
    context.config.options.check_prefixes = vec![Symbol::intern("CNT")];

    let input = source_file!(context.config, input);
    let mut lexer = context.lex(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Count(6))));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("LOop ")));
    assert_eq!(lexer.next()?, Some(Token::RegexStart));
    assert_eq!(lexer.next()?, Some(Token::Raw("[0-9]")));
    assert_eq!(lexer.next()?, Some(Token::RegexEnd));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Not)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("loop")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Next)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::Raw("break")));
    assert_eq!(lexer.next()?, Some(Token::Lf));

    assert_eq!(lexer.next()?, None);
    Ok(())
}

#[test]
fn check_incomplete_match_block_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: [[
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex_with_errors(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_matches!(lexer.next(), Err(ParserError::UnclosedSubstitution { .. }));
    Ok(())
}

#[test]
fn check_incomplete_capture_block_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: [[NUM:
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex_with_errors(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_matches!(lexer.next(), Err(ParserError::UnclosedSubstitution { .. }));
    Ok(())
}

#[test]
fn check_incomplete_invalid_match_numeric_block_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: [[#%8.a,NUM
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex_with_errors(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::MatchStart));
    assert_eq!(lexer.next()?, Some(Token::Hash));
    assert_matches!(lexer.next(), Err(ParserError::UnclosedSubstitution { .. }));
    Ok(())
}

#[test]
fn check_incomplete_regex_block_lexer_test() -> DiagResult<()> {
    let input = r#"
fn main() -> i32 { std::process::exit(); }

// CHECK: {{.*
"#;
    let context = TestContext::new();

    let input = source_file!(context.config, input);
    let mut lexer = context.lex_with_errors(&input);

    assert_eq!(lexer.next()?, Some(Token::Check(Check::Plain)));
    assert_eq!(lexer.next()?, Some(Token::Colon));
    assert_eq!(lexer.next()?, Some(Token::RegexStart));
    assert_matches!(lexer.next(), Err(ParserError::UnclosedRegex { .. }));

    Ok(())
}

#[test]
fn parser_sanity_test() -> DiagResult<()> {
    const INPUT: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../tests/filecheck/sanity/check.txt"
    ));

    let mut context = TestContext::new();

    let input = source_file!(context.config, INPUT);
    let file = context.parse(&input).unwrap();

    let lines = file.lines();
    assert_eq!(lines.len(), 6);

    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Label),
        literal!("Some random"),
    )
    .with_comment(Cow::Borrowed(
        "awk '/^;/ { next }; /.*/ { print }' %s | filecheck %s",
    ));
    assert_eq!(&lines[0], &expected);

    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Empty),
        CheckPattern::Empty(SourceSpan::UNKNOWN),
    );
    assert_eq!(&lines[1], &expected);
    assert_eq!(&lines[2], &expected);

    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Plain),
        literal!("content to"),
    );
    assert_eq!(&lines[3], &expected);

    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Same),
        literal!("show output"),
    );
    assert_eq!(&lines[4], &expected);

    let expected = CheckLine::new(
        SourceSpan::UNKNOWN,
        CheckType::new(SourceSpan::UNKNOWN, Check::Next),
        literal!("and some rules"),
    );
    assert_eq!(&lines[5], &expected);

    Ok(())
}
