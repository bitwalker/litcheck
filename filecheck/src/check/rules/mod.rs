use std::fmt;

use litcheck::diagnostics::{DiagResult, SourceSpan, Spanned};

use crate::check::{
    matchers::{MatchContext, MatchInfo, Matcher},
    Check, CheckFailedError, MatchType, Pattern, PatternSet, RelatedCheckError,
};

pub struct MatchResult<'input> {
    pub ty: MatchType,
    pub info: Option<MatchInfo<'input>>,
}
impl<'input> MatchResult<'input> {
    #[inline(always)]
    pub fn is_ok(&self) -> bool {
        self.ty.is_ok()
    }

    pub fn unwrap_err(self) -> CheckFailedError {
        match self.ty {
            MatchType::Failed(err) => err,
            ty => panic!("attempted to unwrap error from {ty}"),
        }
    }
}

pub trait Rule: fmt::Debug {
    fn kind(&self) -> Check;
    fn span(&self) -> SourceSpan;
    fn apply<'a>(&self, context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>>;
}

#[derive(Debug)]
pub struct CheckEmpty {
    span: SourceSpan,
}
impl CheckEmpty {
    pub fn new(span: SourceSpan) -> Self {
        Self { span }
    }
}
impl Rule for CheckEmpty {
    fn kind(&self) -> Check {
        Check::Empty
    }

    fn span(&self) -> SourceSpan {
        self.span
    }

    fn apply<'a>(&self, context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        // Next line must be empty, meaning the start of the line is the end of the line
        let start = context.start_of_next_line();
        if start < context.block.end && context.is_end_of_line(start) {
            let span = SourceSpan::from(start..start);
            // Move to the start of the empty line
            context.block.start = start;
            Ok(MatchResult {
                ty: MatchType::MatchFoundAndExpected,
                info: Some(MatchInfo::new(span, self.span)),
            })
        } else {
            Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file.clone(),
                    note: None,
                }),
                info: None,
            })
        }
    }
}

#[derive(Debug)]
pub struct CheckPlain<'a> {
    pattern: Pattern<'a>,
}
impl<'a> CheckPlain<'a> {
    pub fn new(pattern: Pattern<'a>) -> Self {
        Self { pattern }
    }
}
impl<'check> Rule for CheckPlain<'check> {
    fn kind(&self) -> Check {
        Check::Plain
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'a>(&self, context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        let input = context.search_block();
        if let Some(info) = self.pattern.try_match(input) {
            context.block.start = info.span.offset() + info.span.len();
            let eol = context
                .next_newline_from(context.block.start)
                .unwrap_or(context.end_of_file());
            context.eol = eol;
            Ok(MatchResult {
                ty: MatchType::MatchFoundAndExpected,
                info: Some(info),
            })
        } else {
            Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file.clone(),
                    note: None,
                }),
                info: None,
            })
        }
    }
}

#[derive(Debug)]
pub struct CheckSame<'a> {
    pattern: Pattern<'a>,
}
impl<'a> CheckSame<'a> {
    pub fn new(pattern: Pattern<'a>) -> Self {
        Self { pattern }
    }
}
impl<'check> Rule for CheckSame<'check> {
    fn kind(&self) -> Check {
        Check::Same
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'a>(&self, context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        let input = context.search_line();
        if let Some(info) = self.pattern.try_match(input) {
            assert!(info.span.offset() < context.eol);
            context.block.start = info.span.offset() + info.span.len();
            Ok(MatchResult {
                ty: MatchType::MatchFoundAndExpected,
                info: Some(info),
            })
        } else {
            // For better diagnostics, extend our search to the end
            // of the block, just in case there is a match, just in the wrong place
            if let Some(info) = self.pattern.try_match(context.search_block()) {
                Ok(MatchResult {
                    ty: MatchType::Failed(CheckFailedError::MatchFoundButWrongLine {
                        span: info.span,
                        input_file: context.input_file.clone(),
                        pattern: Some(RelatedCheckError {
                            span: self.pattern.span(),
                            match_file: context.match_file.clone(),
                        }),
                    }),
                    info: Some(info),
                })
            } else {
                Ok(MatchResult {
                    ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                        span: self.pattern.span(),
                        match_file: context.match_file.clone(),
                        note: None,
                    }),
                    info: None,
                })
            }
        }
    }
}

#[derive(Debug)]
pub struct CheckNext<'a> {
    pattern: Pattern<'a>,
}
impl<'a> CheckNext<'a> {
    pub fn new(pattern: Pattern<'a>) -> Self {
        Self { pattern }
    }
}
impl<'check> Rule for CheckNext<'check> {
    fn kind(&self) -> Check {
        Check::Next
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'a>(&self, context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        let start = context.start_of_next_line();
        let next_eol = context.next_newline_from(start).unwrap_or(context.end());
        let block_end = context.end();
        if start >= block_end {
            // Cannot match, since the search range would
            // overflow the current block.
            return Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file.clone(),
                    note: if context.buffer.len() == block_end {
                        None
                    } else {
                        Some(
                            "search was stopped at end of the preceding CHECK-LABEL scope"
                                .to_string(),
                        )
                    },
                }),
                info: None,
            });
        }
        let input = context.search_range(start..next_eol);
        if let Some(info) = self.pattern.try_match(input) {
            context.block.start = info.span.offset() + info.span.len();
            Ok(MatchResult {
                ty: MatchType::MatchFoundAndExpected,
                info: Some(info),
            })
        } else {
            Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file.clone(),
                    note: None,
                }),
                info: None,
            })
        }
    }
}

#[derive(Debug)]
pub struct CheckDag<'a> {
    patterns: PatternSet<'a>,
}
impl<'a> CheckDag<'a> {
    pub fn new(patterns: PatternSet<'a>) -> Self {
        Self { patterns }
    }
}
impl<'check> Rule for CheckDag<'check> {
    fn kind(&self) -> Check {
        Check::Dag
    }

    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }

    fn apply<'a>(&self, _context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        todo!()
    }
}

#[derive(Debug)]
pub struct CheckNot<'a> {
    patterns: PatternSet<'a>,
}
impl<'a> CheckNot<'a> {
    pub fn new(patterns: PatternSet<'a>) -> Self {
        Self { patterns }
    }
}
impl<'check> Rule for CheckNot<'check> {
    fn kind(&self) -> Check {
        Check::Not
    }

    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }

    fn apply<'a>(&self, _context: &mut MatchContext<'a>) -> DiagResult<MatchResult<'a>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{check::matchers::*, Config};
    use litcheck::{
        diagnostics::{ArcSource, Span},
        StringInterner,
    };
    use std::assert_matches::assert_matches;
    use std::borrow::Cow;

    #[test]
    fn check_empty_test() {
        let mut test = TestContext::new(
            "
abc

def",
            "CHECK-EMPTY:",
        );
        let mut context = test.match_context();
        let rule = CheckEmpty::new(SourceSpan::from(0..12));
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(!result.is_ok());
        assert_matches!(
            result.ty,
            MatchType::Failed(CheckFailedError::MatchNoneButExpected { .. })
        );

        // Move to the 'abc' line
        context.consume_newline();

        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 5);
        assert_eq!(context.buffer[4], b'\n');
        assert_eq!(context.buffer[5], b'\n');
        assert_eq!(context.buffer[6], b'd');
        assert_eq!(result.info.as_ref().unwrap().span.len(), 0);
    }

    #[test]
    fn check_plain_test() {
        let mut test = TestContext::new(
            "
define void @sub1(i32* %p, i32 %v) {
entry:
        %0 = tail call i32 @llvm.atomic.load.sub.i32.p0i32(i32* %p, i32 %v)
        ret void
}

define void @inc4(i64* %p) {
entry:
        %0 = tail call i64 @llvm.atomic.load.add.i64.p0i64(i64* %p, i64 1)
        ret void
}
",
            "CHECK: @inc4",
        );
        let mut context = test.match_context();
        let pattern = Pattern::Substring(
            SubstringMatcher::new(Span::new(SourceSpan::from(8..12), Cow::Borrowed("@inc4")))
                .expect("expected pattern to be valid"),
        );
        let rule = CheckPlain::new(pattern);
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 153);
        assert_eq!(result.info.as_ref().unwrap().span.len(), 5);
        let input = context.search();
        assert_eq!(
            input.as_str(result.info.as_ref().unwrap().matched_range()),
            "@inc4"
        );
        assert_eq!(input.buffer()[context.block.start], b'(');
    }

    #[test]
    fn check_next_test() {
        let mut test = TestContext::new(
            "
define void @inc4(i64* %p) {
entry:
        %0 = tail call i64 @llvm.atomic.load.add.i64.p0i64(i64* %p, i64 1)
        ret void
}
",
            "
CHECK: @inc4
CHECK-NEXT: entry:
",
        );
        let mut context = test.match_context();
        let pattern = Pattern::Substring(
            SubstringMatcher::new(Span::new(SourceSpan::from(8..12), Cow::Borrowed("@inc4")))
                .expect("expected pattern to be valid"),
        );
        let rule = CheckPlain::new(pattern);
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        let pattern = Pattern::Substring(
            SubstringMatcher::new(Span::new(SourceSpan::from(22..30), Cow::Borrowed("entry:")))
                .expect("expected pattern to be valid"),
        );
        let rule = CheckNext::new(pattern);
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 30);
        assert_eq!(result.info.as_ref().unwrap().span.len(), 6);
        let input = context.search();
        assert_eq!(
            input.as_str(result.info.as_ref().unwrap().matched_range()),
            "entry:"
        );
        assert_eq!(input.buffer()[context.block.start], b'\n');
    }

    #[test]
    fn check_same_test() {
        let mut test = TestContext::new(
            "
define void @inc4(i64* %p) {
entry:
        %0 = tail call i64 @llvm.atomic.load.add.i64.p0i64(i64* %p, i64 1)
        ret void
}
",
            "
CHECK: tail call
CHECK-SAME: @llvm.atomic.load.add.i64
",
        );
        let mut context = test.match_context();
        let pattern = Pattern::Substring(
            SubstringMatcher::new(Span::new(
                SourceSpan::from(6..14),
                Cow::Borrowed("tail call"),
            ))
            .expect("expected pattern to be valid"),
        );
        let rule = CheckPlain::new(pattern);
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        let pattern = Pattern::Substring(
            SubstringMatcher::new(Span::new(
                SourceSpan::from(26..50),
                Cow::Borrowed("@llvm.atomic.load.add.i64"),
            ))
            .expect("expected pattern to be valid"),
        );
        let rule = CheckSame::new(pattern);
        let result = rule
            .apply(&mut context)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 64);
        assert_eq!(result.info.as_ref().unwrap().span.len(), 25);
        let input = context.search();
        assert_eq!(
            input.as_str(result.info.as_ref().unwrap().matched_range()),
            "@llvm.atomic.load.add.i64"
        );
        assert_eq!(input.buffer()[context.block.start], b'.');
    }

    #[test]
    #[ignore]
    fn check_dag_test() {
        todo!()
    }

    #[test]
    #[ignore]
    fn check_not_test() {
        todo!()
    }

    #[test]
    #[ignore]
    fn check_dag_not_reordering_test() {
        todo!()
    }

    struct TestContext {
        config: Config,
        interner: StringInterner,
        input_file: ArcSource,
        match_file: ArcSource,
    }
    impl TestContext {
        pub fn new<S, R>(input: S, rules: R) -> Self
        where
            ArcSource: From<S>,
            ArcSource: From<R>,
        {
            Self {
                config: Config {
                    check_prefixes: vec!["CHECK".to_string().into_boxed_str()],
                    comment_prefixes: vec![
                        "COM".to_string().into_boxed_str(),
                        "RUN".to_string().into_boxed_str(),
                    ],
                    allow_unused_prefixes: true,
                    strict_whitespace: false,
                    match_full_lines: false,
                    ignore_case: false,
                    implicit_check_not: vec![],
                    dump_input: Default::default(),
                    dump_input_filter: Default::default(),
                    enable_var_scope: true,
                    variables: vec![],
                    verbose: 0,
                    color: Default::default(),
                },
                interner: StringInterner::new(),
                input_file: ArcSource::from(input),
                match_file: ArcSource::from(rules),
            }
        }

        pub fn match_context(&mut self) -> MatchContext<'_> {
            MatchContext::new(
                &self.config,
                &mut self.interner,
                self.match_file.clone(),
                self.input_file.clone(),
                self.input_file.as_bytes(),
            )
        }
    }
}
