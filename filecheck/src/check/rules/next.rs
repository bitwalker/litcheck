use litcheck::diagnostics::{DiagResult, SourceSpan, Spanned};

use crate::check::{
    matchers::{Context, ContextExt, MatchResult, MatcherMut},
    Check, CheckFailedError, MatchType,
};

use super::*;

#[derive(Debug)]
pub struct CheckNext<M> {
    pattern: M,
}
impl<M> CheckNext<M>
where
    M: MatcherMut,
{
    pub fn new(pattern: M) -> Self {
        Self { pattern }
    }
}
impl<M> Rule for CheckNext<M>
where
    M: MatcherMut,
{
    fn kind(&self) -> Check {
        Check::Next
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let cursor = context.cursor();
        let start = cursor.start_of_next_line();
        let block_end = cursor.end();
        let next_eol = cursor.next_newline_from(start).unwrap_or(block_end);
        if start >= block_end {
            // Cannot match, since the search range would
            // overflow the current block.
            return Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file(),
                    note: if cursor.end_of_file() == block_end {
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
        let result = self.pattern.try_match_mut(input, context)?;
        match &result {
            MatchResult {
                ty,
                info: Some(ref info),
            } if ty.is_ok() => {
                context.cursor_mut().set_start(info.span.end());
            }
            _ => (),
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::matchers::*;
    use crate::testing::TestContext;
    use litcheck::diagnostics::Span;
    use std::{assert_matches::assert_matches, borrow::Cow};

    #[test]
    fn check_next_test() {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK: @inc4
CHECK-NEXT: entry:
",
            )
            .with_input(
                "
define void @inc4(i64* %p) {
entry:
        %0 = tail call i64 @llvm.atomic.load.add.i64.p0i64(i64* %p, i64 1)
        ret void
}
",
            );
        let mut mctx = context.match_context();
        let pattern =
            SubstringMatcher::new(Span::new(SourceSpan::from(8..12), Cow::Borrowed("@inc4")))
                .expect("expected pattern to be valid");
        let rule = CheckPlain::new(pattern);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        let pattern =
            SubstringMatcher::new(Span::new(SourceSpan::from(22..30), Cow::Borrowed("entry:")))
                .expect("expected pattern to be valid");
        let rule = CheckNext::new(pattern);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 30);
        assert_eq!(result.info.as_ref().unwrap().span.len(), 6);
        let input = mctx.search();
        assert_eq!(
            input.as_str(result.info.as_ref().unwrap().matched_range()),
            "entry:"
        );
        assert_eq!(input.buffer()[mctx.cursor().start()], b'\n');
    }
}
