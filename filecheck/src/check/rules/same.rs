use litcheck::diagnostics::{DiagResult, SourceSpan, Spanned};

use crate::check::{
    matchers::{Context, MatchResult, MatcherMut},
    Check, CheckFailedError, MatchType, RelatedCheckError,
};

use super::*;

#[derive(Debug)]
pub struct CheckSame<M> {
    pattern: M,
}
impl<M> CheckSame<M>
where
    M: MatcherMut,
{
    pub fn new(pattern: M) -> Self {
        Self { pattern }
    }
}
impl<M> Rule for CheckSame<M>
where
    M: MatcherMut,
{
    fn kind(&self) -> Check {
        Check::Same
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let input = context.search_line();
        let result = self.pattern.try_match_mut(input, context)?;
        match result {
            MatchResult {
                ty,
                info: Some(info),
            } if ty.is_ok() => {
                let cursor = context.cursor_mut();
                assert!(info.span.offset() < cursor.end_of_line());
                cursor.set_start(info.span.end());
                Ok(MatchResult::new(ty, Some(info)))
            }
            result @ MatchResult { info: Some(_), .. } => Ok(result),
            result => {
                // For better diagnostics, extend our search to the end
                // of the block, just in case there is a match, just in the wrong place
                let extended_result = self
                    .pattern
                    .try_match_mut(context.search_block(), context)?;
                if let Some(info) = extended_result.info {
                    Ok(MatchResult {
                        ty: MatchType::Failed(CheckFailedError::MatchFoundButWrongLine {
                            span: info.span,
                            input_file: context.input_file(),
                            pattern: Some(RelatedCheckError {
                                span: self.pattern.span(),
                                match_file: context.match_file(),
                            }),
                        }),
                        info: Some(info),
                    })
                } else {
                    Ok(result)
                }
            }
        }
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
    fn check_same_test() {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK: tail call
CHECK-SAME: @llvm.atomic.load.add.i64
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
        let pattern = SubstringMatcher::new(Span::new(
            SourceSpan::from(6..14),
            Cow::Borrowed("tail call"),
        ))
        .expect("expected pattern to be valid");
        let rule = CheckPlain::new(pattern);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        let pattern = SubstringMatcher::new(Span::new(
            SourceSpan::from(26..50),
            Cow::Borrowed("@llvm.atomic.load.add.i64"),
        ))
        .expect("expected pattern to be valid");
        let rule = CheckSame::new(pattern);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 64);
        assert_eq!(result.info.as_ref().unwrap().span.len(), 25);
        let input = mctx.search();
        assert_eq!(
            input.as_str(result.info.as_ref().unwrap().matched_range()),
            "@llvm.atomic.load.add.i64"
        );
        assert_eq!(input.buffer()[mctx.cursor().start()], b'.');
    }
}
