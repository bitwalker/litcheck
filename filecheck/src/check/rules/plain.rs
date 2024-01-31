use litcheck::diagnostics::{DiagResult, SourceSpan, Spanned};

use crate::check::{
    matchers::{Context, MatchResult, MatcherMut, Matches},
    Check,
};

use super::*;

#[derive(Debug)]
pub struct CheckPlain<M> {
    pattern: M,
}
impl<M> CheckPlain<M>
where
    M: MatcherMut,
{
    pub fn new(pattern: M) -> Self {
        Self { pattern }
    }
}
impl<M> Rule for CheckPlain<M>
where
    M: MatcherMut,
{
    fn kind(&self) -> Check {
        Check::Plain
    }

    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let input = context.search_block();
        let result = self.pattern.try_match_mut(input, context)?;

        match &result {
            MatchResult {
                ty,
                info: Some(info),
            } if ty.is_ok() => {
                context.cursor_mut().set_start(info.span.end());
            }
            _ => (),
        }
        Ok(result.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::{matchers::*, TestResult};
    use crate::testing::TestContext;
    use litcheck::diagnostics::{DiagResult, Span};
    use std::borrow::Cow;

    #[test]
    fn check_plain_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context.with_checks("CHECK: @inc4").with_input(
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
        );
        let mut mctx = context.match_context();
        let pattern =
            SubstringMatcher::new(Span::new(SourceSpan::from(8..12), Cow::Borrowed("@inc4")))
                .expect("expected pattern to be valid");
        let rule = CheckPlain::new(pattern);
        let matches = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        let matched = TestResult::from_matches(matches, &mctx).into_result()?;
        assert_eq!(matched[0].span.offset(), 153);
        assert_eq!(matched[0].span.len(), 5);
        let input = mctx.search();
        assert_eq!(input.as_str(matched[0].matched_range()), "@inc4");
        assert_eq!(input.buffer()[mctx.cursor().start()], b'(');

        Ok(())
    }
}
