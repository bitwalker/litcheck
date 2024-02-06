use crate::common::*;

/// This matcher always succeeds
#[derive(Debug)]
pub struct AlwaysMatch {
    span: SourceSpan,
}
impl AlwaysMatch {
    pub const fn new(span: SourceSpan) -> Self {
        Self { span }
    }
}
impl MatcherMut for AlwaysMatch {
    #[inline]
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        self.try_match(input, context)
    }
}
impl Matcher for AlwaysMatch {
    #[inline]
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        _context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        Ok(MatchResult::ok(MatchInfo::new(
            input.start()..input.start(),
            self.span,
        )))
    }
}
impl Spanned for AlwaysMatch {
    #[inline]
    fn span(&self) -> SourceSpan {
        self.span
    }
}
