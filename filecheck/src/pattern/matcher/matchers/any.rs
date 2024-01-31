use crate::common::*;

use super::{RegexSetMatcher, SubstringSetMatcher};

#[derive(Debug)]
pub enum MatchAny<'a> {
    /// Match a set of literal strings
    Literal(SubstringSetMatcher<'a>),
    /// Match a set of static regex patterns
    Regex(RegexSetMatcher<'a>),
    ///// Match a set of hybrid patterns
    //Smart(SmartMatcherSet<'a>),
}
impl<'a> Spanned for MatchAny<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Literal(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            //Self::Smart(ref matcher) => matcher.span(),
        }
    }
}
impl<'a> MatcherMut for MatchAny<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        match self {
            Self::Literal(ref matcher) => matcher.try_match_mut(input, context),
            Self::Regex(ref matcher) => matcher.try_match_mut(input, context),
        }
    }
}
