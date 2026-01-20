use crate::common::*;

use super::{RegexMatcher, SubstringMatcher};

/// A matcher which has no dynamic context (variables, etc.)
#[derive(Debug)]
pub enum SimpleMatcher<'a> {
    /// A literal string that must occur somewhere in the input
    Substring(SubstringMatcher<'a>),
    /// A regular expression that must occur somewhere in the input
    Regex(RegexMatcher<'a>),
}
impl<'a> SimpleMatcher<'a> {
    pub fn into_boxed(self) -> AnyMatcher<'a> {
        match self {
            Self::Substring(matcher) => Box::new(matcher),
            Self::Regex(matcher) => Box::new(matcher),
        }
    }
}
impl<'a> MatcherMut for SimpleMatcher<'a> {
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
impl<'a> Matcher for SimpleMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        match self {
            Self::Substring(matcher) => matcher.try_match(input, context),
            Self::Regex(matcher) => matcher.try_match(input, context),
        }
    }
}
impl<'a> Spanned for SimpleMatcher<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Substring(matcher) => matcher.span(),
            Self::Regex(matcher) => matcher.span(),
        }
    }
}
