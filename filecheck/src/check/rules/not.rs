#![allow(unused)]

use litcheck::diagnostics::{DiagResult, SourceSpan, Spanned};

use crate::check::{
    matchers::{self, AnyMatcherMut, Context, MatchInfo, MatchResult, MatcherMut, Matches},
    Check, CheckFailedError, MatchAll, MatchAny, MatchType, Pattern, PatternPrefix,
    RelatedCheckError,
};

use super::*;

#[derive(Debug)]
pub struct CheckNot<'a> {
    patterns: MatchAll<'a>,
}
impl<'a> CheckNot<'a> {
    pub fn new(patterns: MatchAll<'a>) -> Self {
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

    fn apply<'input, 'context, C>(&self, _context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        todo!()
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
    #[ignore]
    fn check_not_test() {
        todo!()
    }
}
