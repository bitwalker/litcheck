use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind};

use crate::common::*;

/// This matcher searches for a given substring in the input buffer,
/// with some control over how the search is conducted.
pub struct SubstringMatcher<'a> {
    /// The span of the pattern in the check file
    /// from which this matcher is derived
    span: SourceSpan,
    /// The original pattern used to construct this matcher
    pattern: Span<Cow<'a, str>>,
    /// The automaton used to perform the search
    searcher: AhoCorasick,
}
impl<'a> fmt::Debug for SubstringMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SubstringMatcher")
            .field("pattern", &self.pattern)
            .field("kind", &self.searcher.kind())
            .field("start_kind", &self.searcher.start_kind())
            .field("match_kind", &self.searcher.match_kind())
            .finish()
    }
}
impl<'a> SubstringMatcher<'a> {
    /// Construct a new matcher from a given string
    pub fn new(pattern: Span<Cow<'a, str>>) -> DiagResult<Self> {
        assert!(
            !pattern.is_empty(),
            "an empty string is not a valid substring pattern"
        );

        let (span, pattern) = pattern.into_parts();
        let mut builder = AhoCorasickBuilder::new();
        let searcher = builder
            .match_kind(MatchKind::LeftmostLongest)
            .kind(Some(AhoCorasickKind::DFA))
            .build([pattern.as_ref()])
            .map_err(|err| {
                let diag = Diag::new("failed to build aho-corasick searcher")
                    .with_help("this pattern was constructed as a DFA, with leftmost-longest match semantics, \
                                it is possible a less restrictive configuration would succeed")
                    .and_label(Label::new(span, err.to_string()));
                Report::from(diag)
            })?;
        Ok(Self {
            span,
            pattern: Span::new(span, pattern),
            searcher,
        })
    }
}
impl<'a> MatcherMut for SubstringMatcher<'a> {
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
impl<'a> Matcher for SubstringMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        if let Some(matched) = self.searcher.find(input) {
            Ok(MatchResult::ok(MatchInfo::new(matched.range(), self.span)))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}
impl<'a> Spanned for SubstringMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substring_matcher() -> DiagResult<()> {
        let mut context = TestContext::new();
        context.with_checks("CHECK: Name: bar").with_input(
            "
Name: foo
Field: 1

Name: bar
Field: 2
"
            .trim_start(),
        );

        let pattern = Span::new(8..10, Cow::Borrowed("Name: bar"));
        let matcher = SubstringMatcher::new(pattern).expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 20);
        assert_eq!(info.span.len(), 9);
        assert_eq!(
            input.as_str(info.span.offset()..(info.span.offset() + info.span.len())),
            "Name: bar"
        );

        Ok(())
    }
}
