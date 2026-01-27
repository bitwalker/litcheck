use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind, StartKind};

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
    pub fn new(pattern: Span<Cow<'a, str>>, config: &Config) -> DiagResult<Self> {
        Self::new_with_start_kind(pattern, StartKind::Unanchored, config)
    }

    pub fn new_with_start_kind(
        pattern: Span<Cow<'a, str>>,
        start_kind: StartKind,
        config: &Config,
    ) -> DiagResult<Self> {
        assert!(
            !pattern.is_empty(),
            "an empty string is not a valid substring pattern"
        );

        let pattern = pattern
            .map(|p| text::canonicalize_horizontal_whitespace(p, config.options.strict_whitespace));

        let (span, pattern) = pattern.into_parts();
        let mut builder = AhoCorasickBuilder::new();
        let searcher = builder
            .match_kind(MatchKind::LeftmostLongest)
            .start_kind(start_kind)
            .kind(Some(AhoCorasickKind::DFA))
            .ascii_case_insensitive(config.options.ignore_case)
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

    pub fn pattern(&self) -> &str {
        self.pattern.inner().as_ref()
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
            let span = SourceSpan::from_range_unchecked(input.source_id(), matched.range());
            Ok(MatchResult::ok(MatchInfo::new(span, self.span)))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.source_file(self.span.source_id()).unwrap(),
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
    use crate::source_file;

    use super::*;

    #[test]
    fn test_substring_matcher() -> DiagResult<()> {
        let mut context = TestContext::new();
        let match_file = source_file!(context.config, "CHECK: Name: bar");
        let input_file = source_file!(
            context.config,
            "
Name: foo
Field: 1

Name: bar
Field: 2
"
            .trim_start()
        );
        context
            .with_checks(match_file)
            .with_input(input_file.clone());

        let pattern = Span::new(
            SourceSpan::from_range_unchecked(input_file.id(), 8..10),
            Cow::Borrowed("Name: bar"),
        );
        let matcher =
            SubstringMatcher::new(pattern, &context.config).expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.start().to_u32(), 20);
        assert_eq!(info.span.len(), 9);
        assert_eq!(input.as_str(info.span.into_slice_index()), "Name: bar");

        Ok(())
    }
}
