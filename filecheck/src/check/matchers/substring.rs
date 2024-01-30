use std::{borrow::Cow, fmt, ops::RangeBounds};

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind, StartKind};

use litcheck::diagnostics::{Diag, DiagResult, LabeledSpan, Report, SourceSpan, Span, Spanned};

use super::{searcher::Searcher, *};
use crate::check::Input;

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
                    .and_label(LabeledSpan::new_with_span(Some(err.to_string()), span));
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

pub struct SubstringSetBuilder<'a> {
    patterns: Vec<Span<Cow<'a, str>>>,
    start_kind: Option<StartKind>,
    match_kind: Option<MatchKind>,
    case_insensitive: bool,
    support_overlapping_matches: bool,
}
impl<'a> Default for SubstringSetBuilder<'a> {
    #[inline]
    fn default() -> Self {
        Self::new_with_patterns(vec![])
    }
}
impl<'a> SubstringSetBuilder<'a> {
    #[inline]
    pub fn new_with_patterns(patterns: Vec<Span<Cow<'a, str>>>) -> Self {
        Self {
            patterns,
            start_kind: None,
            match_kind: None,
            case_insensitive: false,
            support_overlapping_matches: false,
        }
    }

    /// Add `pattern` to the set of substrings to match
    pub fn with_pattern(&mut self, pattern: Span<Cow<'a, str>>) -> &mut Self {
        self.patterns.push(pattern);
        self
    }

    /// Add `patterns` to the set of substrings to match
    pub fn with_patterns<I>(&mut self, patterns: I) -> &mut Self
    where
        I: IntoIterator<Item = Span<Cow<'a, str>>>,
    {
        self.patterns.extend(patterns);
        self
    }

    /// Set whether or not the matcher will support anchored searches.
    ///
    /// Since supporting anchored searches can be significantly more expensive,
    /// you should only do so if you need such searches.
    pub fn support_anchored_search(&mut self, yes: bool) -> &mut Self {
        self.start_kind = if yes {
            Some(StartKind::Both)
        } else {
            Some(StartKind::Unanchored)
        };
        self
    }

    /// Set whether or not the matcher will support asking for overlapping matches
    ///
    /// NOTE: This will force the [MatchKind] to `MatchKind::Standard`, as other semantics
    /// are not support when computing overlapping matches.
    pub fn support_overlapping_matches(&mut self, yes: bool) -> &mut Self {
        self.support_overlapping_matches = yes;
        self.match_kind = Some(MatchKind::Standard);
        self
    }

    /// Set whether or not the search is case-sensitive.
    ///
    /// NOTE: This only applies to ASCII characters, if you require unicode
    /// case insensitivity, you should use [RegexMatcher] or [RegexSetMatcher]
    /// instead.
    pub fn case_insensitive(&mut self, yes: bool) -> &mut Self {
        self.case_insensitive = yes;
        self
    }

    /// Configure the match semantics for this matcher
    ///
    /// NOTE: This function will panic if the provided option conflicts with
    /// the `support_overlapping_matches` setting, as only the default semantics
    /// are supported when overlapping matches are computed.
    pub fn match_kind(&mut self, kind: MatchKind) -> &mut Self {
        assert!(
            !self.support_overlapping_matches,
            "cannot support {kind:?} when overlapping matches are enabled"
        );
        self.match_kind = Some(kind);
        self
    }

    /// Build the [SubstringSetMatcher]
    ///
    /// This function will panic if there are no patterns configured, or if
    /// an incompatible configuration is provided.
    pub fn build(self) -> DiagResult<SubstringSetMatcher<'a>> {
        assert!(
            !self.patterns.is_empty(),
            "there must be at least one pattern in the set"
        );

        let kind = if self.support_overlapping_matches {
            Some(AhoCorasickKind::DFA)
        } else {
            None
        };
        let match_kind = self.match_kind.unwrap_or(MatchKind::LeftmostLongest);
        let start_kind = self.start_kind.unwrap_or(StartKind::Unanchored);
        let mut builder = AhoCorasickBuilder::new();
        builder
            .ascii_case_insensitive(self.case_insensitive)
            .match_kind(match_kind)
            .start_kind(start_kind)
            .kind(kind);
        let searcher = builder
            .build(self.patterns.iter().map(|p| p.as_bytes()))
            .map_err(|err| {
                let labels = self
                    .patterns
                    .iter()
                    .map(|s| LabeledSpan::new_with_span(Some(err.to_string()), s.span()));
                let diag = Diag::new("failed to build multi-substring aho-corasick searcher")
                    .and_labels(labels)
                    .with_help(format!(
                        "search configuration: {kind:?}, {match_kind:?}, {start_kind:?}"
                    ));
                Report::from(diag)
            })?;
        Ok(SubstringSetMatcher {
            patterns: self.patterns,
            searcher,
        })
    }
}

pub struct SubstringSetSearcher<'a, 'patterns, 'input> {
    buffer: &'input [u8],
    crlf: bool,
    last_match_end: Option<usize>,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Cow<'patterns, Vec<Span<Cow<'a, str>>>>,
    /// The compiled regex which will be used to search the input buffer
    pattern: AhoCorasick,
    /// The searcher used to maintain the search state in the buffer
    searcher: super::searcher::AhoCorasickSearcher<'input>,
}
impl<'a, 'patterns, 'input> SubstringSetSearcher<'a, 'patterns, 'input> {
    pub fn new(
        input: Input<'input>,
        patterns: Cow<'patterns, Vec<Span<Cow<'a, str>>>>,
    ) -> DiagResult<Self> {
        let buffer = input.buffer();
        let crlf = input.is_crlf();
        let mut builder = AhoCorasickBuilder::new();
        builder
            .ascii_case_insensitive(false)
            .match_kind(MatchKind::LeftmostLongest)
            .start_kind(StartKind::Unanchored);
        let pattern = builder
            .build(patterns.iter().map(|p| p.as_bytes()))
            .map_err(|err| {
                let labels = patterns
                    .iter()
                    .map(|s| LabeledSpan::new_with_span(Some(err.to_string()), s.span()));
                let diag = Diag::new("failed to build substring set searcher")
                    .and_labels(labels)
                    .with_help(format!(
                        "search configuration: {:?}, {:?}",
                        MatchKind::LeftmostLongest,
                        StartKind::Unanchored
                    ));
                Report::from(diag)
            })?;

        let searcher = super::searcher::AhoCorasickSearcher::new(input.into());

        Ok(Self {
            buffer,
            crlf,
            last_match_end: None,
            patterns,
            pattern,
            searcher,
        })
    }

    pub fn input(&self) -> Input<'input> {
        let input = self.searcher.input();
        Input::new(self.buffer, self.crlf)
            .span(input.get_range())
            .anchored(input.get_anchored().is_anchored())
    }

    pub fn num_patterns(&self) -> usize {
        self.patterns.len()
    }

    pub fn pattern_span(&self, index: usize) -> SourceSpan {
        self.patterns[index].span()
    }
}
impl<'a, 'patterns, 'input> fmt::Debug for SubstringSetSearcher<'a, 'patterns, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SubstringSetSearcher")
            .field("patterns", &self.patterns)
            .field("kind", &self.pattern.kind())
            .field("start_kind", &self.pattern.start_kind())
            .field("match_kind", &self.pattern.match_kind())
            .finish()
    }
}

impl<'a, 'patterns, 'input> Spanned for SubstringSetSearcher<'a, 'patterns, 'input> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a, 'patterns, 'input> Searcher for SubstringSetSearcher<'a, 'patterns, 'input> {
    type Input = aho_corasick::Input<'input>;
    type Match = aho_corasick::Match;
    type MatchError = aho_corasick::MatchError;

    fn input(&self) -> &Self::Input {
        self.searcher.input()
    }
    fn last_match_end(&self) -> Option<usize> {
        self.last_match_end
    }
    fn set_last_match_end(&mut self, end: usize) {
        self.last_match_end = Some(end);
        self.searcher.set_last_match_end(end);
    }
    fn set_range<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        self.searcher.set_range(range);
    }
    fn try_advance<F>(&mut self, finder: F) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        self.searcher.try_advance(finder).map(|m| match m {
            Some(m) => {
                self.last_match_end = Some(m.end());
                Some(m)
            }
            None => None,
        })
    }

    fn handle_overlapping_empty_match<F>(
        &mut self,
        m: Self::Match,
        finder: F,
    ) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        self.searcher.handle_overlapping_empty_match(m, finder)
    }
}
impl<'a, 'patterns, 'b> super::searcher::PatternSetSearcher
    for SubstringSetSearcher<'a, 'patterns, 'b>
{
    fn patterns_len(&self) -> usize {
        self.num_patterns()
    }
    fn pattern_span(
        &self,
        id: <<Self as Searcher>::Match as searcher::Match>::PatternID,
    ) -> SourceSpan {
        SubstringSetSearcher::pattern_span(self, id.as_usize())
    }
    fn try_match_next<'input, 'context, C>(
        &mut self,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        use super::searcher::Input as SearchInput;
        let result = self
            .searcher
            .advance(|input| self.pattern.try_find(input.as_input()));
        if let Some(matched) = result {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            Ok(MatchResult::ok(MatchInfo::new_with_pattern(
                matched.range(),
                pattern_span,
                pattern_id,
            )))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}

/// This matcher is a variation on [SubstringMatcher] that searches
/// for a match of any of multiple substrings in the input buffer.
///
/// This is much more efficient than performing multiple independent searches
/// with [SubstringMatcher], so should be used whenever multiple substring
/// patterns could be matched at the same time.
pub struct SubstringSetMatcher<'a> {
    /// The set of patterns to be matched
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The automaton that will perform the search
    searcher: AhoCorasick,
}
impl<'a> fmt::Debug for SubstringSetMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SubstringSetMatcher")
            .field("patterns", &self.patterns)
            .field("kind", &self.searcher.kind())
            .field("start_kind", &self.searcher.start_kind())
            .field("match_kind", &self.searcher.match_kind())
            .finish()
    }
}
impl<'a> SubstringSetMatcher<'a> {
    /// Create a new matcher for the given set of substring patterns
    ///
    /// NOTE: This function will panic if the set is empty.
    pub fn new(patterns: Vec<Span<Cow<'a, str>>>) -> DiagResult<Self> {
        SubstringSetBuilder::new_with_patterns(patterns).build()
    }

    pub fn search<'input, 'patterns>(
        &'patterns self,
        input: Input<'input>,
    ) -> DiagResult<SubstringSetSearcher<'a, 'patterns, 'input>> {
        SubstringSetSearcher::new(input, Cow::Borrowed(&self.patterns))
    }

    pub fn pattern_len(&self) -> usize {
        self.patterns.len()
    }

    /// Get a builder for configuring and building a new [SubstringSetMatcher]
    pub fn build() -> SubstringSetBuilder<'a> {
        SubstringSetBuilder::default()
    }

    /// Search for all of the non-overlapping matches in the input
    pub fn try_match_all<'input>(&self, input: Input<'input>) -> Vec<MatchInfo<'input>> {
        let mut matches = vec![];
        for matched in self.searcher.find_iter(input) {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            let span = SourceSpan::from(matched.range());
            matches.push(MatchInfo::new_with_pattern(span, pattern_span, pattern_id))
        }
        matches
    }

    /// Search for all of the matches in the input, including overlapping matches
    pub fn try_match_overlapping<'input>(&self, input: Input<'input>) -> Vec<MatchInfo<'input>> {
        let mut matches = vec![];
        for matched in self.searcher.find_overlapping_iter(input) {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            let span = SourceSpan::from(matched.range());
            matches.push(MatchInfo::new_with_pattern(span, pattern_span, pattern_id))
        }
        matches
    }
}
impl<'a> Spanned for SubstringSetMatcher<'a> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a> MatcherMut for SubstringSetMatcher<'a> {
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
impl<'a> Matcher for SubstringSetMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        if let Some(matched) = self.searcher.find(input) {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            Ok(MatchResult::ok(MatchInfo::new_with_pattern(
                matched.range(),
                pattern_span,
                pattern_id,
            )))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::TestContext;

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

    #[test]
    fn test_multi_substring_matcher_overlapping() -> DiagResult<()> {
        const INPUT: &str = "
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
";
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: tail call i64
CHECK-DAG: tail call i32
",
            )
            .with_input(INPUT);

        let pattern1 = Span::new(12..24, Cow::Borrowed("tail call i64"));
        let pattern2 = Span::new(25..41, Cow::Borrowed("tail call i32"));
        let matcher = SubstringSetMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 58);
        assert_eq!(info.span.len(), 13);
        assert_eq!(input.as_str(info.matched_range()), "tail call i32");
        Ok(())
    }

    #[test]
    fn test_multi_substring_matcher_overlapped() -> DiagResult<()> {
        const INPUT: &str = "
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
";
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: tail call i32
CHECK-DAG: tail call
",
            )
            .with_input(INPUT);

        let pattern1 = Span::new(12..24, Cow::Borrowed("tail call i32"));
        let pattern2 = Span::new(25..37, Cow::Borrowed("tail call"));
        let matcher = SubstringSetMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 58);
        assert_eq!(info.span.len(), 13);
        assert_eq!(input.as_str(info.matched_range()), "tail call i32");
        Ok(())
    }

    #[test]
    fn test_multi_substring_matcher_disjoint() -> DiagResult<()> {
        const INPUT: &str = "
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
";
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: inc4
CHECK-DAG: sub1
",
            )
            .with_input(INPUT);

        let pattern1 = Span::new(12..17, Cow::Borrowed("inc4"));
        let pattern2 = Span::new(19..35, Cow::Borrowed("sub1"));
        let matcher = SubstringSetMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 14);
        assert_eq!(info.span.len(), 4);
        assert_eq!(input.as_str(info.matched_range()), "sub1");
        Ok(())
    }

    #[test]
    fn test_multi_substring_matcher_anchored() -> DiagResult<()> {
        const INPUT: &str = "
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
";
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: @inc4
CHECK-DAG: @sub1
",
            )
            .with_input(INPUT);

        let pattern1 = Span::new(0..0, Cow::Borrowed("@inc4"));
        let pattern2 = Span::new(0..0, Cow::Borrowed("@sub1"));
        let mut builder = SubstringSetMatcher::build();
        builder
            .with_patterns([pattern1, pattern2])
            .support_anchored_search(true);
        let matcher = builder.build().expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search_range(13..).anchored(true);
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 13);
        assert_eq!(info.span.len(), 5);
        assert_eq!(input.as_str(info.matched_range()), "@sub1");

        let input = mctx.search().anchored(true);
        let result = matcher.try_match(input, &mctx)?;
        assert_eq!(result.info, None);
        Ok(())
    }
}
