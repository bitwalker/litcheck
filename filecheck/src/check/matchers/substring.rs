use std::{borrow::Cow, fmt};

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, AhoCorasickKind, MatchKind, StartKind};

use litcheck::diagnostics::{Diag, DiagResult, LabeledSpan, Report, SourceSpan, Span, Spanned};

use super::*;
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
impl<'a> Matcher for SubstringMatcher<'a> {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        let matched = self.searcher.find(input)?;
        let span = SourceSpan::from(matched.range());
        Some(MatchInfo::new(span, self.span))
    }
}
impl<'a> Spanned for SubstringMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}

pub struct MultiSubstringMatcherBuilder<'a> {
    patterns: Vec<Span<Cow<'a, str>>>,
    start_kind: Option<StartKind>,
    match_kind: Option<MatchKind>,
    case_insensitive: bool,
    support_overlapping_matches: bool,
}
impl<'a> Default for MultiSubstringMatcherBuilder<'a> {
    #[inline]
    fn default() -> Self {
        Self::new_with_patterns(vec![])
    }
}
impl<'a> MultiSubstringMatcherBuilder<'a> {
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

    /// Build the [MultiSubstringMatcher]
    ///
    /// This function will panic if there are no patterns configured, or if
    /// an incompatible configuration is provided.
    pub fn build(self) -> DiagResult<MultiSubstringMatcher<'a>> {
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
        Ok(MultiSubstringMatcher {
            patterns: self.patterns,
            searcher,
        })
    }
}

/// This matcher is a variation on [SubstringMatcher] that searches
/// for a match of any of multiple substrings in the input buffer.
///
/// This is much more efficient than performing multiple independent searches
/// with [SubstringMatcher], so should be used whenever multiple substring
/// patterns could be matched at the same time.
pub struct MultiSubstringMatcher<'a> {
    /// The set of patterns to be matched
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The automaton that will perform the search
    searcher: AhoCorasick,
}
impl<'a> fmt::Debug for MultiSubstringMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("MultiSubstringMatcher")
            .field("searcher", &self.searcher)
            .finish()
    }
}
impl<'a> MultiSubstringMatcher<'a> {
    /// Create a new matcher for the given set of substring patterns
    ///
    /// NOTE: This function will panic if the set is empty.
    pub fn new(patterns: Vec<Span<Cow<'a, str>>>) -> DiagResult<Self> {
        MultiSubstringMatcherBuilder::new_with_patterns(patterns).build()
    }

    /// Get a builder for configuring and building a new [MultiSubstringMatcher]
    pub fn build() -> MultiSubstringMatcherBuilder<'a> {
        MultiSubstringMatcherBuilder::default()
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
impl<'a> Spanned for MultiSubstringMatcher<'a> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a> Matcher for MultiSubstringMatcher<'a> {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        let matched = self.searcher.find(input)?;
        let pattern_id = matched.pattern().as_usize();
        let pattern_span = self.patterns[pattern_id].span();
        let span = SourceSpan::from(matched.range());
        Some(MatchInfo::new_with_pattern(span, pattern_span, pattern_id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write;

    #[test]
    fn test_substring_matcher() {
        let mut input = String::new();
        writeln!(&mut input, "Name: foo").unwrap();
        writeln!(&mut input, "Field: 1").unwrap();
        writeln!(&mut input).unwrap();
        writeln!(&mut input, "Name: bar").unwrap();
        writeln!(&mut input, "Field: 2").unwrap();

        let pattern = Span::new(SourceSpan::from(0..0), Cow::Borrowed("Name: bar"));
        let matcher = SubstringMatcher::new(pattern).expect("expected pattern to be valid");
        let bytes = input.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 20);
        assert_eq!(info.span.len(), 9);
        assert_eq!(
            input.as_str(info.span.offset()..(info.span.offset() + info.span.len())),
            "Name: bar"
        );
    }

    #[test]
    fn test_multi_substring_matcher_overlapping() {
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

        let pattern1 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("tail call i64"));
        let pattern2 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("tail call i32"));
        let matcher = MultiSubstringMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let bytes = INPUT.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 58);
        assert_eq!(info.span.len(), 13);
        assert_eq!(input.as_str(info.matched_range()), "tail call i32");
    }

    #[test]
    fn test_multi_substring_matcher_overlapped() {
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

        let pattern1 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("tail call i32"));
        let pattern2 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("tail call"));
        let matcher = MultiSubstringMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let bytes = INPUT.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 58);
        assert_eq!(info.span.len(), 13);
        assert_eq!(input.as_str(info.matched_range()), "tail call i32");
    }

    #[test]
    fn test_multi_substring_matcher_disjoint() {
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

        let pattern1 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("inc4"));
        let pattern2 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("sub1"));
        let matcher = MultiSubstringMatcher::new(vec![pattern1, pattern2])
            .expect("expected pattern to be valid");
        let bytes = INPUT.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 14);
        assert_eq!(info.span.len(), 4);
        assert_eq!(input.as_str(info.matched_range()), "sub1");
    }

    #[test]
    fn test_multi_substring_matcher_anchored() {
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

        let pattern1 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("@inc4"));
        let pattern2 = Span::new(SourceSpan::from(0..0), Cow::Borrowed("@sub1"));
        let mut builder = MultiSubstringMatcher::build();
        builder
            .with_patterns([pattern1, pattern2])
            .support_anchored_search(true);
        let matcher = builder.build().expect("expected pattern to be valid");
        let bytes = INPUT.as_bytes();
        let input = Input::new(bytes, false).span(13..).anchored(true);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 13);
        assert_eq!(info.span.len(), 5);
        assert_eq!(input.as_str(info.matched_range()), "@sub1");

        let input = Input::new(bytes, false).anchored(true);
        let result = matcher.try_match(input);
        assert_eq!(result, None);
    }
}
