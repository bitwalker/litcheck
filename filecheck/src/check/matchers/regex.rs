use std::{borrow::Cow, fmt, ops::RangeBounds};

use regex_automata::{
    meta::{Cache, Regex},
    util::captures::Captures,
    MatchKind, PatternID,
};

use litcheck::diagnostics::{Diag, DiagResult, LabeledSpan, Report, SourceSpan, Span, Spanned};

use super::{searcher::Searcher, *};
use crate::{check::Input, expr::ValueType};

/// This matcher is used to match a single regular expression
///
/// This essentially corresponds to [SubstringMatcher], but
/// with regular expressions instead of literal strings.
pub struct RegexMatcher<'a> {
    /// The source string from which the regex was compiled
    pattern: Span<Cow<'a, str>>,
    /// The compiled form of the input regex
    regex: Regex,
    /// Metadata about captures in the pattern
    captures: Vec<ValueType>,
}
impl<'a> fmt::Debug for RegexMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexMatcher")
            .field("pattern", &self.pattern)
            .finish()
    }
}
impl<'a> RegexMatcher<'a> {
    pub fn new(pattern: Span<Cow<'a, str>>) -> DiagResult<Self> {
        let span = pattern.span();
        let regex = Regex::builder()
            .build(pattern.as_ref())
            .map_err(|error| build_error_to_diagnostic(error, 1, |_| span))?;

        // Compute capture group information
        let groups = regex.group_info();
        let num_captures = groups.group_len(PatternID::ZERO);
        let captures = vec![ValueType::String; num_captures];

        Ok(Self {
            pattern,
            regex,
            captures,
        })
    }
}
impl<'a> MatcherMut for RegexMatcher<'a> {
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
impl<'a> Matcher for RegexMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let regex_input = input.into();
        let mut captures = self.regex.create_captures();
        self.regex.search_captures(&regex_input, &mut captures);
        if let Some(matched) = captures.get_match() {
            let span = SourceSpan::from(matched.range());
            let mut capture_infos = Vec::with_capacity(captures.group_len());
            for (index, (capture, ty)) in captures
                .iter()
                .zip(self.captures.iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = capture {
                    let captured = input.as_str(capture_span.range());
                    capture_infos.push(CaptureInfo {
                        span: SourceSpan::from(capture_span.range()),
                        pattern_span: self.pattern.span(),
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(Cow::Borrowed(captured)),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
                }
            }
            Ok(MatchResult::ok(MatchInfo {
                span,
                pattern_span: self.pattern.span(),
                pattern_id: 0,
                captures: capture_infos,
            }))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}
impl<'a> Spanned for RegexMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }
}

pub struct RegexSetSearcher<'a, 'patterns, 'input> {
    buffer: &'input [u8],
    crlf: bool,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Cow<'patterns, [Span<Cow<'a, str>>]>,
    /// The compiled regex which will be used to search the input buffer
    pattern: Regex,
    /// The searcher used to maintain the search state in the buffer
    searcher: super::searcher::RegexSearcher<'input>,
    /// Captures storage
    captures: Captures,
    /// Cache storage
    cache: Cache,
    /// Metadata about captures in the given patterns
    ///
    /// Each pattern gets its own vector of capture info, since
    /// there is no requirement that all patterns have the same
    /// number or type of captures
    capture_types: Cow<'patterns, [Vec<ValueType>]>,
}
impl<'a, 'patterns, 'input> RegexSetSearcher<'a, 'patterns, 'input> {
    pub fn new(input: Input<'input>, patterns: Vec<Span<Cow<'a, str>>>) -> DiagResult<Self> {
        let buffer = input.buffer();
        let crlf = input.is_crlf();
        let pattern = Regex::builder()
            .configure(Regex::config().match_kind(MatchKind::All))
            .build_many(patterns.as_slice())
            .map_err(|error| {
                build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
            })?;
        let searcher = super::searcher::RegexSearcher::new(input.into());
        let captures = pattern.create_captures();
        let cache = pattern.create_cache();

        // Compute capture group information
        let mut capture_types = vec![];
        let groups = pattern.group_info();
        for i in 0..patterns.len() {
            let pid = PatternID::new_unchecked(i);
            let num_captures = groups.group_len(pid);
            capture_types.push(vec![ValueType::String; num_captures]);
        }

        Ok(Self {
            buffer,
            crlf,
            patterns: Cow::Owned(patterns),
            pattern,
            searcher,
            captures,
            cache,
            capture_types: Cow::Owned(capture_types),
        })
    }

    pub fn input(&self) -> Input<'input> {
        let input = self.searcher.input();
        Input::new(self.buffer, self.crlf)
            .span(input.get_range())
            .anchored(input.get_anchored().is_anchored())
    }
}
impl<'a, 'patterns, 'input> fmt::Debug for RegexSetSearcher<'a, 'patterns, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexSetSearcher")
            .field("pattern", &self.pattern)
            .field("searcher", &self.searcher)
            .finish()
    }
}
impl<'a, 'patterns, 'input> Spanned for RegexSetSearcher<'a, 'patterns, 'input> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a, 'patterns, 'input> Searcher for RegexSetSearcher<'a, 'patterns, 'input> {
    type Input = regex_automata::Input<'input>;
    type Match = regex_automata::Match;
    type MatchError = regex_automata::MatchError;

    #[inline]
    fn input(&self) -> &Self::Input {
        self.searcher.input()
    }
    #[inline]
    fn last_match_end(&self) -> Option<usize> {
        self.searcher.last_match_end()
    }
    #[inline]
    fn set_last_match_end(&mut self, end: usize) {
        self.searcher.set_last_match_end(end);
    }
    #[inline]
    fn set_range<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        self.searcher.set_range(range);
    }
    #[inline]
    fn try_advance<F>(&mut self, finder: F) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        self.searcher.try_advance(finder)
    }
    #[inline]
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
    for RegexSetSearcher<'a, 'patterns, 'b>
{
    fn patterns_len(&self) -> usize {
        self.patterns.len()
    }
    fn pattern_span(
        &self,
        id: <<Self as Searcher>::Match as searcher::Match>::PatternID,
    ) -> SourceSpan {
        self.patterns[id.as_usize()].span()
    }
    fn try_match_next<'input, 'context, C>(
        &mut self,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let result = self.searcher.advance(|input| {
            self.pattern
                .search_captures_with(&mut self.cache, input, &mut self.captures);
            Ok(self.captures.get_match())
        });
        if let Some(matched) = result {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            let span = SourceSpan::from(matched.range());
            let mut capture_infos = Vec::with_capacity(self.captures.group_len());
            for (index, (capture, ty)) in self
                .captures
                .iter()
                .zip(self.capture_types[pattern_id].iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = capture {
                    let input = context.search();
                    let captured = input.as_str(capture_span.range());
                    capture_infos.push(CaptureInfo {
                        span: SourceSpan::from(capture_span.range()),
                        pattern_span,
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(Cow::Borrowed(captured)),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
                }
            }
            Ok(MatchResult::ok(MatchInfo {
                span,
                pattern_span,
                pattern_id,
                captures: capture_infos,
            }))
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

pub struct RegexSetMatcher<'a> {
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The compiled regex which will be used to search the input buffer
    pattern: Regex,
    /// Metadata about captures in the given patterns
    ///
    /// Each pattern gets its own vector of capture info, since
    /// there is no requirement that all patterns have the same
    /// number or type of captures
    captures: Vec<Vec<ValueType>>,
}
impl<'a> fmt::Debug for RegexSetMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexSetMatcher")
            .field("pattern", &self.pattern)
            .finish()
    }
}
impl<'a> RegexSetMatcher<'a> {
    pub fn new(patterns: Vec<Span<Cow<'a, str>>>) -> DiagResult<Self> {
        let pattern = Regex::builder()
            .configure(Regex::config().match_kind(MatchKind::All))
            .build_many(patterns.as_slice())
            .map_err(|error| {
                build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
            })?;

        // Compute capture group information
        let mut captures = vec![];
        let groups = pattern.group_info();
        for i in 0..patterns.len() {
            let pid = PatternID::new_unchecked(i);
            let num_captures = groups.group_len(pid);
            captures.push(vec![ValueType::String; num_captures]);
        }

        Ok(Self {
            patterns,
            pattern,
            captures,
        })
    }

    pub fn search<'input, 'patterns>(
        &'patterns self,
        input: Input<'input>,
    ) -> RegexSetSearcher<'a, 'patterns, 'input> {
        let buffer = input.buffer();
        let crlf = input.is_crlf();
        let pattern = self.pattern.clone();
        let captures = pattern.create_captures();
        let cache = pattern.create_cache();
        RegexSetSearcher {
            buffer,
            crlf,
            patterns: Cow::Borrowed(&self.patterns),
            pattern,
            searcher: super::searcher::RegexSearcher::new(input.into()),
            captures,
            cache,
            capture_types: Cow::Borrowed(&self.captures),
        }
    }

    pub fn from_parts(
        pattern: Regex,
        patterns: Vec<Span<Cow<'a, str>>>,
        captures: Vec<Vec<ValueType>>,
    ) -> Self {
        Self {
            patterns,
            pattern,
            captures,
        }
    }

    pub fn pattern_len(&self) -> usize {
        self.patterns.len()
    }
}
impl<'a> Spanned for RegexSetMatcher<'a> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a> MatcherMut for RegexSetMatcher<'a> {
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
impl<'a> Matcher for RegexSetMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let regex_input = input.into();
        let mut captures = self.pattern.create_captures();
        self.pattern.search_captures(&regex_input, &mut captures);
        if let Some(matched) = captures.get_match() {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            let span = SourceSpan::from(matched.range());
            let mut capture_infos = Vec::with_capacity(captures.group_len());
            for (index, (capture, ty)) in captures
                .iter()
                .zip(self.captures[pattern_id].iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = capture {
                    let captured = input.as_str(capture_span.range());
                    capture_infos.push(CaptureInfo {
                        span: SourceSpan::from(capture_span.range()),
                        pattern_span,
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(Cow::Borrowed(captured)),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
                }
            }
            Ok(MatchResult::ok(MatchInfo {
                span,
                pattern_span,
                pattern_id,
                captures: capture_infos,
            }))
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

pub fn build_error_to_diagnostic<F>(
    error: regex_automata::meta::BuildError,
    num_patterns: usize,
    get_pattern_span: F,
) -> Report
where
    F: Fn(usize) -> SourceSpan,
{
    let diagnostic = if let Some(pattern_id) = error.pattern() {
        let span = get_pattern_span(pattern_id.as_usize());
        if let Some(syntax_err) = error.syntax_error() {
            Diag::new(format!("invalid regex pattern: {error}"))
                .with_help("a syntax error prevented us from compiling this pattern")
                .with_url("https://docs.rs/regex/latest/regex/index.html#syntax")
                .and_label(LabeledSpan::new_with_span(
                    Some(syntax_err.to_string()),
                    span,
                ))
        } else {
            Diag::new("unable to compile regex pattern set")
                .with_help("the pattern shown exceeded preconfigured limits during construction")
                .and_label(LabeledSpan::new_with_span(None, span))
        }
    } else if num_patterns > 1 {
        Diag::new(format!("unable to compile regex pattern set: {error}"))
            .with_help("construction of a multi-pattern regex from this set failed to due to preconfigured limits")
            .with_labels((0..num_patterns).map(|pid| LabeledSpan::new_with_span(None, get_pattern_span(pid))))
    } else {
        Diag::new(format!("unable to compile regex pattern: {error}"))
            .with_help("construction of this regex failed to due to preconfigured limits")
            .with_label(LabeledSpan::new_with_span(None, get_pattern_span(0)))
    };
    Report::from(diagnostic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::TestContext;

    #[test]
    fn test_regex_matcher() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(r"CHECK: {{Name: b[[:alpha:]]*}}")
            .with_input(
                "
Name: foo
Field: 1

Name: bar
Field: 2
",
            );

        let pattern = Span::new(SourceSpan::from(0..0), Cow::Borrowed("Name: b[[:alpha:]]*"));
        let matcher = RegexMatcher::new(pattern).expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 21);
        assert_eq!(info.span.len(), 9);
        assert_eq!(
            input.as_str(info.span.offset()..(info.span.offset() + info.span.len())),
            "Name: bar"
        );

        Ok(())
    }
}
