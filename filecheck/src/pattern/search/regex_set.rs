use regex_automata::{meta::Cache, util::captures::Captures, MatchKind, PatternID};

use crate::{common::*, expr::ValueType, pattern::matcher::regex};

use super::RegexSearcher;

pub struct RegexSetSearcher<'a, 'patterns, 'input> {
    buffer: &'input [u8],
    crlf: bool,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Cow<'patterns, [Span<Cow<'a, str>>]>,
    /// The compiled regex which will be used to search the input buffer
    pattern: Regex,
    /// The searcher used to maintain the search state in the buffer
    searcher: RegexSearcher<'input>,
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
                regex::build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
            })?;
        let searcher = RegexSearcher::new(input.into());
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

    pub fn from_matcher(
        input: Input<'input>,
        regex: Regex,
        patterns: Cow<'patterns, [Span<Cow<'a, str>>]>,
        capture_types: Cow<'patterns, [Vec<ValueType>]>,
    ) -> Self {
        let buffer = input.buffer();
        let crlf = input.is_crlf();
        let captures = regex.create_captures();
        let cache = regex.create_cache();
        Self {
            buffer,
            crlf,
            patterns,
            pattern: regex,
            searcher: RegexSearcher::new(input.into()),
            captures,
            cache,
            capture_types,
        }
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
impl<'a, 'patterns, 'b> PatternSetSearcher for RegexSetSearcher<'a, 'patterns, 'b> {
    fn patterns_len(&self) -> usize {
        self.patterns.len()
    }
    fn pattern_span(
        &self,
        id: <<Self as Searcher>::Match as super::Match>::PatternID,
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
                            ValueType::Number(_) => {
                                panic!("numeric captures are not expected here")
                            }
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
