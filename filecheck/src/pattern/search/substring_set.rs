use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind, StartKind};

use crate::common::*;

use super::AhoCorasickSearcher;

pub struct SubstringSetSearcher<'a, 'patterns, 'input> {
    source_id: SourceId,
    buffer: &'input [u8],
    crlf: bool,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Cow<'patterns, [Span<Cow<'a, str>>]>,
    /// The compiled regex which will be used to search the input buffer
    pattern: AhoCorasick,
    /// The searcher used to maintain the search state in the buffer
    searcher: AhoCorasickSearcher<'input>,
}
impl<'a, 'patterns, 'input> SubstringSetSearcher<'a, 'patterns, 'input> {
    pub fn new(
        input: Input<'input>,
        patterns: Cow<'patterns, [Span<Cow<'a, str>>]>,
    ) -> DiagResult<Self> {
        let source_id = input.source_id();
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
                    .map(|s| Label::new(s.span(), err.to_string()).into());
                let diag = Diag::new("failed to build substring set searcher")
                    .and_labels(labels)
                    .with_help(format!(
                        "search configuration: {:?}, {:?}",
                        MatchKind::LeftmostLongest,
                        StartKind::Unanchored
                    ));
                Report::from(diag)
            })?;

        let searcher = AhoCorasickSearcher::new(input.into());

        Ok(Self {
            source_id,
            buffer,
            crlf,
            patterns,
            pattern,
            searcher,
        })
    }

    pub fn input(&self) -> Input<'input> {
        let input = self.searcher.input();
        Input::new(self.source_id, self.buffer, self.crlf)
            .bounded(input.get_range())
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
        let start = self
            .patterns
            .iter()
            .map(|p| p.span())
            .min_by(|a, b| a.start().cmp(&b.start()))
            .unwrap();
        let end = self
            .patterns
            .iter()
            .map(|p| p.span())
            .max_by(|a, b| a.end().cmp(&b.end()))
            .unwrap();
        SourceSpan::new(start.source_id(), Range::new(start.start(), end.end()))
    }
}
impl<'a, 'patterns, 'input> PatternSearcher<'input>
    for SubstringSetSearcher<'a, 'patterns, 'input>
{
    type Input = aho_corasick::Input<'input>;
    type PatternID = aho_corasick::PatternID;

    fn input(&self) -> &Self::Input {
        self.searcher.input()
    }
    fn last_match_end(&self) -> Option<usize> {
        self.searcher.last_match_end()
    }
    fn set_last_match_end(&mut self, end: usize) {
        self.searcher.set_last_match_end(end);
    }
    fn patterns_len(&self) -> usize {
        self.num_patterns()
    }
    fn pattern_span(&self, id: Self::PatternID) -> SourceSpan {
        SubstringSetSearcher::pattern_span(self, id.as_usize())
    }
    fn try_match_next<'context, C>(&mut self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        use super::Input as SearchInput;
        let result = self
            .searcher
            .advance(|input| self.pattern.try_find(input.as_input()));
        if let Some(matched) = result {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            Ok(MatchResult::ok(MatchInfo::new_with_pattern(
                SourceSpan::try_from_range(self.input().source_id(), matched.range()).unwrap(),
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
