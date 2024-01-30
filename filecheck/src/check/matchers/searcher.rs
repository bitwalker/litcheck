use std::{fmt, ops::RangeBounds};

use litcheck::{
    diagnostics::{DiagResult, SourceSpan},
    range::{self, Range},
};

use crate::check::{Context, PatternIdentifier};

pub type AhoCorasickSearcher<'input> =
    DefaultSearcher<aho_corasick::Input<'input>, aho_corasick::Match, aho_corasick::MatchError>;

pub type RegexSearcher<'input> = DefaultSearcher<
    regex_automata::Input<'input>,
    regex_automata::Match,
    regex_automata::MatchError,
>;

pub trait Searcher {
    type Input: Input;
    type Match: Match;
    type MatchError: std::error::Error;

    fn input(&self) -> &Self::Input;
    fn last_match_end(&self) -> Option<usize>;
    fn set_last_match_end(&mut self, end: usize);
    fn set_range<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>;
    fn advance<F>(&mut self, finder: F) -> Option<Self::Match>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        match self.try_advance(finder) {
            Ok(m) => m,
            Err(err) => panic!("unexpected search error: {}", err),
        }
    }
    fn try_advance<F>(&mut self, finder: F) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>;

    fn handle_overlapping_empty_match<F>(
        &mut self,
        m: Self::Match,
        finder: F,
    ) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>;
}

pub trait PatternSetSearcher: Searcher {
    fn patterns_len(&self) -> usize;
    fn pattern_span(&self, id: <<Self as Searcher>::Match as Match>::PatternID) -> SourceSpan;
    fn try_match_next<'input, 'context, C>(
        &mut self,
        context: &C,
    ) -> DiagResult<crate::check::MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized;
}

pub trait Match: fmt::Debug {
    type PatternID: PatternIdentifier;

    fn is_empty(&self) -> bool;
    fn pattern(&self) -> Self::PatternID;
    fn end(&self) -> usize;
    fn range(&self) -> Range<usize>;
}
impl Match for aho_corasick::Match {
    type PatternID = aho_corasick::PatternID;

    fn is_empty(&self) -> bool {
        aho_corasick::Match::is_empty(self)
    }
    fn pattern(&self) -> Self::PatternID {
        aho_corasick::Match::pattern(self)
    }
    fn end(&self) -> usize {
        aho_corasick::Match::end(self)
    }
    fn range(&self) -> Range<usize> {
        aho_corasick::Match::range(self).into()
    }
}
impl Match for regex_automata::Match {
    type PatternID = regex_automata::PatternID;

    fn is_empty(&self) -> bool {
        regex_automata::Match::is_empty(self)
    }
    fn pattern(&self) -> Self::PatternID {
        regex_automata::Match::pattern(self)
    }
    fn end(&self) -> usize {
        regex_automata::Match::end(self)
    }
    fn range(&self) -> Range<usize> {
        regex_automata::Match::range(self).into()
    }
}

pub trait Input: fmt::Debug {
    fn buffer(&self) -> &[u8];
    fn anchored(&self) -> bool;
    fn range(&self) -> Range<usize>;
    fn start(&self) -> usize;
    fn set_start(&mut self, start: usize);
    fn set_range(&mut self, range: Range<usize>);
    fn as_input(&self) -> crate::check::Input<'_> {
        crate::check::Input::new(self.buffer(), false)
            .anchored(self.anchored())
            .span(self.range())
    }
}
impl<'input> Input for aho_corasick::Input<'input> {
    fn buffer(&self) -> &[u8] {
        self.haystack()
    }
    fn anchored(&self) -> bool {
        self.get_anchored().is_anchored()
    }
    fn range(&self) -> Range<usize> {
        aho_corasick::Input::get_range(self).into()
    }
    fn start(&self) -> usize {
        aho_corasick::Input::start(self)
    }
    fn set_start(&mut self, start: usize) {
        aho_corasick::Input::set_start(self, start)
    }
    fn set_range(&mut self, range: Range<usize>) {
        aho_corasick::Input::set_range(self, range)
    }
}
impl<'input> Input for regex_automata::Input<'input> {
    fn buffer(&self) -> &[u8] {
        self.haystack()
    }
    fn anchored(&self) -> bool {
        self.get_anchored().is_anchored()
    }
    fn range(&self) -> Range<usize> {
        regex_automata::Input::get_range(self).into()
    }
    fn start(&self) -> usize {
        regex_automata::Input::start(self)
    }
    fn set_start(&mut self, start: usize) {
        regex_automata::Input::set_start(self, start)
    }
    fn set_range(&mut self, range: Range<usize>) {
        regex_automata::Input::set_range(self, range)
    }
}

#[derive(Debug)]
pub struct DefaultSearcher<I, M, E> {
    input: I,
    last_match_end: Option<usize>,
    concrete: core::marker::PhantomData<(M, E)>,
}
impl<I, M, E> DefaultSearcher<I, M, E>
where
    I: Input,
    M: Match,
    E: std::error::Error,
{
    pub fn new(input: I) -> Self {
        Self {
            input,
            last_match_end: None,
            concrete: core::marker::PhantomData,
        }
    }
}
impl<I, M, E> Searcher for DefaultSearcher<I, M, E>
where
    I: Input,
    M: Match,
    E: std::error::Error,
{
    type Input = I;
    type Match = M;
    type MatchError = E;

    fn input(&self) -> &Self::Input {
        &self.input
    }

    fn last_match_end(&self) -> Option<usize> {
        self.last_match_end
    }

    fn set_last_match_end(&mut self, start: usize) {
        self.input.set_start(start);
        self.last_match_end = Some(start);
    }

    fn set_range<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let fallback_bounds = self.input.range();
        let range = range::range_from_bounds_with_defaults(
            range,
            fallback_bounds.start,
            fallback_bounds.end,
        );
        self.input.set_range(range);
        self.last_match_end = Some(range.start);
    }

    fn try_advance<F>(&mut self, mut finder: F) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        let mut m = match finder(&self.input)? {
            None => return Ok(None),
            Some(m) => m,
        };
        if m.is_empty() && Some(m.end()) == self.last_match_end {
            m = match self.handle_overlapping_empty_match(m, finder)? {
                None => return Ok(None),
                Some(m) => m,
            };
        }
        self.input.set_start(m.end());
        self.last_match_end = Some(m.end());
        Ok(Some(m))
    }

    #[cold]
    #[inline(never)]
    fn handle_overlapping_empty_match<F>(
        &mut self,
        m: Self::Match,
        mut finder: F,
    ) -> Result<Option<Self::Match>, Self::MatchError>
    where
        F: FnMut(&Self::Input) -> Result<Option<Self::Match>, Self::MatchError>,
    {
        assert!(m.is_empty());
        self.input
            .set_start(self.input.start().checked_add(1).unwrap());
        finder(&self.input)
    }
}
