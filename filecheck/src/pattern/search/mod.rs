mod aho_corasick;
mod r#default;
mod regex;
mod regex_set;
mod substring_set;

pub use self::aho_corasick::AhoCorasickSearcher;
pub use self::r#default::DefaultSearcher;
pub use self::regex::RegexSearcher;
pub use self::regex_set::RegexSetSearcher;
pub use self::substring_set::SubstringSetSearcher;

use std::{fmt, ops::RangeBounds};

use crate::common::{Context, DiagResult, MatchResult, PatternIdentifier, Range, SourceSpan};

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
    ) -> DiagResult<MatchResult<'input>>
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

pub trait Input: fmt::Debug {
    fn buffer(&self) -> &[u8];
    fn anchored(&self) -> bool;
    fn range(&self) -> Range<usize>;
    fn start(&self) -> usize;
    fn set_start(&mut self, start: usize);
    fn set_range(&mut self, range: Range<usize>);
    fn as_input(&self) -> crate::common::Input<'_> {
        crate::common::Input::new(self.buffer(), false)
            .anchored(self.anchored())
            .span(self.range())
    }
}
