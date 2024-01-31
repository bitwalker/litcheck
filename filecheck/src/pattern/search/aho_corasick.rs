use litcheck::range::Range;

use super::{DefaultSearcher, Input, Match};

pub type AhoCorasickSearcher<'input> =
    DefaultSearcher<aho_corasick::Input<'input>, aho_corasick::Match, aho_corasick::MatchError>;

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
