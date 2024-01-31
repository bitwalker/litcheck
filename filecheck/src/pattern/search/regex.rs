use crate::common::Range;

use super::{DefaultSearcher, Input, Match};

pub type RegexSearcher<'input> = DefaultSearcher<
    regex_automata::Input<'input>,
    regex_automata::Match,
    regex_automata::MatchError,
>;

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
