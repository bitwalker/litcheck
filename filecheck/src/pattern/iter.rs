use crate::common::{CheckFailedError, MatchInfo, MatchResult};

pub struct MatchIter<'input, const N: usize> {
    matches: smallvec::IntoIter<[MatchResult<'input>; N]>,
}
impl<'input, const N: usize> MatchIter<'input, N> {
    pub(super) fn new(matches: smallvec::IntoIter<[MatchResult<'input>; N]>) -> Self {
        Self { matches }
    }
}
impl<'input, const N: usize> MatchIter<'input, N> {
    #[inline]
    pub fn as_slice(&self) -> &[MatchResult<'input>] {
        self.matches.as_slice()
    }
}
impl<'input, const N: usize> ExactSizeIterator for MatchIter<'input, N> {
    fn len(&self) -> usize {
        self.matches.len()
    }
}
impl<'input, const N: usize> Iterator for MatchIter<'input, N> {
    type Item = Result<Option<MatchInfo<'input>>, CheckFailedError>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.matches.next().map(MatchResult::into_result)
    }
}
impl<'input, const N: usize> DoubleEndedIterator for MatchIter<'input, N> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.matches.next_back().map(MatchResult::into_result)
    }
}
