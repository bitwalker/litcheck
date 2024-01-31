use crate::common::*;

use super::iter::MatchIter;

#[derive(Debug, Default)]
pub struct Matches<'input>(SmallVec<[MatchResult<'input>; 1]>);
impl<'input> Matches<'input> {
    pub fn with_capacity(cap: usize) -> Self {
        Self(SmallVec::with_capacity(cap))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn as_slice(&self) -> &[MatchResult<'input>] {
        self.0.as_slice()
    }

    /// Returns true if all of the matches are successful
    pub fn is_ok(&self) -> bool {
        self.0.iter().all(|mr| mr.is_ok())
    }

    /// Returns true if at least one match failed
    pub fn has_errors(&self) -> bool {
        !self.is_ok()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &MatchResult<'input>> + '_ {
        self.0.iter()
    }

    pub fn push(&mut self, result: MatchResult<'input>) {
        self.0.push(result);
    }

    pub fn append(&mut self, matches: &mut Self) {
        self.0.append(&mut matches.0);
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = MatchResult<'input>>,
    {
        self.0.extend(iter);
    }
}
impl<'input> From<MatchResult<'input>> for Matches<'input> {
    fn from(result: MatchResult<'input>) -> Self {
        Self(smallvec![result])
    }
}
impl<'input> FromIterator<MatchResult<'input>> for Matches<'input> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = MatchResult<'input>>,
    {
        Self(SmallVec::from_iter(iter))
    }
}
impl<'input> IntoIterator for Matches<'input> {
    type Item = Result<Option<MatchInfo<'input>>, CheckFailedError>;
    type IntoIter = MatchIter<'input, 1>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        MatchIter::new(self.0.into_iter())
    }
}
