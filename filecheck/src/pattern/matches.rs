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

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut MatchResult<'input>> + '_ {
        self.0.iter_mut()
    }

    pub fn push(&mut self, result: MatchResult<'input>) {
        use core::cmp::Ordering;

        match self
            .0
            .binary_search_by(|probe| match (probe.info.as_ref(), result.info.as_ref()) {
                // Always place new failed matches at the end
                (_, None) => Ordering::Less,
                // Failed matches always come last
                (None, Some(_)) => Ordering::Greater,
                (Some(a), Some(b)) => a
                    .span
                    .start()
                    .cmp(&b.span.start())
                    .then_with(|| a.span.end().cmp(&b.span.end()))
                    .then_with(|| a.pattern_span.start().cmp(&b.pattern_span.start())),
            }) {
            Ok(index) | Err(index) => {
                self.0.insert(index, result);
            }
        }
    }

    pub fn append(&mut self, matches: &mut Self) {
        self.0.append(&mut matches.0);
        self.sort();
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = MatchResult<'input>>,
    {
        self.0.extend(iter);
    }

    /// Return the maximum extent of the successful matches, or None if
    /// no matches were successful.
    pub fn range(&self) -> Option<Range<usize>> {
        match self.0.as_slice() {
            [MatchResult {
                info: Some(info),
                ty,
            }] if ty.is_ok() => Some(info.matched_range()),
            [MatchResult {
                info: Some(info),
                ty,
            }, .., MatchResult {
                info: Some(info2),
                ty: ty2,
            }] if ty.is_ok() && ty2.is_ok() => Some(Range::new(
                info.span.start().to_usize(),
                info2.span.end().to_usize(),
            )),
            matches => matches
                .iter()
                .find_map(|mr| mr.matched_range())
                .map(|start_range| {
                    matches
                        .iter()
                        .rev()
                        .find_map(|mr| {
                            mr.matched_range()
                                .map(|r| Range::new(start_range.start, r.end))
                        })
                        .unwrap_or(start_range)
                }),
        }
    }

    fn sort(&mut self) {
        use core::cmp::Ordering;

        self.0
            .sort_unstable_by(|a, b| match (a.info.as_ref(), b.info.as_ref()) {
                (None, None) => Ordering::Equal,
                (Some(_), None) => Ordering::Less,
                (None, Some(_)) => Ordering::Greater,
                (Some(a), Some(b)) => a
                    .span
                    .start()
                    .cmp(&b.span.start())
                    .then_with(|| a.span.end().cmp(&b.span.end()))
                    .then_with(|| a.pattern_span.start().cmp(&b.pattern_span.start())),
            });
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
        let mut matches = Self(SmallVec::from_iter(iter));
        matches.sort();
        matches
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
