use crate::common::*;

#[derive(Debug)]
pub struct DefaultSearcher<I, M, E> {
    input: I,
    last_match_end: Option<usize>,
    concrete: core::marker::PhantomData<(M, E)>,
}
impl<I, M, E> DefaultSearcher<I, M, E>
where
    I: super::Input,
    M: super::Match,
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
    I: super::Input,
    M: super::Match,
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
