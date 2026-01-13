use std::{cmp::Ordering, collections::VecDeque};

use crate::common::*;

pub struct PatternSetSearcher<'a, 'patterns, 'input> {
    input: Input<'input>,
    last_match_end: Option<usize>,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: &'patterns [Pattern<'a>],
    next_starts: Vec<usize>,
    /// A bitset representing patterns which should be skipped
    excluded: SmallVec<[u64; 1]>,
    /// The buffer of pending pattern matches
    ///
    /// Each time a search is performed, we locate the next match
    /// for all patterns, place them into this buffer, and sort by
    /// the start position of each match. We then pop matches from
    /// the buffer until no more remain, at which point we locate
    /// the next set of matches.
    found: VecDeque<MatchResult<'input>>,
}
impl<'a, 'patterns, 'input> PatternSetSearcher<'a, 'patterns, 'input> {
    pub fn new(input: Input<'input>, patterns: &'patterns [Pattern<'a>]) -> DiagResult<Self> {
        let num_patterns = patterns.len();
        let excluded_chunks = (num_patterns / 64) + num_patterns.is_multiple_of(64) as usize;
        let excluded = smallvec![0; excluded_chunks];
        let next_starts = vec![0; num_patterns];

        Ok(Self {
            input,
            last_match_end: None,
            patterns,
            next_starts,
            excluded,
            found: VecDeque::with_capacity(num_patterns),
        })
    }
}
impl<'a, 'patterns, 'input> fmt::Debug for PatternSetSearcher<'a, 'patterns, 'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PatternSetSearcher")
            .field("patterns", &self.patterns)
            .finish()
    }
}

impl<'a, 'patterns, 'input> Spanned for PatternSetSearcher<'a, 'patterns, 'input> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a, 'patterns, 'input> PatternSearcher<'input> for PatternSetSearcher<'a, 'patterns, 'input> {
    type Input = Input<'input>;
    type PatternID = usize;

    fn input(&self) -> &Self::Input {
        &self.input
    }
    fn last_match_end(&self) -> Option<usize> {
        self.last_match_end
    }
    fn set_last_match_end(&mut self, end: usize) {
        self.last_match_end = Some(end);
        self.input.set_start(end);
        for start in self.next_starts.iter_mut() {
            *start = end;
        }
    }
    fn patterns_len(&self) -> usize {
        self.patterns.len()
    }
    fn pattern_span(&self, id: Self::PatternID) -> SourceSpan {
        self.patterns[id].span()
    }
    fn try_match_next<'context, C>(&mut self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        if let Some(mr) = self.found.pop_front() {
            if let Some(range) = mr.matched_range() {
                let last_end = self.last_match_end.get_or_insert(range.end);
                *last_end = core::cmp::max(*last_end, range.end);
            }
            return Ok(mr);
        }

        let mut next_start = self.input.start();
        for (pattern_id, pattern) in self.patterns.iter().enumerate() {
            let excluded_chunk = pattern_id / 64;
            let excluded_bit = 1 << (pattern_id % 64);
            let is_excluded = self.excluded[excluded_chunk] & excluded_bit == excluded_bit;

            if is_excluded {
                continue;
            }

            let mut input = self.input;
            input.set_start(self.next_starts[pattern_id]);

            let mut guard = context.protect();
            match pattern.try_match_mut(self.input, &mut guard)? {
                MatchResult {
                    info: Some(mut info),
                    ty,
                } if ty.is_ok() => {
                    let end = info.span.end();
                    next_start = core::cmp::min(end, next_start);
                    self.next_starts[pattern_id] =
                        if info.span.range().is_empty() && Some(end) == self.last_match_end {
                            end.saturating_add(1)
                        } else {
                            end
                        };
                    info.pattern_id = pattern_id;
                    self.found.push_back(MatchResult {
                        info: Some(info),
                        ty,
                    });
                }
                MatchResult { info: None, .. } => {
                    // We exclude patterns when we run out of matches for them
                    self.next_starts[pattern_id] = self.input.buffer().len();
                    self.excluded[excluded_chunk] |= excluded_bit;
                }
                MatchResult {
                    info: Some(mut info),
                    ty,
                } => {
                    // This is an error that we are going to return before other valid matches.
                    let end = info.span.end();
                    next_start = core::cmp::min(end, next_start);
                    self.next_starts[pattern_id] =
                        if info.span.range().is_empty() && Some(end) == self.last_match_end {
                            end.saturating_add(1)
                        } else {
                            end
                        };
                    info.pattern_id = pattern_id;
                    self.found.push_back(MatchResult {
                        info: Some(info),
                        ty,
                    });
                }
            }
        }

        // Set the input next start to the smallest end match
        self.input.set_start(next_start);

        // Sort the matches, errors first, then leftmost-longest (by span length, then by span start, then by pattern id)
        let found = self.found.make_contiguous();
        found.sort_unstable_by(|a, b| match (a, b) {
            (
                MatchResult {
                    ty: MatchType::Failed(_),
                    info: Some(info1),
                },
                MatchResult {
                    ty: MatchType::Failed(_),
                    info: Some(info2),
                },
            ) => info1
                .span
                .len()
                .cmp(&info2.span.len())
                .then_with(|| info1.span.start().cmp(&info2.span.start()))
                .then_with(|| info1.pattern_id.cmp(&info2.pattern_id)),
            (
                MatchResult {
                    ty: MatchType::Failed(_),
                    ..
                },
                _,
            ) => Ordering::Less,
            (
                _,
                MatchResult {
                    ty: MatchType::Failed(_),
                    ..
                },
            ) => Ordering::Greater,
            (
                MatchResult {
                    info: Some(info1), ..
                },
                MatchResult {
                    info: Some(info2), ..
                },
            ) => info1
                .span
                .len()
                .cmp(&info2.span.len())
                .then_with(|| info1.span.start().cmp(&info2.span.start()))
                .then_with(|| info1.pattern_id.cmp(&info2.pattern_id)),
            _ => unreachable!(),
        });

        // Return the next match
        match self.found.pop_front() {
            Some(found) => {
                if let Some(range) = found.matched_range() {
                    let last_end = self.last_match_end.get_or_insert(range.end);
                    *last_end = core::cmp::max(*last_end, range.end);
                }
                Ok(found)
            }
            None => Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            )),
        }
    }
}
