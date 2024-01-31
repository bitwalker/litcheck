use crate::{common::*, pattern::search::Match as SearchMatch};

/// This type is used to visit a set of N unordered patterns,
/// such as those associated with CHECK-DAG/CHECK-NOT. It finds
/// matches for all N patterns, or fails with errors indicating
/// which patterns could not be matched.
///
/// Static patterns are those which meet one of the following criteria:
///
/// * The patterns involved are of a single pattern type (e.g. regex)
/// * The patterns do not contain any match blocks or substitutions
///
/// Currently, this visitor is used for evaluating sets of patterns
/// which are either sets of literal strings, or sets of regexes.
///
/// This visitor requires a [PatternSetSearcher] that will be used to
/// find matches for the set of patterns we wish to visit.
///
/// Using that searcher, this visitor will find the next longest match
/// for any of the patterns, mark that pattern visited so it is not
/// matched again. The search continues until all patterns are matched,
/// or we run out of input to search. Either way, all match results
/// are returned at the end for use in diagnostics.
pub struct StaticPatternSetVisitor<'a, S: PatternSetSearcher> {
    searcher: &'a mut S,
    errors: Vec<Option<CheckFailedError>>,
}
#[cfg(test)]
impl<'a, S: PatternSetSearcher + fmt::Debug> fmt::Debug for StaticPatternSetVisitor<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        f.debug_struct("StaticPatternSetVisitor")
            .field("searcher", &self.searcher)
            .field("errors", &self.errors)
            .finish()
    }
}
impl<'a, S: PatternSetSearcher> StaticPatternSetVisitor<'a, S> {
    /// Create a new visitor for the given [PatternSetSearcher]
    pub fn new(searcher: &'a mut S) -> Self {
        let num_patterns = searcher.patterns_len();

        let mut errors = Vec::with_capacity(num_patterns);
        errors.resize_with(num_patterns, || None);
        Self { searcher, errors }
    }

    pub fn try_match_all<'input>(
        &mut self,
        context: &mut ContextGuard<'_, 'input, '_>,
    ) -> DiagResult<Matches<'input>> {
        let mut matches = Matches::with_capacity(self.patterns_len());
        loop {
            let result = self.searcher.try_match_next(context)?;
            match result {
                MatchResult {
                    ty,
                    info: Some(info),
                } if ty.is_ok() => {
                    // Clear any previous errors for this pattern
                    self.errors[info.pattern_id] = None;

                    // Ignore this match if we have already matched this pattern
                    if matches
                        .iter()
                        .any(|mr| mr.pattern_id() == Some(info.pattern_id))
                    {
                        continue;
                    }

                    // Ignore this match if we have already matched a pattern to it at this location
                    let range = info.span.range();
                    if matches
                        .iter()
                        .map(|mr| mr.info.as_ref().unwrap().span.range())
                        .any(|span| span.contains(&range.start) || span.contains(&range.end))
                    {
                        continue;
                    }

                    // Record that we've matched this pattern
                    matches.push(MatchResult::new(ty, Some(info)));

                    // If we've matched all patterns, we're done, otherwise keep searching
                    if self.patterns_len() == matches.len() {
                        let pos = self.searcher.last_match_end().unwrap();
                        context.cursor_mut().set_start(pos);
                        break;
                    }
                }
                MatchResult {
                    ty: MatchType::Failed(err),
                    info: Some(info),
                } => {
                    self.errors[info.pattern_id] = Some(err);
                    continue;
                }
                MatchResult {
                    ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected { .. }),
                    info: None,
                } => {
                    // No more matches are possible
                    let errors = core::mem::take(&mut self.errors);
                    let num_matches = matches.len();
                    if self.patterns_len() != num_matches {
                        for (pattern_id, maybe_error) in errors.into_iter().enumerate() {
                            if let Some(error) = maybe_error {
                                matches.push(MatchResult::failed(error));
                            }
                            if matches.as_slice()[..num_matches]
                                .iter()
                                .any(|mr| mr.pattern_id() == Some(pattern_id))
                            {
                                continue;
                            }
                            let span = self.searcher.pattern_span(
                                <<S as Searcher>::Match as SearchMatch>::PatternID::new_unchecked(
                                    pattern_id,
                                ),
                            );
                            matches.push(MatchResult::failed(
                                CheckFailedError::MatchNoneButExpected {
                                    span,
                                    match_file: context.match_file(),
                                    note: None,
                                },
                            ));
                        }
                    } else {
                        assert!(errors.iter().all(|e| e.is_none()));
                    }
                    break;
                }
                result => {
                    matches.push(result);
                    break;
                }
            }
        }

        Ok(matches)
    }

    #[inline(always)]
    fn patterns_len(&self) -> usize {
        self.searcher.patterns_len()
    }
}
