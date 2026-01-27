use crate::{ast::Capture, common::*, env::Bindings, pattern::search::Input as SearcherInput};

/// This type is used to visit a set of N unordered patterns,
/// such as those associated with CHECK-DAG/CHECK-NOT. It finds
/// matches for all N patterns, or fails with errors indicating
/// which patterns could not be matched.
///
/// Dynamic patterns are those which meet one of the following criteria:
///
/// * The patterns involved are prefixed with a heterogenous set of pattern types
/// * The patterns contain match blocks or substitutions
///
/// This visitor requires a [PatternSearcher] that will be used to
/// find matches for a set of "prefix" patterns. This might be a single
/// pattern for the set (if they have a common prefix), or a set where
/// there is exactly one prefix pattern for every pattern (if the prefixes
/// are disjoint).
///
/// Using that searcher, this visitor will find the next match for any of
/// the prefixe patterns, and then find the longest match for all of the
/// suffix patterns associated with that prefix. Once found, the match for
/// that one pattern is recorded, and is never visited again. Once all
/// patterns have been found, the visitor stops searching and returns all
/// matches.
///
/// If any of the following occur, the set of matches found + those that
/// have not (or which have failed) is returned, so that a useful diagnostic
/// can be presented showing why the overall match failed:
///
/// * No more matches can be found for any prefix pattern
/// * We hit end-of-block/file before all matches have been found
/// * One or more of the matches failed due to constraints or other post-processing, and no further
///   successful matches for those patterns were found.
pub struct DynamicPatternSetVisitor<'a, S> {
    searcher: &'a mut S,
    suffix_patterns: &'a [Vec<Pattern<'a>>],
    patterns_matched: Vec<Vec<Span<usize>>>,
    prefix_pattern_offsets: Vec<usize>,
    errors: Vec<Option<CheckFailedError>>,
    num_patterns: usize,
    max_suffix_patterns: usize,
}
#[cfg(test)]
impl<'a, S: fmt::Debug> fmt::Debug for DynamicPatternSetVisitor<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        f.debug_struct("DynamicPatternSetVisitor")
            .field("searcher", &self.searcher)
            .field("num_patterns", &self.num_patterns)
            .field("suffix_patterns", &self.suffix_patterns)
            .field("patterns_matched", &self.patterns_matched)
            .field("prefix_pattern_offsets", &self.prefix_pattern_offsets)
            .field("errors", &self.errors)
            .finish()
    }
}
impl<'a, 'input, S: PatternSearcher<'input>> DynamicPatternSetVisitor<'a, S> {
    /// Create a new instance of this visitor with the given prefix searcher
    /// and corresponding set of suffix patterns.
    ///
    /// This function will panic if the number of suffix pattern vectors
    /// does not match the number of prefix patterns.
    pub fn new(searcher: &'a mut S, suffix_patterns: &'a [Vec<Pattern<'a>>]) -> Self {
        let num_patterns = suffix_patterns.iter().map(|sp| sp.len()).sum();
        let num_prefixes = searcher.patterns_len();
        let max_suffix_patterns = suffix_patterns.iter().map(|sp| sp.len()).max().unwrap();

        assert_eq!(
            suffix_patterns.len(),
            num_prefixes,
            "the number of suffix patterns must be exactly equal to the number of prefix patterns"
        );

        let mut prefix_pattern_offsets = Vec::with_capacity(num_prefixes);
        let mut offset = 0;
        for sp in suffix_patterns.iter() {
            prefix_pattern_offsets.push(offset);
            offset += sp.len();
        }

        let mut errors = Vec::with_capacity(num_patterns);
        errors.resize_with(num_patterns, || None);

        Self {
            searcher,
            suffix_patterns,
            patterns_matched: vec![vec![]; num_prefixes],
            prefix_pattern_offsets,
            errors,
            num_patterns,
            max_suffix_patterns,
        }
    }

    pub fn try_match_all<'guard, 'context>(
        &mut self,
        context: &mut ContextGuard<'guard, 'input, 'context>,
    ) -> DiagResult<Matches<'input>> {
        // Until we have matched all patterns in the set:
        //
        // 1. For each unvisited prefix
        // 2. Find the next occurrance of the prefix
        // 3. Find the longest unvisited matching pattern anchored to the end of the prefix
        // 4. Mark the pattern as visited
        // 5. Repeat steps 3-4 until all patterns of the prefix have been matched, an error
        // was encountered, or the end of the input is reached. All 3 scenarios mark the
        // prefix as visited, and we start back at step 1
        // 6. When all prefixes have been visited, we determine if any of the patterns have
        // errors, or are unmatched, and if so, the entire match fails. Otherwise we return
        // a successful match result.
        let mut match_states =
            Vec::<Option<MatchState<'input>>>::with_capacity(self.max_suffix_patterns);
        match_states.resize_with(self.max_suffix_patterns, || None);
        let mut matched = Matches::with_capacity(self.num_patterns);
        loop {
            // Introduce a new binding scope that will be persisted to when a match is found
            let mut prefix_context = context.protect();
            match self.next_prefix(&mut prefix_context)? {
                // We found a match for the prefix
                ControlFlow::Continue(prefix_id) => {
                    let prefix_id = prefix_id.into_inner();
                    // Add an temporary binding scope which will be merged into
                    // the overall match bindings if we find a match at this location
                    let mut pattern_context = prefix_context.protect();
                    // Try to find a matching suffix pattern
                    match_states.clear();
                    for (index, pattern) in self.suffix_patterns[prefix_id].iter().enumerate() {
                        // Skip patterns we've already successfully matched
                        if self.patterns_matched[prefix_id]
                            .iter()
                            .any(|id| id == &index)
                        {
                            match_states.push(None);
                            continue;
                        }

                        let pattern_offset = self.prefix_pattern_offsets[prefix_id] + index;
                        match self.match_pattern(pattern, &mut pattern_context)? {
                            // A match was successful
                            Ok(info) => {
                                self.errors[pattern_offset] = None;
                                match_states.push(Some(MatchState {
                                    info,
                                    bindings: pattern_context.env_mut().take(),
                                }));
                            }
                            // We failed to match this pattern
                            Err(error) => {
                                // Remember the most recent error for matched patterns that failed
                                self.errors[pattern_offset] = Some(error);
                                match_states.push(None);
                                pattern_context.env_mut().clear();
                            }
                        }
                    }
                    // Exit the temporary scope
                    drop(pattern_context);

                    // Choose the longest match, if we have one; either way,
                    // we search for the next prefix match next
                    match self.select_longest_match(
                        prefix_id,
                        &mut match_states,
                        &mut prefix_context,
                    ) {
                        // Pattern matched, and all patterns have been matched, so we're done
                        ControlFlow::Break(info) => {
                            prefix_context.save();
                            matched.push(MatchResult::ok(info));
                            return Ok(matched);
                        }
                        // Pattern matched, but not all patterns have been matched yet
                        ControlFlow::Continue(Some(info)) => {
                            prefix_context.save();
                            matched.push(MatchResult::ok(info));
                            continue;
                        }
                        // No match at this prefix, try again
                        ControlFlow::Continue(None) => {
                            continue;
                        }
                    }
                }
                // We found a partial match for the prefix, but it failed
                // for some reason, so we skip to the next prefix, but
                // log the error
                ControlFlow::Break(Some(MatchResult {
                    ty,
                    info: Some(info),
                })) => {
                    log::debug!(target: "visitor:dynamic", "found match for prefix at offset {}, but it failed with reason: {}; ignoring the match..", info.span.start(), ty);
                }
                // We were unable to find an instance of the prefix
                ControlFlow::Break(_) => break,
            }
        }

        // No more matches are possible
        for ((prefix_id, suffix_patterns), visited) in self
            .suffix_patterns
            .iter()
            .enumerate()
            .zip(self.patterns_matched.iter())
        {
            let pattern_offset = self.prefix_pattern_offsets[prefix_id];
            for (index, (pattern, error)) in suffix_patterns
                .iter()
                .zip(self.errors[pattern_offset..].iter_mut())
                .enumerate()
            {
                if let Some(error) = error.take() {
                    matched.push(MatchResult::failed(error));
                    continue;
                }
                if visited.iter().any(|id| id == &index) {
                    continue;
                }
                let span = pattern.span();
                matched.push(MatchResult::failed(
                    CheckFailedError::MatchNoneButExpected {
                        span,
                        match_file: context.source_file(span.source_id()).unwrap(),
                        note: None,
                    },
                ));
            }
        }
        Ok(matched)
    }

    fn match_pattern<'guard, 'context>(
        &mut self,
        pattern: &'guard Pattern<'a>,
        context: &mut ContextGuard<'guard, 'input, 'context>,
    ) -> DiagResult<Result<MatchInfo<'input>, CheckFailedError>> {
        let bounds = self.searcher.input().range();
        let bounds = self.searcher.last_match_end().unwrap_or(bounds.start)..bounds.end;
        let input = Input::new(
            context.cursor().source_id,
            context.buffer(),
            context.cursor().is_crlf(),
        )
        .anchored(true)
        .bounded(bounds);
        match pattern.try_match_mut(input, context)? {
            MatchResult {
                ty: MatchType::Failed(error),
                ..
            } => Ok(Err(error)),
            MatchResult {
                info: Some(info), ..
            } => Ok(Ok(info)),
            MatchResult { .. } => unreachable!("all successful matches here must have match info"),
        }
    }

    fn next_prefix(
        &mut self,
        context: &mut ContextGuard<'_, 'input, '_>,
    ) -> DiagResult<ControlFlow<Option<MatchResult<'input>>, Span<usize>>> {
        loop {
            let result = self.searcher.try_match_next(context)?;
            if let Some(info) = result.info.as_ref() {
                // Bail with an error if we found a match but it failed for some reason
                if !result.is_ok() {
                    break Ok(ControlFlow::Break(Some(result)));
                }

                // Ignore this match if we have already visited all patterns of this prefix
                if self.is_prefix_visited(info.pattern_id) {
                    continue;
                }

                // Ignore this match if we have already matched a pattern to it at this location
                let range = info.span.into_slice_index();
                if self.patterns_matched[info.pattern_id]
                    .iter()
                    .map(|span| span.span().into_slice_index())
                    .any(|span| span.contains(&range.start) || span.contains(&range.end))
                {
                    continue;
                }

                // Bind any captured variables produced by this pattern
                {
                    let scope = context.env_mut();
                    for capture_info in info.captures.iter() {
                        match capture_info.capture {
                            Capture::Ignore(_) | Capture::All(_) => continue,
                            Capture::Implicit(tv)
                            | Capture::Explicit(tv)
                            | Capture::Mapped { with: tv, .. } => match tv.name {
                                name @ (VariableName::User(_) | VariableName::Global(_)) => {
                                    scope.insert(name, capture_info.value.clone());
                                }
                                VariableName::Pseudo(name) => {
                                    let diag = Diag::new("unsupported variable binding")
                                        .with_label(Label::new(name.span(), "occurs here"))
                                        .with_help("pseudo-variables like LINE cannot be bound");
                                    return Err(Report::new(diag));
                                }
                            },
                        }
                    }
                }

                break Ok(ControlFlow::Continue(Span::new(info.span, info.pattern_id)));
            } else {
                break Ok(ControlFlow::Break(None));
            }
        }
    }

    fn select_longest_match(
        &mut self,
        prefix_id: usize,
        match_states: &mut [Option<MatchState<'input>>],
        context: &mut ContextGuard<'_, 'input, '_>,
    ) -> ControlFlow<MatchInfo<'input>, Option<MatchInfo<'input>>> {
        use core::cmp::Ordering;

        // Choose the longest match, if we have one; either way,
        // we search for the next prefix match next
        let longest_match = match_states
            .iter_mut()
            .enumerate()
            .max_by(|(_, x), (_, y)| match (x, y) {
                (None, None) => Ordering::Equal,
                (None, Some(_)) => Ordering::Less,
                (Some(_), None) => Ordering::Greater,
                (Some(x), Some(y)) => x.cmp(y),
            });
        if let Some((index, longest_match)) =
            longest_match.and_then(|(id, lm)| lm.take().map(|lm| (id, lm)))
        {
            self.patterns_matched[prefix_id].push(Span::new(longest_match.info.span, index));
            // Restore the matched bindings, and save them to the overall match binding scope
            context.save_bindings(longest_match.bindings);

            if self.all_prefixes_visited() {
                ControlFlow::Break(longest_match.info)
            } else {
                ControlFlow::Continue(Some(longest_match.info))
            }
        } else {
            ControlFlow::Continue(None)
        }
    }

    fn all_prefixes_visited(&self) -> bool {
        self.suffix_patterns
            .iter()
            .zip(self.patterns_matched.iter())
            .all(|(sp, pv)| sp.len() == pv.len())
    }

    fn is_prefix_visited(&self, index: usize) -> bool {
        self.patterns_matched[index].len() == self.suffix_patterns[index].len()
    }
}

#[derive(Debug)]
struct MatchState<'a> {
    info: MatchInfo<'a>,
    bindings: Bindings<Value<'a>>,
}
impl<'a> Eq for MatchState<'a> {}
impl<'a> PartialEq for MatchState<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.info.span == other.info.span
    }
}
impl<'a> PartialOrd for MatchState<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<'a> Ord for MatchState<'a> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.info.span.len().cmp(&other.info.span.len())
    }
}
