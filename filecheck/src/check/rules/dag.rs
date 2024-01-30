use std::{collections::BTreeMap, ops::ControlFlow};

use litcheck::{
    diagnostics::{DiagResult, SourceSpan, Span, Spanned},
    Symbol,
};

use crate::{
    check::{
        matchers::{Context, ContextGuard, MatchInfo, MatchResult, MatcherMut},
        pattern::PatternIdentifier,
        searcher::{Input as SearcherInput, Match as SearcherMatch, PatternSetSearcher, Searcher},
        Check, CheckFailedError, Input, MatchAll, MatchAny, MatchType, Pattern,
    },
    expr::Value,
};

use super::*;

#[derive(Debug)]
struct MatchState<'a> {
    info: MatchInfo<'a>,
    bindings: BTreeMap<Symbol, Value<'a>>,
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

struct PatternVisitor<'a, S: PatternSetSearcher> {
    searcher: &'a mut S,
    visited: Vec<<<S as Searcher>::Match as SearcherMatch>::PatternID>,
    errors: Vec<Option<CheckFailedError>>,
    first_match: Option<SourceSpan>,
}
#[cfg(test)]
impl<'a, S: PatternSetSearcher + std::fmt::Debug> std::fmt::Debug for PatternVisitor<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("PatternVisitor")
            .field("searcher", &self.searcher)
            .field("visited", &self.visited)
            .field("errors", &self.errors)
            .field("first_match", &self.first_match)
            .finish()
    }
}
impl<'a, S: PatternSetSearcher> PatternVisitor<'a, S> {
    fn new(searcher: &'a mut S) -> Self {
        let num_patterns = searcher.patterns_len();

        let mut errors = Vec::with_capacity(num_patterns);
        errors.resize_with(num_patterns, || None);
        Self {
            searcher,
            visited: Vec::with_capacity(num_patterns),
            errors,
            first_match: None,
        }
    }

    fn patterns_len(&self) -> usize {
        self.searcher.patterns_len()
    }

    fn pattern_span(&self, id: <<S as Searcher>::Match as SearcherMatch>::PatternID) -> SourceSpan {
        self.searcher.pattern_span(id)
    }

    pub fn try_match_all<'input>(
        &mut self,
        context: &mut ContextGuard<'_, 'input, '_>,
    ) -> DiagResult<MatchResult<'input>> {
        loop {
            let search_guard = context.protect();
            let result = self.searcher.try_match_next(&search_guard)?;
            if let Some(info) = result.info.as_ref() {
                let pid = <<<S as Searcher>::Match as SearcherMatch>::PatternID as PatternIdentifier>::new(info.pattern_id)
                    .unwrap();
                // Ignore this match if we have already visited this pattern
                if self.visited.contains(&pid) {
                    continue;
                }
                // Record that we've matched this pattern
                self.visited.push(pid);
                // Save any local bindings produced during this match
                search_guard.save();
                // If we've matched all patterns, we're done, otherwise keep searching
                let first_match = *self.first_match.get_or_insert(info.span);
                if self.patterns_len() == self.visited.len() {
                    let start = first_match.offset();
                    break Ok(MatchResult::ok(MatchInfo::new(
                        start..info.span.end(),
                        info.pattern_span,
                    )));
                }
            } else {
                // No more matches are possible
                let mut failed = Vec::with_capacity(self.patterns_len() - self.visited.len());
                let errors = core::mem::take(&mut self.errors);
                for (pattern_id, mut error) in errors.into_iter().enumerate() {
                    if let Some(error) = error.take() {
                        failed.push(error);
                        continue;
                    }
                    let pid = <<<S as Searcher>::Match as SearcherMatch>::PatternID as PatternIdentifier>::new_unchecked(
                        pattern_id,
                    );
                    if self.visited.contains(&pid) {
                        continue;
                    }
                    let span = self.pattern_span(pid);
                    failed.push(CheckFailedError::MatchNoneButExpected {
                        span,
                        match_file: context.match_file(),
                        note: None,
                    });
                }
                break Ok(MatchResult::failed(CheckFailedError::MatchAllFailed {
                    failed,
                }));
            }
        }
    }
}

struct SubPatternVisitor<'a, S: PatternSetSearcher> {
    searcher: &'a mut S,
    subpatterns: &'a [Vec<Pattern<'a>>],
    patterns_matched: Vec<Vec<Span<usize>>>,
    prefix_pattern_offsets: Vec<usize>,
    errors: Vec<Option<CheckFailedError>>,
    num_patterns: usize,
    max_subpatterns: usize,
}
impl<'a, S: PatternSetSearcher + std::fmt::Debug> std::fmt::Debug for SubPatternVisitor<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("SubPatternVisitor")
            .field("searcher", &self.searcher)
            .field("num_patterns", &self.num_patterns)
            .field("subpatterns", &self.subpatterns)
            .field("patterns_matched", &self.patterns_matched)
            .field("prefix_pattern_offsets", &self.prefix_pattern_offsets)
            .field("errors", &self.errors)
            .finish()
    }
}
impl<'a, S: PatternSetSearcher> SubPatternVisitor<'a, S> {
    fn new_with_subpatterns(searcher: &'a mut S, subpatterns: &'a [Vec<Pattern<'a>>]) -> Self {
        let num_patterns = subpatterns.iter().map(|sp| sp.len()).sum();
        let num_prefixes = searcher.patterns_len();
        let max_subpatterns = subpatterns.iter().map(|sp| sp.len()).max().unwrap();

        assert_eq!(
            subpatterns.len(),
            num_prefixes,
            "the number of subpatterns must be exactly equal to the number of prefix patterns"
        );

        let mut prefix_pattern_offsets = Vec::with_capacity(num_prefixes);
        let mut offset = 0;
        for sp in subpatterns.iter() {
            prefix_pattern_offsets.push(offset);
            offset += sp.len();
        }

        let mut errors = Vec::with_capacity(num_patterns);
        errors.resize_with(num_patterns, || None);

        Self {
            searcher,
            subpatterns,
            patterns_matched: vec![vec![]; num_prefixes],
            prefix_pattern_offsets,
            errors,
            num_patterns,
            max_subpatterns,
        }
    }

    fn pattern_span(&self) -> SourceSpan {
        let start = self
            .subpatterns
            .iter()
            .flat_map(|p| p.iter().map(|p| p.start()))
            .min()
            .unwrap();
        let end = self
            .subpatterns
            .iter()
            .flat_map(|p| p.iter().map(|p| p.end()))
            .max()
            .unwrap();
        SourceSpan::from(start..end)
    }

    fn all_prefixes_visited(&self) -> bool {
        self.subpatterns
            .iter()
            .zip(self.patterns_matched.iter())
            .all(|(sp, pv)| sp.len() == pv.len())
    }

    fn is_prefix_visited(&self, index: usize) -> bool {
        self.patterns_matched[index].len() == self.subpatterns[index].len()
    }

    pub fn try_match_all<'guard, 'input, 'context>(
        &mut self,
        context: &mut ContextGuard<'guard, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
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
            Vec::<Option<MatchState<'input>>>::with_capacity(self.max_subpatterns);
        match_states.resize_with(self.max_subpatterns, || None);
        loop {
            // Introduce a new binding scope that will be persisted to when a match is found
            let mut prefix_context = context.protect();
            match dbg!(self.next_prefix(&mut prefix_context))? {
                // We found a match for the prefix
                ControlFlow::Continue(prefix_id) => {
                    let prefix_id = prefix_id.into_inner();
                    // Save the searcher state so it can be restored if we fail to match
                    // at this location
                    let searcher_snapshot = self.searcher.last_match_end();
                    // Add an temporary binding scope which will be merged into
                    // the overall match bindings if we find a match at this location
                    let mut pattern_context = prefix_context.protect();
                    let start_pos = pattern_context.cursor().position();
                    // Try to find a matching subpattern
                    match_states.clear();
                    for (index, pattern) in self.subpatterns[prefix_id].iter().enumerate() {
                        // Skip patterns we've already successfully matched
                        if self.patterns_matched[prefix_id]
                            .iter()
                            .any(|id| id == &index)
                        {
                            match_states.push(None);
                            continue;
                        }

                        let pattern_offset = self.prefix_pattern_offsets[prefix_id] + index;
                        dbg!(
                            index,
                            pattern_offset,
                            prefix_id,
                            searcher_snapshot,
                            &pattern
                        );
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
                        pattern_context.cursor_mut().move_to(start_pos);
                        if let Some(end) = searcher_snapshot {
                            self.searcher.set_last_match_end(end);
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
                        ControlFlow::Break(_) => {
                            prefix_context.save();
                            let start = self
                                .patterns_matched
                                .iter()
                                .map(|sp| sp.iter().map(|span| span.start()).min().unwrap())
                                .min()
                                .unwrap();
                            let end = self
                                .patterns_matched
                                .iter()
                                .map(|sp| sp.iter().map(|span| span.end()).max().unwrap())
                                .max()
                                .unwrap();
                            return Ok(MatchResult::ok(MatchInfo::new(
                                start..end,
                                self.pattern_span(),
                            )));
                        }
                        // Pattern matched, but not all patterns have been matched yet
                        ControlFlow::Continue(Some(_)) => {
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
                    log::debug!("found match for prefix at offset {}, but it failed with reason: {}; ignoring the match..", info.span.start(), ty);
                }
                // We were unable to find an instance of the prefix
                ControlFlow::Break(_) => break,
            }
        }

        // No more matches are possible
        let mut failed = Vec::with_capacity(
            self.num_patterns
                - self
                    .patterns_matched
                    .iter()
                    .map(|sp| sp.len())
                    .sum::<usize>(),
        );
        for ((prefix_id, subpatterns), visited) in self
            .subpatterns
            .iter()
            .enumerate()
            .zip(self.patterns_matched.iter())
        {
            let pattern_offset = self.prefix_pattern_offsets[prefix_id];
            for (index, (pattern, error)) in subpatterns
                .iter()
                .zip(self.errors[pattern_offset..].iter_mut())
                .enumerate()
            {
                let pattern_id = pattern_offset + index;
                if let Some(error) = error.take() {
                    failed.push(error);
                    continue;
                }
                if visited.iter().any(|id| id == &pattern_id) {
                    continue;
                }
                let span = pattern.span();
                failed.push(CheckFailedError::MatchNoneButExpected {
                    span,
                    match_file: context.match_file(),
                    note: None,
                });
            }
        }
        Ok(MatchResult::failed(CheckFailedError::MatchAllFailed {
            failed,
        }))
    }

    fn match_pattern<'guard, 'input, 'context>(
        &mut self,
        pattern: &'guard Pattern<'a>,
        context: &mut ContextGuard<'guard, 'input, 'context>,
    ) -> DiagResult<Result<MatchInfo<'input>, CheckFailedError>> {
        let input = self.searcher.input();
        let input = Input::new(context.cursor().buffer(), false)
            .anchored(true)
            .span(input.range());
        dbg!(input.bounds());
        match dbg!(pattern.try_match_mut(input, context))? {
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

    fn next_prefix<'input>(
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
                let range = info.span.range();
                if self.patterns_matched[info.pattern_id]
                    .iter()
                    .map(|span| span.range())
                    .any(|span| span.contains(&range.start) || span.contains(&range.end))
                {
                    continue;
                }

                break Ok(ControlFlow::Continue(Span::new(info.span, info.pattern_id)));
            } else {
                break Ok(ControlFlow::Break(None));
            }
        }
    }

    fn select_longest_match<'input>(
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
        dbg!(&longest_match);
        if let Some((index, longest_match)) =
            longest_match.and_then(|(id, lm)| lm.take().map(|lm| (id, lm)))
        {
            dbg!(prefix_id, index);
            self.patterns_matched[prefix_id].push(Span::new(longest_match.info.span, index));
            // Restore the matched bindings, and save them to the overall match binding scope
            context.extend_locals(longest_match.bindings);

            if self.all_prefixes_visited() {
                ControlFlow::Break(longest_match.info)
            } else {
                ControlFlow::Continue(Some(longest_match.info))
            }
        } else {
            ControlFlow::Continue(None)
        }
    }
}

#[derive(Debug)]
pub struct CheckDag<'a> {
    patterns: MatchAll<'a>,
}
impl<'a> CheckDag<'a> {
    pub fn new(patterns: MatchAll<'a>) -> Self {
        Self { patterns }
    }
}
impl<'check> Rule for CheckDag<'check> {
    fn kind(&self) -> Check {
        Check::Dag
    }

    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut guard = context.protect();
        let input = guard.search_block();
        match self.patterns {
            MatchAll::Literal(ref matcher) => {
                let mut searcher = matcher.search(input)?;
                let mut visitor = PatternVisitor::new(&mut searcher);
                match visitor.try_match_all(&mut guard) {
                    Ok(result) if result.is_ok() => {
                        guard.save();
                        Ok(result)
                    }
                    Ok(result) => Ok(result),
                    Err(error) => Err(error),
                }
            }
            MatchAll::Regex(ref matcher) => {
                let mut searcher = matcher.search(input);
                let mut visitor = PatternVisitor::new(&mut searcher);
                match visitor.try_match_all(&mut guard) {
                    Ok(result) if result.is_ok() => {
                        guard.save();
                        Ok(result)
                    }
                    Ok(result) => Ok(result),
                    Err(error) => Err(error),
                }
            }
            MatchAll::Smart {
                ref searcher,
                ref patterns,
                ..
            } => {
                let result = match searcher {
                    MatchAny::Literal(ref matcher) => {
                        let mut searcher = matcher.search(input)?;
                        let mut visitor =
                            SubPatternVisitor::new_with_subpatterns(&mut searcher, patterns);
                        visitor.try_match_all(&mut guard)
                    }
                    MatchAny::Regex(ref matcher) => {
                        let mut searcher = matcher.search(input);
                        let mut visitor =
                            SubPatternVisitor::new_with_subpatterns(&mut searcher, patterns);
                        visitor.try_match_all(&mut guard)
                    }
                };
                match result {
                    Ok(result) if result.is_ok() => {
                        guard.save();
                        Ok(result)
                    }
                    Ok(result) => Ok(result),
                    Err(error) => Err(error),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::matchers::*;
    use crate::check::CheckFailedError;
    use crate::testing::TestContext;
    use litcheck::diagnostics::Report;
    use std::collections::VecDeque;

    /// This test ensures that the ordering of CHECK-DAG patterns is not
    /// significant even in the presence of a common prefix. The test can
    /// only pass if this is true, as line matched by the first directive
    /// appears after the line matched by the second.
    #[test]
    fn check_dag_common_prefix_non_overlapping_patterns_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: v[[#]] = add v9, v0
CHECK-DAG: v[[#]] = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let lines = check_file.into_lines();

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        match result {
            MatchResult {
                ty: MatchType::Failed(err),
                ..
            } => {
                return Err(Report::new(err));
            }
            result => {
                assert!(result.is_ok());
            }
        }

        Ok(())
    }

    /// In this test, the first of the two CHECK-DAG directives can match the
    /// same content as the second, while the second only has one possible match.
    /// This test can only succeed if the ordering of the directives is not significant,
    /// at least in this specific case. However, the catch here is that the reason the
    /// test does not fail is because we always take the longest match first, so even
    /// though the first directive would match the only line that the second can match,
    /// the second directive wins because it matches a longer span of input.
    #[test]
    fn check_dag_overlapping_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-LABEL: block0:
CHECK-DAG: v[[#]] = {{[a-z]+[.][a-z]+}}
CHECK-DAG: v[[#]] = const.i32 0
CHECK-LABEL: block1:
CHECK: br block2(v[[#]], v[[#]])
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let mut lines = VecDeque::from(check_file.into_lines());
        lines.pop_back();
        lines.pop_back();
        lines.pop_front();
        let lines = Vec::from(lines);

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        match result {
            MatchResult {
                ty: MatchType::Failed(err),
                ..
            } => {
                return Err(Report::new(err));
            }
            result => {
                assert!(result.is_ok());
            }
        }

        Ok(())
    }

    #[test]
    #[ignore]
    fn check_dag_check_label_boundaries_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: v[[#]] = add v9, v0
CHECK-DAG: v[[#]] = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let lines = check_file.into_lines();

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        match result {
            MatchResult {
                ty: MatchType::Failed(err),
                ..
            } => Err(Report::new(err)),
            result => {
                dbg!(&result);
                assert!(result.is_ok());
                let info = result.info.unwrap();
                Err(Report::new(CheckFailedError::MatchFoundErrorNote {
                    span: info.span,
                    input_file: mctx.input_file(),
                    pattern: Some(crate::check::RelatedCheckError {
                        span: info.pattern_span,
                        match_file: mctx.match_file(),
                    }),
                    help: None,
                }))
            }
        }
    }
}
