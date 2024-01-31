use std::{fmt, ops::ControlFlow};

use litcheck::{
    diagnostics::{
        ArcSource, Diagnostic, FileName, NamedSourceFile, Source, SourceFile, SourceSpan, Spanned,
    },
    range::Range,
    StringInterner,
};
use smallvec::SmallVec;

use crate::{
    check::{
        matchers::{Context, MatchContext, MatchInfo, Matcher, Matches},
        Rule,
    },
    Config,
};

use super::{bytecode::*, CheckFailedError, RelatedCheckError, RelatedError};

/// Information about a successful test run
#[derive(Debug)]
pub struct TestResult {
    /// The info for each positive match (i.e. does not include CHECK-NOT)
    matches: Vec<MatchInfo<'static>>,
    /// The number of checks that passed (includes both positive and negative assertions)
    passed: usize,
    /// If the test failed, this field contains the errors associated with that failure
    error: TestFailed,
}
impl TestResult {
    pub fn new(context: &MatchContext<'_, '_>) -> Self {
        let error = TestFailed::new(vec![], context);
        Self {
            matches: vec![],
            passed: 0,
            error,
        }
    }

    pub fn from_matches(matches: Matches<'_>, context: &MatchContext<'_, '_>) -> Self {
        let mut test_result = Self::new(context);
        test_result.append(matches);
        test_result
    }

    pub fn from_error(error: TestFailed) -> Self {
        Self {
            matches: vec![],
            passed: 0,
            error,
        }
    }

    pub fn append(&mut self, matches: Matches<'_>) {
        for result in matches.into_iter() {
            match result {
                Ok(Some(info)) => self.matched(info),
                Ok(None) => self.passed(),
                Err(err) => self.failed(err),
            }
        }
    }

    pub fn matched(&mut self, matched: MatchInfo<'_>) {
        self.matches.push(matched.into_static());
        self.passed += 1;
    }

    pub fn passed(&mut self) {
        self.passed += 1;
    }

    pub fn failed(&mut self, error: CheckFailedError) {
        self.error.errors.push(error);
    }

    pub fn is_ok(&self) -> bool {
        self.error.errors.is_empty()
    }

    pub fn is_failed(&self) -> bool {
        !self.error.errors.is_empty()
    }

    pub fn num_matched(&self) -> usize {
        self.matches.len()
    }

    pub fn num_errors(&self) -> usize {
        self.error.errors.len()
    }

    pub fn errors(&self) -> &[CheckFailedError] {
        self.error.errors.as_slice()
    }

    pub fn into_result(mut self) -> Result<Vec<MatchInfo<'static>>, TestFailed> {
        if self.is_ok() {
            self.matches
                .sort_by(|a, b| a.pattern_span.start().cmp(&b.pattern_span.start()));
            Ok(self.matches)
        } else {
            Err(self.error)
        }
    }

    pub fn unwrap_err(self) -> TestFailed {
        if self.is_ok() {
            self.error
        } else {
            panic!(
                "attempted to unwrap error when test was successful: {:#?}",
                self
            );
        }
    }
}

#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("{test_from} failed")]
#[diagnostic(help("see emitted diagnostics for details"))]
pub struct TestFailed {
    pub test_from: TestInputType,
    #[label]
    pub span: SourceSpan,
    #[source_code]
    pub input: ArcSource,
    #[related]
    pub errors: Vec<CheckFailedError>,
}
impl TestFailed {
    pub fn new(errors: Vec<CheckFailedError>, context: &MatchContext<'_, '_>) -> Self {
        let input_file = context.input_file();
        Self {
            test_from: TestInputType(context.match_file().name()),
            span: input_file.span(),
            input: input_file.clone(),
            errors,
        }
    }

    pub fn errors(&self) -> &[CheckFailedError] {
        self.errors.as_slice()
    }
}

#[derive(Debug)]
pub struct TestInputType(FileName);
impl fmt::Display for TestInputType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            FileName::Stdin => f.write_str("test from standard input"),
            FileName::Path(path) => write!(f, "test at {}", path.display()),
            FileName::Virtual(name) => write!(f, "test '{name}'"),
        }
    }
}

pub struct Checker<'a> {
    config: &'a Config,
    interner: &'a mut StringInterner,
    program: CheckProgram<'a>,
    match_file: ArcSource,
}
impl<'a> Checker<'a> {
    pub fn new(
        config: &'a Config,
        interner: &'a mut StringInterner,
        program: CheckProgram<'a>,
        match_file: ArcSource,
    ) -> Self {
        Self {
            config,
            interner,
            program,
            match_file,
        }
    }

    /// Check `source` against the rules in this [Checker]
    pub fn check<S>(&mut self, source: &S) -> TestResult
    where
        S: NamedSourceFile + ?Sized,
    {
        let source = ArcSource::new(Source::new(source.name(), source.source().to_string()));
        self.check_input(source)
    }

    /// Check `input` against the rules in this [Checker]
    pub fn check_str<S>(&mut self, input: &S) -> TestResult
    where
        S: AsRef<str>,
    {
        let source = ArcSource::new(Source::from(input.as_ref().to_string()));
        self.check_input(source)
    }

    /// Check `source` against the rules in this [Checker]
    pub fn check_input(&mut self, source: ArcSource) -> TestResult {
        let buffer = source.source_bytes();
        let mut context = MatchContext::new(
            self.config,
            self.interner,
            self.match_file.clone(),
            source.clone(),
            buffer,
        );
        match analyze_blocks(&self.program, &mut context) {
            Ok(blocks) => check_blocks(blocks, &self.program, &mut context),
            Err(failed) => TestResult::from_error(failed),
        }
    }
}

/// Analyze `program` to identify distinct "blocks" of checks, which
/// are implicitly delimited by CHECK-LABEL directives.
///
/// This will evaluate the CHECK-LABEL patterns, and construct block
/// metadata for each bounded set of checks to be used during evaluation
/// of the remaining directives in the file.
pub fn analyze_blocks<'input, 'context: 'input>(
    program: &CheckProgram<'context>,
    context: &mut MatchContext<'input, 'context>,
) -> Result<SmallVec<[BlockInfo; 2]>, TestFailed> {
    // Traverse the code of the program and try to identify
    // the byte ranges corresponding to block starts of the
    // program. Once the block starts are recorded, we can
    // properly constrain the search bounds of the matchers
    let mut blocks = SmallVec::<[BlockInfo; 2]>::default();
    let mut errors = Vec::<CheckFailedError>::new();
    // We must push an implicit block for ops that come before the first CHECK-LABEL,
    // if such ops exist
    let eof = context.cursor().end_of_file();
    if !matches!(program.code.first(), Some(CheckOp::BlockStart(_))) {
        blocks.push(BlockInfo {
            label_info: None,
            code_start: Some(0),
            start: Some(0),
            end: None,
            eof,
        });
    }

    for (i, op) in program.code.iter().enumerate() {
        if let CheckOp::BlockStart(ref pattern) = op {
            // Push a block for this CHECK-LABEL
            let ix = match program.code.get(i + 1) {
                Some(CheckOp::BlockStart(_)) | None => None,
                Some(_) => Some(i + 1),
            };
            // Find the starting index of this pattern
            //
            // If no match is found, record the error,
            // and skip ahead in the instruction stream
            let buffer = context.search_to_end();
            match pattern.try_match(buffer, context) {
                Ok(result) => {
                    match result.info {
                        Some(info) => {
                            // We must compute the indices for the end of the previous block,
                            // and the start of the current block, by looking forwards/backwards
                            // for the nearest newlines in those directions.
                            let match_start = info.span.offset();
                            let cursor = context.cursor_mut();
                            let eol = cursor.next_newline_from(match_start).unwrap_or(eof);
                            let prev_block_end = cursor.prev_newline_from(match_start).unwrap_or(0);
                            // Start subsequent searches at the newline, to ensure that rules which
                            // match on next lines can eat the first newline, but prevent any further
                            // matching of rules on this line
                            cursor.set_start(eol);
                            // Create the block info for this CHECK-LABEL to record the start index
                            let block_id = blocks.len();
                            blocks.push(BlockInfo {
                                label_info: Some(info.into_static()),
                                code_start: ix,
                                start: Some(cursor.start()),
                                end: None,
                                eof,
                            });
                            // Update the block info for the most recent CHECK-LABEL to record its end index,
                            // if one is present
                            if let Some(prev_block) = blocks[..block_id]
                                .iter_mut()
                                .rev()
                                .find(|bi| bi.start.is_some())
                            {
                                prev_block.end = Some(prev_block_end);
                            }
                        }
                        None => {
                            // The current block could not be found, so record an error
                            blocks.push(BlockInfo {
                                label_info: None,
                                code_start: ix,
                                start: None,
                                end: None,
                                eof,
                            });
                            let span = pattern.span();
                            let msg = format!(
                                "Unable to find a match for this pattern in the input.\
                            Search started at byte {}, ending at {eof}",
                                context.cursor().start()
                            );
                            errors.push(CheckFailedError::MatchNoneButExpected {
                                span,
                                match_file: context.match_file(),
                                note: Some(msg),
                            });
                        }
                    }
                }
                Err(err) => {
                    blocks.push(BlockInfo {
                        label_info: None,
                        code_start: ix,
                        start: None,
                        end: None,
                        eof,
                    });
                    errors.push(CheckFailedError::MatchNoneForInvalidPattern {
                        span: pattern.span(),
                        match_file: context.match_file(),
                        error: Some(RelatedError::new(err)),
                    });
                }
            }
        }
    }

    // Reset the context bounds
    context.cursor_mut().reset();

    if errors.is_empty() {
        Ok(blocks)
    } else {
        Err(TestFailed::new(errors, context))
    }
}

/// Evaluate all check operations in the given blocks
pub fn check_blocks<'input, 'a: 'input, I>(
    blocks: I,
    program: &CheckProgram<'a>,
    context: &mut MatchContext<'input, 'a>,
) -> TestResult
where
    I: IntoIterator<Item = BlockInfo>,
{
    // Execute compiled check program now that we have the blocks identified.
    //
    // If we encounter a block which was not found in the check file, all ops
    // up to the next CHECK-LABEL are skipped.
    //
    // Each time a CHECK-LABEL is found, we set the search bounds of the match
    // context to stay within that region. If --enable-var-scopes is set, the
    // locals bound so far will be cleared
    let mut test_result = TestResult::new(context);
    for mut block in blocks.into_iter() {
        if let Some(label_info) = block.label_info.take() {
            test_result.matched(label_info);
        }
        if block.start.is_none() {
            continue;
        }
        if block.code_start.is_none() {
            continue;
        }

        // Update the match context cursor
        context.enter_block(block.range());
        let code = &program.code[unsafe { block.code_start.unwrap_unchecked() }..];
        check_block(code, &mut test_result, context);
    }

    test_result
}

/// Evaluate a block of check operations with the given context,
/// gathering any errors encountered into `errors`.
pub fn check_block<'input, 'a: 'input>(
    code: &[CheckOp<'a>],
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) {
    for op in code.iter() {
        if let ControlFlow::Break(_) = check_op(op, test_result, context) {
            break;
        }

        if context.cursor().is_empty() {
            break;
        }
    }
}

/// Evaluate a single check operation with the given context
pub fn check_op<'input, 'a: 'input>(
    op: &CheckOp<'a>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) -> ControlFlow<()> {
    match dbg!(op) {
        CheckOp::BlockStart(_) => {
            // We have reached the end of the current CHECK-LABEL block,
            // move to the next block where we will handle updating the
            // context as appropriate
            return ControlFlow::Break(());
        }
        CheckOp::ApplyRule(rule) => match rule.apply(context) {
            Ok(matches) => {
                test_result.append(matches);
            }
            Err(err) => {
                test_result.failed(CheckFailedError::MatchNoneForInvalidPattern {
                    span: rule.span(),
                    match_file: context.match_file(),
                    error: Some(RelatedError::new(err)),
                });
            }
        },
        CheckOp::ApplyRulePrecededBy(rule, preceded_by) => {
            let initial_cursor = context.cursor().position();
            let mut rule_context = context.protect();
            match preceded_by.apply(&mut rule_context) {
                Ok(preceding_matches) if preceding_matches.is_ok() => {
                    let next_cursor = rule_context.cursor().position();
                    rule_context.move_to(initial_cursor);
                    match rule.apply(&mut rule_context) {
                        Ok(matches) if matches.is_ok() => {
                            // Find the set of preceded-by matches which overlap with `matches`
                            let boundary = matches
                                .iter()
                                .filter_map(|mr| mr.info.as_ref())
                                .map(|info| info.span.start())
                                .min()
                                .unwrap_or(next_cursor.range.start);
                            let mut is_ok = true;
                            for result in preceding_matches.into_iter() {
                                if let Some(info) = result.unwrap() {
                                    // The matches in the preceded by rule passed by `rule`,
                                    // which is not allowed, so we reject those matches and
                                    // treat it as a failure
                                    if info.span.start() >= boundary {
                                        is_ok = false;
                                        test_result.failed(CheckFailedError::MatchFoundButDiscarded {
                                            span: info.span,
                                            input_file: rule_context.input_file(),
                                            pattern: Some(RelatedCheckError {
                                                span: preceded_by.span(),
                                                match_file: rule_context.match_file(),
                                            }),
                                            note: Some(format!(
                                                "match was found after {} rule it was expected to precede",
                                                rule.kind()
                                            )),
                                        });
                                    } else {
                                        test_result.matched(info);
                                    }
                                } else {
                                    test_result.passed();
                                }
                            }
                            test_result.append(matches);
                            if is_ok {
                                // Persist changes to the context and drop the guard
                                rule_context.save();
                            } else {
                                drop(rule_context);
                                context.cursor_mut().move_to_end();
                            }
                        }
                        Ok(matches) => {
                            test_result.append(preceding_matches);
                            test_result.append(matches);
                            drop(rule_context);
                            context.cursor_mut().move_to_end();
                        }
                        Err(err) => {
                            drop(rule_context);
                            test_result.failed(CheckFailedError::MatchNoneForInvalidPattern {
                                span: rule.span(),
                                match_file: context.match_file(),
                                error: Some(RelatedError::new(err)),
                            });
                            context.cursor_mut().move_to_end();
                        }
                    }
                }
                Ok(preceding_matches) => {
                    drop(rule_context);
                    context.cursor_mut().move_to_end();
                    test_result.append(preceding_matches);
                }
                Err(err) => {
                    drop(rule_context);
                    context.cursor_mut().move_to_end();
                    test_result.failed(CheckFailedError::MatchNoneForInvalidPattern {
                        span: preceded_by.span(),
                        match_file: context.match_file(),
                        error: Some(RelatedError::new(err)),
                    });
                }
            }
        }
        CheckOp::RepeatRule(_rule, _count) => todo!(),
        CheckOp::RepeatRulePrecededBy(_rule, _count, _preceded_by) => todo!(),
    }

    ControlFlow::Continue(())
}

#[derive(Debug)]
pub struct BlockInfo {
    /// The span of the CHECK-LABEL span which started this block, if applicable
    #[allow(unused)]
    pub label_info: Option<MatchInfo<'static>>,
    /// The instruction index in the check program code for the first non-CHECK-LABEL
    /// instruction in the block, if any.
    pub code_start: Option<usize>,
    /// The starting byte index of the block, if known.
    ///
    /// The index begins at the start of the next line following the CHECK-LABEL line
    ///
    /// If None, the CHECK-LABEL pattern was never found; `end` must also be None
    /// in that case
    pub start: Option<usize>,
    /// The ending byte index of the block, if known.
    ///
    /// The index ends at the last byte of the line preceding a subsequent CHECK-LABEL.
    ///
    /// If subsequent blocks with start indices are recorded, this must be Some with
    /// an end index relative to the next start index in ascending order.
    pub end: Option<usize>,
    /// The end-of-file index
    pub eof: usize,
}
impl BlockInfo {
    pub fn range(&self) -> Range<usize> {
        Range::new(self.start.unwrap_or(0), self.end.unwrap_or(self.eof))
    }

    #[cfg(test)]
    pub fn code_start(&self) -> usize {
        self.code_start.unwrap_or(0)
    }
}
