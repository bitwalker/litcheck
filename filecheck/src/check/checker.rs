use litcheck::{
    diagnostics::{
        ArcSource, Diagnostic, FileName, NamedSourceFile, Source, SourceFile, SourceSpan, Spanned,
    },
    StringInterner,
};
use smallvec::SmallVec;
use std::fmt;

use crate::{
    check::{
        matchers::{Context, MatchContext, Matcher},
        Rule,
    },
    Config,
};

use super::{bytecode::*, CheckFailedError, RelatedCheckError, RelatedError};

#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("{test_from} failed")]
#[diagnostic(help("see emitted diagnostics for details"))]
pub struct TestFailed {
    test_from: TestInputType,
    #[label]
    span: SourceSpan,
    #[source_code]
    input: ArcSource,
    #[related]
    errors: Vec<CheckFailedError>,
}
impl TestFailed {
    pub fn errors(&self) -> &[CheckFailedError] {
        self.errors.as_slice()
    }
}

#[derive(Debug)]
struct TestInputType(FileName);
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
    errors: Vec<CheckFailedError>,
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
            errors: vec![],
        }
    }

    pub fn check<S>(&mut self, source: &S) -> Result<(), TestFailed>
    where
        S: NamedSourceFile + ?Sized,
    {
        self.check_all(ArcSource::new(Source::new(
            source.name(),
            source.source().to_string(),
        )))
    }

    pub fn check_str<S>(&mut self, source: &S) -> Result<(), TestFailed>
    where
        S: SourceFile + ?Sized,
    {
        self.check_all(ArcSource::new(Source::from(source.source().to_string())))
    }

    pub fn check_all(&mut self, input_file: ArcSource) -> Result<(), TestFailed> {
        // Initialize context
        let buffer = input_file.source_bytes();
        let buffer_len = buffer.len();

        let mut context = MatchContext::new(
            self.config,
            self.interner,
            self.match_file.clone(),
            input_file.clone(),
            buffer,
        );

        // Traverse the code of the program and try to identify
        // the byte ranges corresponding to block starts of the
        // program. Once the block starts are recorded, we can
        // properly constrain the search bounds of the matchers
        let mut blocks = SmallVec::<[BlockInfo; 2]>::default();
        // We must push an implicit block for ops that come before the first CHECK-LABEL,
        // if such ops exist
        if !matches!(self.program.code.first(), Some(CheckOp::BlockStart(_))) {
            blocks.push(BlockInfo {
                label_span: None,
                code_start: Some(0),
                start: Some(0),
                end: None,
            });
        }
        for (i, op) in self.program.code.iter().enumerate() {
            if let CheckOp::BlockStart(ref pattern) = op {
                // Push a block for this CHECK-LABEL
                let block_id = blocks.len();
                let ix = match self.program.code.get(i + 1) {
                    Some(CheckOp::BlockStart(_)) | None => None,
                    Some(_) => Some(i + 1),
                };
                blocks.push(BlockInfo {
                    label_span: Some(pattern.span()),
                    code_start: ix,
                    start: None,
                    end: None,
                });
                // Find the starting index of this pattern
                //
                // If no match is found, record the error,
                // and skip ahead in the instruction stream
                let buffer = context.search_to_end();
                match pattern.try_match(buffer, &context) {
                    Ok(result) => {
                        match result.info {
                            Some(info) => {
                                // We must compute the indices for the end of the previous block,
                                // and the start of the current block, by looking forwards/backwards
                                // for the nearest newlines in those directions.
                                let match_start = info.span.offset();
                                let cursor = context.cursor_mut();
                                let eol =
                                    cursor.next_newline_from(match_start).unwrap_or(buffer_len);
                                let prev_block_end =
                                    cursor.prev_newline_from(match_start).unwrap_or(0);
                                // Start subsequent searches at the newline, to ensure that rules which
                                // match on next lines can eat the first newline, but prevent any further
                                // matching of rules on this line
                                cursor.set_start(eol);
                                // Update the block info for this CHECK-LABEL to record the start index
                                blocks[block_id].start = Some(cursor.start());
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
                                let span = pattern.span();
                                let msg = format!(
                                    "Unable to find a match for this pattern in the input.\
                                Search started at byte {}, ending at {buffer_len}",
                                    context.cursor().start()
                                );
                                self.errors.push(CheckFailedError::MatchNoneButExpected {
                                    span,
                                    match_file: self.match_file.clone(),
                                    note: Some(msg),
                                });
                            }
                        }
                    }
                    Err(err) => {
                        self.errors
                            .push(CheckFailedError::MatchNoneForInvalidPattern {
                                span: pattern.span(),
                                match_file: context.match_file.clone(),
                                error: Some(RelatedError::new(err)),
                            });
                    }
                }
            }
        }

        // Reset the context bounds
        context.cursor_mut().reset();

        // Execute compiled check program now that we have the blocks identified.
        //
        // If we encounter a block which was not found in the check file, all ops
        // up to the next CHECK-LABEL are skipped.
        //
        // Each time a CHECK-LABEL is found, we set the search bounds of the match
        // context to stay within that region. If --enable-var-scopes is set, the
        // locals bound so far will be cleared
        let mut ix;
        let mut block_id = 0;
        'next_block: while let Some(block) = blocks.get(block_id) {
            if block.start.is_none() {
                block_id += 1;
                continue;
            }
            if block.code_start.is_none() {
                block_id += 1;
                continue;
            }

            // Update the match context
            context.enter_block(block.start.unwrap_or(0)..block.end.unwrap_or(buffer_len));

            ix = unsafe { block.code_start.unwrap_unchecked() };
            while let Some(op) = self.program.code.get(ix) {
                ix += 1;
                match op {
                    CheckOp::BlockStart(_) => {
                        // We have reached the end of the current CHECK-LABEL block,
                        // move to the next block where we will handle updating the
                        // context as appropriate
                        block_id += 1;
                        continue 'next_block;
                    }
                    CheckOp::ApplyRule(rule) => match rule.apply(&mut context) {
                        Ok(result) if result.is_ok() => (),
                        Ok(result) => {
                            self.errors.push(result.unwrap_err());
                        }
                        Err(err) => {
                            self.errors
                                .push(CheckFailedError::MatchNoneForInvalidPattern {
                                    span: rule.span(),
                                    match_file: self.match_file.clone(),
                                    error: Some(RelatedError::new(err)),
                                });
                        }
                    },
                    CheckOp::ApplyRulePrecededBy(rule, preceded_by) => {
                        let initial_cursor = context.cursor().position();
                        let mut rule_context = context.protect();
                        match preceded_by.apply(&mut rule_context) {
                            Ok(result) if result.is_ok() => {
                                let next_cursor = rule_context.cursor().position();
                                rule_context.move_to(initial_cursor);
                                match rule.apply(&mut rule_context) {
                                    Ok(next_result) if next_result.is_ok() => {
                                        let offset =
                                            next_result.info.as_ref().unwrap().span.start();
                                        let skipped =
                                            initial_cursor.range.start..next_cursor.range.start;
                                        if skipped.contains(&offset) {
                                            // The matches in the preceded by rule passed by `rule`,
                                            // which is not allowed, so we reject those matches and
                                            // treat it as a failure
                                            self.errors.push(CheckFailedError::MatchFoundButDiscarded {
                                                span: result.info.unwrap().span,
                                                input_file: rule_context.input_file(),
                                                pattern: Some(RelatedCheckError {
                                                    span: preceded_by.span(),
                                                    match_file: self.match_file.clone(),
                                                }),
                                                note: Some(format!("match was found after {} rule it was expected to precede", rule.kind())),
                                            });
                                            drop(rule_context);
                                            context.cursor_mut().move_to_end();
                                            continue;
                                        }
                                        // Persist changes to the context and drop the guard
                                        rule_context.save();
                                    }
                                    Ok(next_result) => {
                                        self.errors.push(next_result.unwrap_err());
                                        drop(rule_context);
                                        context.cursor_mut().move_to_end();
                                    }
                                    Err(err) => {
                                        self.errors.push(
                                            CheckFailedError::MatchNoneForInvalidPattern {
                                                span: rule.span(),
                                                match_file: self.match_file.clone(),
                                                error: Some(RelatedError::new(err)),
                                            },
                                        );
                                        drop(rule_context);
                                        context.cursor_mut().move_to_end();
                                    }
                                }
                            }
                            Ok(result) => {
                                self.errors.push(result.unwrap_err());
                                drop(rule_context);
                                context.cursor_mut().move_to_end();
                            }
                            Err(err) => {
                                self.errors
                                    .push(CheckFailedError::MatchNoneForInvalidPattern {
                                        span: preceded_by.span(),
                                        match_file: self.match_file.clone(),
                                        error: Some(RelatedError::new(err)),
                                    });
                                drop(rule_context);
                                context.cursor_mut().move_to_end();
                            }
                        }
                    }
                    CheckOp::RepeatRule(_rule, _count) => todo!(),
                    CheckOp::RepeatRulePrecededBy(_rule, _count, _preceded_by) => todo!(),
                }

                if context.cursor().is_empty() {
                    block_id += 1;
                    continue 'next_block;
                }
            }

            // If we reach here, we have executed all code in the program
            break;
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(TestFailed {
                test_from: TestInputType(self.match_file.name()),
                span: input_file.span(),
                input: input_file.clone(),
                errors: core::mem::take(&mut self.errors),
            })
        }
    }
}

#[derive(Debug)]
struct BlockInfo {
    /// The span of the CHECK-LABEL span which started this block, if applicable
    #[allow(unused)]
    label_span: Option<SourceSpan>,
    /// The instruction index in the check program code for the first non-CHECK-LABEL
    /// instruction in the block, if any.
    code_start: Option<usize>,
    /// The starting byte index of the block, if known.
    ///
    /// The index begins at the start of the next line following the CHECK-LABEL line
    ///
    /// If None, the CHECK-LABEL pattern was never found; `end` must also be None
    /// in that case
    start: Option<usize>,
    /// The ending byte index of the block, if known.
    ///
    /// The index ends at the last byte of the line preceding a subsequent CHECK-LABEL.
    ///
    /// If subsequent blocks with start indices are recorded, this must be Some with
    /// an end index relative to the next start index in ascending order.
    end: Option<usize>,
}
