mod exclusion;
mod program;
mod variables;

use std::path::Path;

pub use self::exclusion::ExclusionGroup;
pub use self::program::{CheckGroup, CheckProgram, CheckSection, CheckTree};

use crate::common::*;

/// Whether checking can continue, distinguishing an ordinary miss from a failed exclusion.
#[derive(Debug)]
pub enum CheckOutcome<'input> {
    Matched(Vec<Matches<'input>>),
    Unmatched(Vec<Matches<'input>>),
    ExclusionFailed(Vec<Matches<'input>>),
}

use CheckOutcome::{ExclusionFailed, Matched, Unmatched};

pub struct Checker<'a> {
    config: &'a Config,
    program: CheckProgram<'a>,
    match_file: Arc<SourceFile>,
}
impl<'a> Checker<'a> {
    pub fn new(config: &'a Config, program: CheckProgram<'a>, match_file: Arc<SourceFile>) -> Self {
        Self {
            config,
            program,
            match_file,
        }
    }

    /// Check `source` against the rules in this [Checker]
    pub fn check(&mut self, source: impl AsRef<Path>) -> Result<TestResult, SourceManagerError> {
        let source = self.config.source_manager.load_file(source.as_ref())?;
        Ok(self.check_input(source))
    }

    /// Check `input` against the rules in this [Checker]
    pub fn check_str(&mut self, name: FileName, input: impl AsRef<str>) -> TestResult {
        let source = self.config.source_manager.load(
            SourceLanguage::Unknown,
            name,
            input.as_ref().to_string(),
        );
        self.check_input(source)
    }

    /// Check `source` against the rules in this [Checker]
    pub fn check_input(&mut self, source: Arc<SourceFile>) -> TestResult {
        let buffer = source.as_bytes();
        let mut context =
            MatchContext::new(self.config, self.match_file.clone(), source.clone(), buffer);

        if !self.config.options.allow_empty && buffer.is_empty() {
            return TestResult::from_error(TestFailed::new(
                vec![CheckFailedError::EmptyInput],
                &context,
            ));
        }

        let (blocks, errors) = discover_blocks(&self.program, &mut context);
        let mut result = check_blocks(blocks, &self.program, &mut context);
        for error in errors {
            result.failed(error);
        }
        result.attach_input_dump(&context);
        result
    }
}

/// Analyze the input to identify regions of input corresponding to
/// block sections in `program`, i.e. regions delimited by CHECK-LABEL
/// directives.
///
/// This will evaluate the CHECK-LABEL patterns, and construct block
/// metadata for each region trailing a CHECK-LABEL directive.
pub fn discover_blocks<'input, 'context: 'input>(
    program: &CheckProgram<'context>,
    context: &mut MatchContext<'input, 'context>,
) -> (SmallVec<[BlockInfo; 2]>, Vec<CheckFailedError>) {
    // Traverse the code of the program and try to identify
    // the byte ranges corresponding to block starts of the
    // program. Once the block starts are recorded, we can
    // properly constrain the search bounds of the matchers
    let mut blocks = SmallVec::<[BlockInfo; 2]>::default();
    let mut errors = Vec::<CheckFailedError>::new();
    // We must push an implicit block for ops that come before the first CHECK-LABEL,
    // if such ops exist
    let eof = context.cursor().end_of_file();
    if !matches!(program.sections.first(), Some(CheckSection::Block { .. })) {
        blocks.push(BlockInfo {
            label_info: None,
            sections: Some(Range::new(
                0,
                program
                    .sections
                    .iter()
                    .take_while(|s| !matches!(s, CheckSection::Block { .. }))
                    .count(),
            )),
            start: Some(0),
            end: None,
            eof,
        });
    }

    for (i, section) in program.sections.iter().enumerate() {
        if let CheckSection::Block { label, .. } = section {
            // Find the starting index of this pattern
            //
            // If no match is found, record the error,
            // and skip ahead in the instruction stream
            let buffer = context.search_to_eof();
            match label.try_match(buffer, context) {
                Ok(result) => {
                    match result.info {
                        Some(info) => {
                            if context.config.remarks_enabled()
                                && let Ok(loc) =
                                    context.source_manager().file_line_col(info.pattern_span)
                            {
                                eprintln!(
                                    "{loc}: remark: CHECK-LABEL: expected string found in input"
                                );
                            }
                            // Only the label match itself is excluded from the surrounding blocks.
                            // Unmatched prefixes and suffixes on its line must remain searchable.
                            let match_start = info.span.start().to_usize();
                            let match_end = info.span.end().to_usize();
                            context.cursor_mut().set_start(match_end);
                            // Create the block info for this CHECK-LABEL to record the start index
                            let block_id = blocks.len();
                            blocks.push(BlockInfo {
                                label_info: Some(info.into_static()),
                                sections: Some(Range::new(i, i + 1)),
                                start: Some(match_end),
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
                                prev_block.end = Some(match_start);
                            }
                        }
                        None => {
                            // The current block could not be found, so record an error
                            blocks.push(BlockInfo {
                                label_info: None,
                                sections: Some(Range::new(i, i + 1)),
                                start: None,
                                end: None,
                                eof,
                            });
                            errors.push(CheckFailedError::match_none(
                                label.span(),
                                &buffer,
                                context,
                            ));
                        }
                    }
                }
                Err(err) => {
                    blocks.push(BlockInfo {
                        label_info: None,
                        sections: Some(Range::new(i, i + 1)),
                        start: None,
                        end: None,
                        eof,
                    });
                    errors.push(CheckFailedError::MatchNoneForInvalidPattern {
                        span: label.span(),
                        match_file: context.source_file(label.span().source_id()).unwrap(),
                        error: Some(RelatedError::new(err)),
                    });
                }
            }
        }
    }

    // Reset the context bounds
    context.cursor_mut().reset();

    (blocks, errors)
}

/// Evaluate all check operations in the given blocks
pub fn check_blocks<'input, 'a: 'input, I>(
    blocks: I,
    program: &'input CheckProgram<'a>,
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
        if block.sections.is_none() {
            continue;
        }

        // Update the match context cursor
        context.enter_block(block.range());
        let section_range = unsafe { block.sections.unwrap_unchecked() };
        if section_range.is_empty() {
            // An implicit block covering zero sections, i.e. there is nothing to check
            continue;
        }
        if section_range.len() > 1 {
            // No CHECK-LABEL
            check_group_sections(
                &program.sections[section_range.start..section_range.end],
                &mut test_result,
                context,
            );
        } else {
            match &program.sections[section_range.start] {
                CheckSection::Block { body, .. } => {
                    // CHECK-LABEL
                    check_block_section(body, &mut test_result, context);
                }
                section @ CheckSection::Group { .. } => {
                    // Single-group, no blocks
                    check_group_sections(core::slice::from_ref(section), &mut test_result, context);
                }
            }
        }
    }

    test_result
}

/// Evaluate a section of check operations with the given context,
/// gathering any errors encountered into `errors`.
pub fn check_block_section<'input, 'a: 'input>(
    body: &[CheckGroup<'a>],
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) {
    check_groups(body, test_result, context);
}

pub fn check_group_sections<'input, 'a: 'input>(
    body: &[CheckSection<'a>],
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) {
    check_groups(
        body.iter().map(|section| match section {
            CheckSection::Group { body } => body,
            _ => unreachable!(),
        }),
        test_result,
        context,
    );
}

fn check_groups<'group, 'input, 'a: 'input + 'group>(
    groups: impl IntoIterator<Item = &'group CheckGroup<'a>>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) {
    for group in groups {
        match check_group(group, test_result, context) {
            Ok(ExclusionFailed(matched)) => {
                matched
                    .into_iter()
                    .for_each(|matches| test_result.append(matches));
                break;
            }
            Ok(Matched(matched) | Unmatched(matched)) => {
                matched
                    .into_iter()
                    .for_each(|matches| test_result.append(matches));
            }
            Err(error) => test_result.failed(error),
        }
    }
}

#[expect(clippy::result_large_err)]
pub fn check_group<'section, 'input, 'a: 'input>(
    group: &'section CheckGroup<'a>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) -> Result<CheckOutcome<'input>, CheckFailedError> {
    match group {
        CheckGroup::Never(pattern) => {
            let matches = pattern.apply(context.search_block(), context);
            if matches.has_errors() {
                Ok(ExclusionFailed(vec![matches]))
            } else {
                if context.config.remarks_enabled()
                    && let Ok(loc) = context.source_manager().file_line_col(pattern.span())
                {
                    eprintln!(
                        "{loc}: remark: CHECK-NOT: none of the expected strings were found in the input"
                    );
                }
                Ok(Matched(vec![matches]))
            }
        }
        CheckGroup::Ordered(rules) => {
            let mut matched = vec![];
            let mut rules = rules.iter();
            while let Some(rule) = rules.next() {
                match rule.apply(context) {
                    Ok(matches) => {
                        if matches.is_ok() {
                            if context.config.remarks_enabled()
                                && let Ok(loc) = context.source_manager().file_line_col(rule.span())
                            {
                                eprintln!(
                                    "{loc}: remark: {}: expected string found in input",
                                    DynRule::kind(rule)
                                );
                            }
                            matched.push(matches);
                        } else {
                            let skipped_spans =
                                rules.map(|rule| rule.span()).collect::<SmallVec<[_; 4]>>();
                            let skipped_start = skipped_spans
                                .iter()
                                .map(|rule| rule.span())
                                .min_by_key(|span| span.start().to_u32());
                            let skipped_end = skipped_spans
                                .into_iter()
                                .map(|rule| rule.span())
                                .max_by_key(|span| span.end().to_u32());
                            let skipped = skipped_start.zip(skipped_end).map(|(start, end)| {
                                SourceSpan::new(
                                    start.source_id(),
                                    Range::new(start.start(), end.end()),
                                )
                            });
                            let mut cause = matches
                                .into_results()
                                .into_iter()
                                .flat_map(|matches| {
                                    if matches.is_ok() {
                                        None
                                    } else {
                                        Some(matches.unwrap_err())
                                    }
                                })
                                .collect::<Vec<_>>();
                            // Simplify the diagnostic in the case where there is a single failure
                            // and no dependent checks
                            if cause.len() == 1 && skipped.is_none() {
                                test_result.failed(cause.pop().unwrap());
                            } else {
                                test_result.failed(CheckFailedError::MatchGroupFailed {
                                    span: rule.span(),
                                    match_file: context
                                        .source_file(rule.span().source_id())
                                        .unwrap(),
                                    cause,
                                    skipped,
                                });
                            }
                            return Ok(Unmatched(matched));
                        }
                    }
                    Err(err) => {
                        test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: rule.span(),
                            match_file: context.source_file(rule.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(err)),
                        });
                    }
                }
            }
            if matched.is_empty() || matched.iter().all(|m| m.range().is_none()) {
                Ok(Unmatched(matched))
            } else {
                Ok(Matched(matched))
            }
        }
        CheckGroup::Repeated { rule, count } => {
            let mut matched = vec![];
            let count = *count;
            for n in 0..count {
                match rule.apply(context) {
                    Ok(matches) => {
                        let match_results = matches.into_results();
                        let mut related = vec![];
                        let mut matches = Matches::default();
                        for match_result in match_results {
                            if match_result.is_ok() {
                                matches.push(match_result);
                            } else {
                                related.extend(
                                    match_result.unwrap_err().related_labels_for(rule.span()),
                                );
                            }
                        }

                        if related.is_empty() {
                            if context.config.remarks_enabled()
                                && let Ok(loc) = context.source_manager().file_line_col(rule.span())
                            {
                                eprintln!(
                                    "{loc}: remark: {}: expected string found in input ({} of {count} times)",
                                    DynRule::kind(rule),
                                    n + 1,
                                );
                            }
                            matched.push(matches);
                        } else {
                            test_result.failed(CheckFailedError::MatchRepeatedError {
                                span: rule.span(),
                                match_file: context.source_file(rule.span().source_id()).unwrap(),
                                n,
                                count,
                                related,
                            });
                            return Ok(Unmatched(matched));
                        }
                    }
                    Err(err) => {
                        test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: rule.span(),
                            match_file: context.source_file(rule.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(err)),
                        });
                        return Ok(Unmatched(matched));
                    }
                }
            }
            Ok(Matched(matched))
        }
        CheckGroup::Unordered(check_dag) => match check_dag.apply(context) {
            Ok(matches) => {
                if matches.range().is_none() {
                    Ok(Unmatched(vec![matches]))
                } else {
                    Ok(Matched(vec![matches]))
                }
            }
            Err(err) => {
                test_result.failed(CheckFailedError::MatchNoneErrorNote {
                    span: check_dag.span(),
                    match_file: context.source_file(check_dag.span().source_id()).unwrap(),
                    error: Some(RelatedError::new(err)),
                });
                Ok(Unmatched(vec![]))
            }
        },
        CheckGroup::Bounded {
            left: check_dag,
            right,
        } => {
            let initial_result = check_dag.apply(context);
            match check_group(right, test_result, context) {
                Ok(Matched(mut right_matches)) => {
                    let right_range = right_matches[0]
                        .range()
                        .expect("expected at least one match");
                    match initial_result {
                        Ok(mut left_matches) => {
                            if let Some(left_range) = left_matches.range()
                                && (left_range.start >= right_range.start
                                    || left_range.end >= right_range.start)
                            {
                                // At least one matching CHECK-DAG overlaps following CHECK,
                                // so visit each match result and rewrite overlapping matches
                                // to better guide users
                                let right_pattern_span = right.first_pattern_span();
                                for mr in left_matches.iter_mut() {
                                    match mr {
                                        MatchResult {
                                            info: Some(info),
                                            ty,
                                        } if ty.is_ok() => {
                                            let left_range = info.span;
                                            if left_range.start().to_usize() >= right_range.start
                                                || left_range.end().to_usize() >= right_range.start
                                            {
                                                let span = info.span;
                                                let pattern_span = info.pattern_span;
                                                *mr = MatchResult::failed(
                                                    CheckFailedError::MatchFoundButDiscarded {
                                                        span,
                                                        input_file: context.input_file(),
                                                        labels: vec![
                                                            RelatedLabel::error(
                                                                Label::new(
                                                                    pattern_span,
                                                                    "matched by this pattern",
                                                                ),
                                                                context
                                                                    .source_file(
                                                                        pattern_span.source_id(),
                                                                    )
                                                                    .unwrap(),
                                                            ),
                                                            RelatedLabel::warn(
                                                                Label::new(
                                                                    right_pattern_span,
                                                                    "because it cannot be reordered past this pattern",
                                                                ),
                                                                context
                                                                    .source_file(
                                                                        right_pattern_span
                                                                            .source_id(),
                                                                    )
                                                                    .unwrap(),
                                                            ),
                                                            RelatedLabel::note(
                                                                Label::point(
                                                                    context.input_file.id(),
                                                                    right_range.start as u32,
                                                                    "which begins here",
                                                                ),
                                                                context.input_file(),
                                                            ),
                                                        ],
                                                        note: None,
                                                    },
                                                );
                                            }
                                        }
                                        MatchResult {
                                            info: Some(info),
                                            ty: MatchType::Failed(err),
                                        } => {
                                            let span = info.span;
                                            let pattern_span = info.pattern_span;
                                            match err {
                                                CheckFailedError::MatchError { .. }
                                                | CheckFailedError::MatchFoundErrorNote {
                                                    ..
                                                }
                                                | CheckFailedError::MatchFoundConstraintFailed {
                                                    ..
                                                }
                                                | CheckFailedError::MatchFoundButDiscarded {
                                                    ..
                                                } if (span.start().to_usize()
                                                    >= right_range.start
                                                    || span.end().to_usize()
                                                        >= right_range.start) =>
                                                {
                                                    *mr = MatchResult::failed(
                                                        CheckFailedError::MatchFoundButDiscarded {
                                                            span,
                                                            input_file: context.input_file(),
                                                            labels: vec![
                                                                RelatedLabel::error(
                                                                    Label::new(
                                                                        pattern_span,
                                                                        "matched by this pattern",
                                                                    ),
                                                                    context
                                                                        .source_file(
                                                                            pattern_span
                                                                                .source_id(),
                                                                        )
                                                                        .unwrap(),
                                                                ),
                                                                RelatedLabel::warn(
                                                                    Label::new(
                                                                        right_pattern_span,
                                                                        "because it cannot be reordered past this pattern",
                                                                    ),
                                                                    context
                                                                        .source_file(
                                                                            right_pattern_span
                                                                                .source_id(),
                                                                        )
                                                                        .unwrap(),
                                                                ),
                                                                RelatedLabel::note(
                                                                    Label::point(
                                                                        context.input_file.id(),
                                                                        right_range.start as u32,
                                                                        "which begins here",
                                                                    ),
                                                                    context.input_file(),
                                                                ),
                                                            ],
                                                            note: None,
                                                        },
                                                    );
                                                }
                                                _ => (),
                                            }
                                        }
                                        _ => continue,
                                    }
                                }
                            }
                            let mut matched = Vec::with_capacity(1 + right_matches.len());
                            matched.push(left_matches);
                            matched.append(&mut right_matches);
                            Ok(Matched(matched))
                        }
                        Err(err) => {
                            test_result.failed(CheckFailedError::MatchNoneErrorNote {
                                span: check_dag.span(),
                                match_file: context
                                    .source_file(check_dag.span().source_id())
                                    .unwrap(),
                                error: Some(RelatedError::new(err)),
                            });
                            Ok(Matched(right_matches))
                        }
                    }
                }
                Ok(Unmatched(mut right_matches)) => match initial_result {
                    Ok(left_matches) => {
                        let mut matched = Vec::with_capacity(1 + right_matches.len());
                        let is_ok = left_matches.range().is_none();
                        matched.push(left_matches);
                        matched.append(&mut right_matches);
                        if is_ok {
                            Ok(Unmatched(matched))
                        } else {
                            Ok(Matched(matched))
                        }
                    }
                    Err(err) => {
                        test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: check_dag.span(),
                            match_file: context.source_file(check_dag.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(err)),
                        });
                        Ok(Unmatched(right_matches))
                    }
                },
                Ok(ExclusionFailed(mut right_matches)) => {
                    match initial_result {
                        Ok(left_matches) => right_matches.insert(0, left_matches),
                        Err(error) => test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: check_dag.span(),
                            match_file: context.source_file(check_dag.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(error)),
                        }),
                    }
                    Ok(ExclusionFailed(right_matches))
                }
                Err(right_err) => {
                    let result = match initial_result {
                        Ok(left_matches) => {
                            if left_matches.range().is_none() {
                                Unmatched(vec![left_matches])
                            } else {
                                Matched(vec![left_matches])
                            }
                        }
                        Err(left_err) => {
                            test_result.failed(CheckFailedError::MatchNoneErrorNote {
                                span: check_dag.span(),
                                match_file: context
                                    .source_file(check_dag.span().source_id())
                                    .unwrap(),
                                error: Some(RelatedError::new(left_err)),
                            });
                            Unmatched(vec![])
                        }
                    };
                    test_result.failed(CheckFailedError::MatchNoneErrorNote {
                        span: right.span(),
                        match_file: context.source_file(right.span().source_id()).unwrap(),
                        error: Some(RelatedError::new(Report::new(right_err))),
                    });
                    Ok(result)
                }
            }
        }
        CheckGroup::Tree(tree) => check_tree(tree, test_result, context),
    }
}

#[expect(clippy::result_large_err)]
fn check_tree<'input, 'a: 'input>(
    tree: &CheckTree<'a>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) -> Result<CheckOutcome<'input>, CheckFailedError> {
    match tree {
        CheckTree::MatchAll(group) => check_group(group, test_result, context),
        CheckTree::MatchAround { root, left, right } => {
            let (left_matched, mut matched) = match check_tree(left, test_result, context)? {
                Matched(matched) => (true, matched),
                Unmatched(matched) => (false, matched),
                ExclusionFailed(matched) => return Ok(ExclusionFailed(matched)),
            };
            let left_end = if left_matched {
                matched
                    .iter()
                    .filter_map(|matches| matches.range().map(|r| r.end))
                    .max()
                    .unwrap()
            } else {
                context.cursor().start()
            };
            match check_tree(right, test_result, context)? {
                Matched(mut right_matched) => {
                    let right_start = right_matched
                        .iter()
                        .filter_map(|matches| matches.range().map(|r| r.start))
                        .min()
                        .unwrap();
                    let bound = left_matched.then(|| {
                        let pattern = match right.first() {
                            Left(group) => group.first_pattern_span(),
                            Right(patterns) => patterns.first_pattern_span(),
                        };
                        (pattern, right_start)
                    });
                    let exclusions =
                        root.apply(context.search_range(left_end..right_start), context);
                    let failed = exclusions.has_errors();
                    let mut exclusions = exclusions;
                    if let Some((pattern, offset)) = bound {
                        for result in exclusions.iter_mut() {
                            if let MatchType::Failed(CheckFailedError::MatchFoundButExcluded {
                                labels,
                                ..
                            }) = &mut result.ty
                            {
                                labels.extend([
                                    RelatedLabel::note(
                                        Label::new(pattern, "exclusion is bounded by this pattern"),
                                        context.source_file(pattern.source_id()).unwrap(),
                                    ),
                                    RelatedLabel::note(
                                        Label::point(
                                            context.input_file.id(),
                                            offset as u32,
                                            "which corresponds to this location in the input",
                                        ),
                                        context.input_file(),
                                    ),
                                ]);
                            }
                        }
                    }
                    matched.push(exclusions);
                    matched.append(&mut right_matched);
                    Ok(if failed {
                        ExclusionFailed(matched)
                    } else {
                        Matched(matched)
                    })
                }
                Unmatched(mut right_matched) => {
                    // A missing right boundary already explains why this region could not be checked.
                    (0..root.pattern_len()).for_each(|_| test_result.passed());
                    matched.append(&mut right_matched);
                    Ok(if left_matched {
                        Matched(matched)
                    } else {
                        Unmatched(matched)
                    })
                }
                ExclusionFailed(mut right_matched) => {
                    matched.append(&mut right_matched);
                    Ok(ExclusionFailed(matched))
                }
            }
        }
        CheckTree::MatchBefore { root, left } => {
            let left_end = context.cursor().start();
            let (left_matched, mut matched) = match check_tree(left, test_result, context)? {
                Matched(matched) => (true, matched),
                Unmatched(matched) => (false, matched),
                ExclusionFailed(matched) => return Ok(ExclusionFailed(matched)),
            };
            let exclusion = if left_matched {
                context.search_block()
            } else {
                context.search_range(left_end..context.cursor().end())
            };
            let exclusions = root.apply(exclusion, context);
            let failed = exclusions.has_errors();
            matched.push(exclusions);
            Ok(if failed {
                ExclusionFailed(matched)
            } else if left_matched {
                Matched(matched)
            } else {
                Unmatched(matched)
            })
        }
        CheckTree::MatchAfter { root, right } => {
            let left_end = context.cursor().start();
            match check_tree(right, test_result, context)? {
                Matched(mut matched) => {
                    let right_start = matched
                        .iter()
                        .filter_map(|matches| matches.range().map(|r| r.start))
                        .min()
                        .unwrap();
                    let exclusions =
                        root.apply(context.search_range(left_end..right_start), context);
                    let failed = exclusions.has_errors();
                    matched.push(exclusions);
                    Ok(if failed {
                        ExclusionFailed(matched)
                    } else {
                        Matched(matched)
                    })
                }
                Unmatched(matched) => {
                    (0..root.pattern_len()).for_each(|_| test_result.passed());
                    Ok(Unmatched(matched))
                }
                ExclusionFailed(matched) => Ok(ExclusionFailed(matched)),
            }
        }
    }
}

#[derive(Debug)]
pub struct BlockInfo {
    /// The span of the CHECK-LABEL span which started this block, if applicable
    #[allow(unused)]
    pub label_info: Option<MatchInfo<'static>>,
    /// The section index range in the check program which covers all of the sections
    /// included in this block
    pub sections: Option<Range<usize>>,
    /// The starting byte index of the block, if known.
    ///
    /// The index begins immediately after the CHECK-LABEL match.
    ///
    /// If None, the CHECK-LABEL pattern was never found; `end` must also be None
    /// in that case
    pub start: Option<usize>,
    /// The ending byte index of the block, if known.
    ///
    /// The index ends immediately before a subsequent CHECK-LABEL match.
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
}
