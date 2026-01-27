mod program;

use std::path::Path;

pub use self::program::{CheckGroup, CheckProgram, CheckSection, CheckTree};

use crate::{common::*, pattern::matcher::MatchAny};

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

        match discover_blocks(&self.program, &mut context) {
            Ok(blocks) => check_blocks(blocks, &self.program, &mut context),
            Err(failed) => TestResult::from_error(failed),
        }
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
                            // We must compute the indices for the end of the previous block,
                            // and the start of the current block, by looking forwards/backwards
                            // for the nearest newlines in those directions.
                            let match_start = info.span.start().to_usize();
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
                                sections: Some(Range::new(i, i + 1)),
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
                                sections: Some(Range::new(i, i + 1)),
                                start: None,
                                end: None,
                                eof,
                            });
                            let span = label.span();
                            let msg = format!(
                                "Unable to find a match for this pattern in the input.\
                            Search started at byte {}, ending at {eof}",
                                context.cursor().start()
                            );
                            errors.push(CheckFailedError::MatchNoneButExpected {
                                span,
                                match_file: context.source_file(span.source_id()).unwrap(),
                                note: Some(msg),
                            });
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

    if errors.is_empty() {
        Ok(blocks)
    } else {
        Err(TestFailed::new(errors, context))
    }
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
    for group in body.iter() {
        match check_group(group, test_result, context) {
            Ok(Ok(matched) | Err(matched)) => {
                matched
                    .into_iter()
                    .for_each(|matches| test_result.append(matches));
            }
            Err(err) => {
                test_result.failed(err);
            }
        }
    }
}

pub fn check_group_sections<'input, 'a: 'input>(
    body: &[CheckSection<'a>],
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) {
    for section in body.iter() {
        let CheckSection::Group { body: group } = section else {
            unreachable!()
        };
        match check_group(group, test_result, context) {
            Ok(Ok(matched) | Err(matched)) => {
                matched
                    .into_iter()
                    .for_each(|matches| test_result.append(matches));
            }
            Err(err) => {
                test_result.failed(err);
            }
        }
    }
}

pub fn check_group<'section, 'input, 'a: 'input>(
    group: &'section CheckGroup<'a>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) -> Result<Result<Vec<Matches<'input>>, Vec<Matches<'input>>>, CheckFailedError> {
    match group {
        CheckGroup::Never(pattern) => {
            // The given pattern should not match any of
            // the remaining input in this block
            let input = context.search_block();
            match check_not(pattern, input, context) {
                Ok(Ok(num_patterns)) => {
                    if context.config.remarks_enabled()
                        && let Ok(loc) = context.source_manager().file_line_col(pattern.span())
                    {
                        eprintln!(
                            "{loc}: remark: CHECK-NOT: none of the expected strings were found in the input"
                        );
                    }
                    (0..num_patterns).for_each(|_| test_result.passed());
                    Ok(Ok(vec![]))
                }
                Ok(Err(info)) => Ok(Err(vec![Matches::from_iter([MatchResult::failed(
                    CheckFailedError::MatchFoundButExcluded {
                        span: info.span,
                        input_file: context.input_file(),
                        labels: vec![RelatedLabel::error(
                            Label::new(info.pattern_span, "by this pattern"),
                            context.source_file(info.pattern_span.source_id()).unwrap(),
                        )],
                    },
                )])])),
                Err(err) => {
                    // Something went wrong with this pattern
                    Err(CheckFailedError::MatchNoneErrorNote {
                        span: pattern.span(),
                        match_file: context.source_file(pattern.span().source_id()).unwrap(),
                        error: Some(RelatedError::new(err)),
                    })
                }
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
                            return Ok(Err(matched));
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
                Ok(Err(matched))
            } else {
                Ok(Ok(matched))
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
                            return Ok(Err(matched));
                        }
                    }
                    Err(err) => {
                        test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: rule.span(),
                            match_file: context.source_file(rule.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(err)),
                        });
                        return Ok(Err(matched));
                    }
                }
            }
            Ok(Ok(matched))
        }
        CheckGroup::Unordered(check_dag) => match check_dag.apply(context) {
            Ok(matches) => {
                if matches.range().is_none() {
                    Ok(Err(vec![matches]))
                } else {
                    Ok(Ok(vec![matches]))
                }
            }
            Err(err) => {
                test_result.failed(CheckFailedError::MatchNoneErrorNote {
                    span: check_dag.span(),
                    match_file: context.source_file(check_dag.span().source_id()).unwrap(),
                    error: Some(RelatedError::new(err)),
                });
                Ok(Err(vec![]))
            }
        },
        CheckGroup::Bounded {
            left: check_dag,
            right,
        } => {
            let initial_result = check_dag.apply(context);
            match check_group(right, test_result, context) {
                Ok(Ok(mut right_matches)) => {
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
                                                } => {
                                                    if span.start().to_usize() >= right_range.start
                                                        || span.end().to_usize()
                                                            >= right_range.start
                                                    {
                                                        *mr = MatchResult::failed(CheckFailedError::MatchFoundButDiscarded {
                                                                span,
                                                                input_file: context.input_file(),
                                                                labels: vec![
                                                                    RelatedLabel::error(
                                                                        Label::new(pattern_span, "matched by this pattern"),
                                                                        context.source_file(pattern_span.source_id()).unwrap()
                                                                    ),
                                                                    RelatedLabel::warn(
                                                                        Label::new(right_pattern_span, "because it cannot be reordered past this pattern"),
                                                                        context.source_file(right_pattern_span.source_id()).unwrap()
                                                                    ),
                                                                    RelatedLabel::note(
                                                                        Label::point(context.input_file.id(), right_range.start as u32, "which begins here"),
                                                                        context.input_file()
                                                                    ),
                                                                ],
                                                                note: None,
                                                            });
                                                    }
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
                            Ok(Ok(matched))
                        }
                        Err(err) => {
                            test_result.failed(CheckFailedError::MatchNoneErrorNote {
                                span: check_dag.span(),
                                match_file: context
                                    .source_file(check_dag.span().source_id())
                                    .unwrap(),
                                error: Some(RelatedError::new(err)),
                            });
                            Ok(Ok(right_matches))
                        }
                    }
                }
                Ok(Err(mut right_matches)) => match initial_result {
                    Ok(left_matches) => {
                        let mut matched = Vec::with_capacity(1 + right_matches.len());
                        let is_ok = left_matches.range().is_none();
                        matched.push(left_matches);
                        matched.append(&mut right_matches);
                        if is_ok {
                            Ok(Err(matched))
                        } else {
                            Ok(Ok(matched))
                        }
                    }
                    Err(err) => {
                        test_result.failed(CheckFailedError::MatchNoneErrorNote {
                            span: check_dag.span(),
                            match_file: context.source_file(check_dag.span().source_id()).unwrap(),
                            error: Some(RelatedError::new(err)),
                        });
                        Ok(Err(right_matches))
                    }
                },
                Err(right_err) => {
                    let result = match initial_result {
                        Ok(left_matches) => {
                            if left_matches.range().is_none() {
                                Err(vec![left_matches])
                            } else {
                                Ok(vec![left_matches])
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
                            Err(vec![])
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

fn check_tree<'input, 'a: 'input>(
    tree: &CheckTree<'a>,
    test_result: &mut TestResult,
    context: &mut MatchContext<'input, 'a>,
) -> Result<Result<Vec<Matches<'input>>, Vec<Matches<'input>>>, CheckFailedError> {
    match tree {
        CheckTree::MatchAll(group) => check_group(group, test_result, context),
        CheckTree::MatchAround { root, left, right } => {
            let mut matched = vec![];
            match check_tree(left, test_result, context).expect("unexpected check-not error") {
                // There may be some errors, but at least one pattern matched
                Ok(mut left_matched) => {
                    assert!(
                        !left_matched.is_empty(),
                        "expected at least one match result"
                    );
                    // Set aside the start of the exclusion region between `left` and `right`
                    let left_end = left_matched
                        .iter()
                        .filter_map(|matches| matches.range().map(|r| r.end))
                        .max()
                        .unwrap();
                    // Add the left matches to the pending test results
                    matched.append(&mut left_matched);
                    match check_tree(right, test_result, context)
                        .expect("unexpected check-not error")
                    {
                        Ok(mut right_matched) => {
                            assert!(
                                !right_matched.is_empty(),
                                "expected at least one match result"
                            );
                            // Find the end of the exclusion region
                            let right_start = right_matched
                                .iter()
                                .filter_map(|matches| matches.range().map(|r| r.start))
                                .min()
                                .unwrap();
                            // Ensure none of the CHECK-NOT patterns match in the exclusion region
                            let exclusion = context.search_range(left_end..right_start);
                            match check_not(root, exclusion, context) {
                                Ok(Ok(num_passed)) => {
                                    (0..num_passed).for_each(|_| test_result.passed());
                                }
                                Ok(Err(info)) => {
                                    let right_pattern_span = match right.first() {
                                        Left(group) => group.first_pattern_span(),
                                        Right(patterns) => patterns.first_pattern_span(),
                                    };
                                    matched.push(Matches::from_iter([MatchResult::failed(CheckFailedError::MatchFoundButExcluded {
                                        span: info.span,
                                        input_file: context.input_file(),
                                        labels: vec![
                                            RelatedLabel::error(
                                                Label::new(info.pattern_span, "excluded by this pattern"),
                                                context.source_file(info.pattern_span.source_id()).unwrap(),
                                            ),
                                            RelatedLabel::note(
                                                Label::new(right_pattern_span, "exclusion is bounded by this pattern"),
                                                context.source_file(right_pattern_span.source_id()).unwrap(),
                                            ),
                                            RelatedLabel::note(
                                                Label::point(context.input_file.id(), right_start as u32, "which corresponds to this location in the input"),
                                                context.input_file()
                                            ),
                                        ],
                                    })]));
                                }
                                Err(err) => {
                                    // Something went wrong with this pattern
                                    matched.push(Matches::from_iter([MatchResult::failed(
                                        CheckFailedError::MatchNoneErrorNote {
                                            span: root.span(),
                                            match_file: context
                                                .source_file(root.span().source_id())
                                                .unwrap(),
                                            error: Some(RelatedError::new(err)),
                                        },
                                    )]));
                                }
                            }
                            // Add the right matches to the test results
                            matched.append(&mut right_matched);
                        }
                        Err(mut right_matched) => {
                            // TODO: Check to see if the right-hand side would have matched BEFORE left, and present a better error

                            // Since none of the right-hand patterns matched, we will treat the CHECK-NOT patterns
                            // as having passed, since the error of interest is the failed match
                            (0..root.pattern_len()).for_each(|_| test_result.passed());
                            // Add the right matches to the test results
                            matched.append(&mut right_matched);
                        }
                    }
                    Ok(Ok(matched))
                }
                // None of the left-hand patterns matched
                Err(mut left_matched) => {
                    // Add the failed matches to the test results
                    matched.append(&mut left_matched);
                    // Proceed with the right-hand patterns
                    let left_end = context.cursor().start();
                    match check_tree(right, test_result, context)
                        .expect("unexpected check-not error")
                    {
                        Ok(mut right_matched) => {
                            assert!(
                                !right_matched.is_empty(),
                                "expected at least one match result"
                            );
                            // Find the end of the exclusion region
                            let right_start = right_matched
                                .iter()
                                .filter_map(|matches| matches.range().map(|r| r.start))
                                .min()
                                .unwrap();
                            // Ensure none of the CHECK-NOT patterns match in the exclusion region
                            let exclusion = context.search_range(left_end..right_start);
                            match check_not(root, exclusion, context) {
                                Ok(Ok(num_passed)) => {
                                    (0..num_passed).for_each(|_| test_result.passed());
                                }
                                Ok(Err(info)) => {
                                    matched.push(Matches::from_iter([MatchResult::failed(
                                        CheckFailedError::MatchFoundButExcluded {
                                            span: info.span,
                                            input_file: context.input_file(),
                                            labels: vec![RelatedLabel::error(
                                                Label::new(info.pattern_span, "by this pattern"),
                                                context
                                                    .source_file(info.pattern_span.source_id())
                                                    .unwrap(),
                                            )],
                                        },
                                    )]));
                                }
                                Err(err) => {
                                    // Something went wrong with this pattern
                                    matched.push(Matches::from_iter([MatchResult::failed(
                                        CheckFailedError::MatchNoneErrorNote {
                                            span: root.span(),
                                            match_file: context
                                                .source_file(root.span().source_id())
                                                .unwrap(),
                                            error: Some(RelatedError::new(err)),
                                        },
                                    )]));
                                }
                            }
                            // Add the right matches to the test results
                            matched.append(&mut right_matched);
                            Ok(Ok(matched))
                        }
                        Err(mut right_matched) => {
                            // Since none of the right-hand patterns matched, we will treat the CHECK-NOT patterns
                            // as having passed, since the error of interest is the failed match
                            (0..root.pattern_len()).for_each(|_| test_result.passed());
                            // Add the right matches to the test results
                            matched.append(&mut right_matched);
                            Ok(Err(matched))
                        }
                    }
                }
            }
        }
        CheckTree::MatchBefore { root, left } => {
            let left_end = context.cursor().start();
            match check_tree(left, test_result, context).expect("unexpected check-not error") {
                Ok(mut left_matched) => {
                    assert!(
                        !left_matched.is_empty(),
                        "expected at least one match result"
                    );

                    let exclusion = context.search_block();
                    match check_not(root, exclusion, context) {
                        Ok(Ok(num_passed)) => {
                            (0..num_passed).for_each(|_| test_result.passed());
                        }
                        Ok(Err(info)) => {
                            left_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchFoundButExcluded {
                                    span: info.span,
                                    input_file: context.input_file(),
                                    labels: vec![RelatedLabel::error(
                                        Label::new(info.pattern_span, "by this pattern"),
                                        context.source_file(info.pattern_span.source_id()).unwrap(),
                                    )],
                                },
                            )]));
                        }
                        Err(err) => {
                            // Something went wrong with this pattern
                            left_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchNoneErrorNote {
                                    span: root.span(),
                                    match_file: context
                                        .source_file(root.span().source_id())
                                        .unwrap(),
                                    error: Some(RelatedError::new(err)),
                                },
                            )]));
                        }
                    }
                    Ok(Ok(left_matched))
                }
                Err(mut left_matched) => {
                    // None of the left-hand patterns matched, but we can proceed with the CHECK-NOT anyway
                    let right_start = context.cursor().end();
                    let exclusion = context.search_range(left_end..right_start);
                    match check_not(root, exclusion, context) {
                        Ok(Ok(num_passed)) => {
                            (0..num_passed).for_each(|_| test_result.passed());
                        }
                        Ok(Err(info)) => {
                            left_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchFoundButExcluded {
                                    span: info.span,
                                    input_file: context.input_file(),
                                    labels: vec![RelatedLabel::error(
                                        Label::new(info.pattern_span, "by this pattern"),
                                        context.source_file(info.pattern_span.source_id()).unwrap(),
                                    )],
                                },
                            )]));
                        }
                        Err(err) => {
                            // Something went wrong with this pattern
                            left_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchNoneErrorNote {
                                    span: root.span(),
                                    match_file: context
                                        .source_file(root.span().source_id())
                                        .unwrap(),
                                    error: Some(RelatedError::new(err)),
                                },
                            )]));
                        }
                    }
                    Ok(Err(left_matched))
                }
            }
        }
        CheckTree::MatchAfter { root, right } => {
            let left_end = context.cursor().start();
            match check_tree(right, test_result, context).expect("unexpected check-not error") {
                Ok(mut right_matched) => {
                    assert!(
                        !right_matched.is_empty(),
                        "expected at least one match result"
                    );
                    let right_start = right_matched
                        .iter()
                        .filter_map(|matches| matches.range().map(|r| r.start))
                        .min()
                        .unwrap();

                    let exclusion = context.search_range(left_end..right_start);
                    match check_not(root, exclusion, context) {
                        Ok(Ok(num_passed)) => {
                            (0..num_passed).for_each(|_| test_result.passed());
                        }
                        Ok(Err(info)) => {
                            right_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchFoundButExcluded {
                                    span: info.span,
                                    input_file: context.input_file(),
                                    labels: vec![RelatedLabel::error(
                                        Label::new(info.pattern_span, "by this pattern"),
                                        context.source_file(info.pattern_span.source_id()).unwrap(),
                                    )],
                                },
                            )]));
                        }
                        Err(err) => {
                            // Something went wrong with this pattern
                            right_matched.push(Matches::from_iter([MatchResult::failed(
                                CheckFailedError::MatchNoneErrorNote {
                                    span: root.span(),
                                    match_file: context
                                        .source_file(root.span().source_id())
                                        .unwrap(),
                                    error: Some(RelatedError::new(err)),
                                },
                            )]));
                        }
                    }
                    Ok(Ok(right_matched))
                }
                Err(right_matched) => {
                    // Since none of the right-hand patterns matched, we will treat the CHECK-NOT patterns
                    // as having passed, since the error of interest is the failed match
                    (0..root.pattern_len()).for_each(|_| test_result.passed());
                    Ok(Err(right_matched))
                }
            }
        }
    }
}

fn check_not<'input, 'a: 'input>(
    patterns: &MatchAny<'a>,
    input: Input<'input>,
    context: &mut MatchContext<'input, 'a>,
) -> DiagResult<Result<usize, MatchInfo<'input>>> {
    match patterns.try_match_mut(input, context)? {
        MatchResult {
            info: Some(info),
            ty,
        } if ty.is_ok() => Ok(Err(info)),
        result => {
            assert!(!result.is_ok());
            Ok(Ok(patterns.pattern_len()))
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
}
