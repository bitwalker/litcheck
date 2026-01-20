use regex_automata::{
    PatternID,
    util::{captures::Captures, primitives::NonMaxUsize},
};

use crate::{
    ast::Capture,
    common::*,
    errors::{InvalidNumericCastError, UndefinedVariableError},
    expr::ValueType,
    pattern::{
        matcher::{SubstringMatcher, regex},
        search::Input as _,
    },
};

use super::{CaptureGroup, MatchOp, Operand};

pub struct SmartSearcher<'a, 'input> {
    span: SourceSpan,
    captures: &'a mut Captures,
    /// The original input we were given to search
    input: Input<'input>,
    /// The current search input
    cursor: Input<'input>,
    /// The operand stack used by the searcher
    stack: Vec<Operand<'input>>,
    match_pattern_id: PatternID,
    match_start: usize,
    match_end: usize,
}
impl<'a, 'input> SmartSearcher<'a, 'input> {
    pub fn new(span: SourceSpan, input: Input<'input>, captures: &'a mut Captures) -> Self {
        let start = input.start();
        Self {
            span,
            captures,
            input,
            cursor: input,
            stack: vec![],
            match_pattern_id: PatternID::ZERO,
            match_start: start,
            match_end: start,
        }
    }

    pub fn reset(&mut self, start: usize) {
        self.captures.set_pattern(None);
        self.cursor = self.input;
        self.stack.clear();
        self.match_pattern_id = PatternID::ZERO;
        self.match_start = start;
        self.match_end = start;
        self.cursor.set_start(start);
    }

    pub fn search_captures<'context, C>(
        mut self,
        ops: &[MatchOp<'a>],
        context: &mut C,
    ) -> DiagResult<()>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let (prefix, rest) = ops.split_first().expect("expected at least one match op");
        loop {
            let guard = context.protect();
            match self.search_captures_guarded(prefix, rest, guard) {
                ControlFlow::Break(result) => break result,
                ControlFlow::Continue(_) => continue,
            }
        }
    }

    fn search_captures_guarded<'context>(
        &mut self,
        prefix: &MatchOp<'a>,
        rest: &[MatchOp<'a>],
        mut guard: ContextGuard<'_, 'input, 'context>,
    ) -> std::ops::ControlFlow<DiagResult<()>> {
        let anchored = self.cursor.is_anchored();
        let result = match self.match_part(prefix, &guard) {
            Ok(result) => result,
            Err(err) => return ControlFlow::Break(Err(err)),
        };
        if let Some(info) = result.info.as_ref() {
            self.part_matched(info.span.into_slice_index());
            self.cursor.set_anchored(true);

            let result = match self.match_parts(rest, &mut guard) {
                Ok(result) => result,
                Err(err) => return ControlFlow::Break(Err(err)),
            };
            self.cursor.set_anchored(anchored);
            if result.is_ok() {
                // Match was successful, we're done!
                guard.save();
                return ControlFlow::Break(Ok(()));
            }

            // Match failed, so let's start over again and look for another match of the prefix
            // pattern to try from
            log::debug!(
                "match attempt starting at offset {} failed beginning at offset {} with reason: {}",
                self.match_start,
                self.cursor.start(),
                &result.ty
            );
            // Reset the searcher, but start the next search where the cursor left off
            self.reset(self.cursor.start());
            if anchored {
                self.captures.set_pattern(None);
                return ControlFlow::Break(Ok(()));
            }
        } else {
            // No possible match in remaining input
            self.captures.set_pattern(None);
            return ControlFlow::Break(Ok(()));
        }

        ControlFlow::Continue(())
    }

    fn match_parts<'context>(
        &mut self,
        parts: &[MatchOp<'a>],
        context: &mut ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        for part in parts.iter() {
            if self.cursor.is_empty() {
                // No match possible
                self.captures.set_pattern(None);
                return Ok(MatchResult::failed(
                    CheckFailedError::MatchNoneButExpected {
                        span: self.span,
                        match_file: context.match_file(),
                        note: None,
                    },
                ));
            }

            match part {
                MatchOp::Constraint {
                    span,
                    constraint,
                    expr,
                } => {
                    let result = self.evaluate_constraint(*span, *constraint, expr, context)?;
                    if !result.is_ok() {
                        return Ok(result);
                    }
                }
                MatchOp::Bind { name, ty } => {
                    let value = self.stack.pop().expect("expected value on operand stack");
                    let value = match ty {
                        None | Some(ValueType::String) => value.into_value(),
                        Some(ValueType::Number(format)) => value
                            .as_numeric(name.span(), *format, context)
                            .map(Expr::Num)
                            .map(Value::Num)
                            .map_err(|mut err| {
                                err.span = Some(self.span);
                                err.specific_span = Some(name.span());
                                err
                            })?,
                    };
                    context.env_mut().insert(*name, value);
                }
                MatchOp::Drop => {
                    self.stack.pop().expect("expected value on operand stack");
                }
                part => {
                    let result = self.match_part(part, context)?;
                    if let Some(matched) = result.info.as_ref() {
                        self.part_matched(..matched.span.end().to_usize());
                    } else {
                        return Ok(result);
                    }
                }
            }
        }

        // If we reached here, a match was successful
        set_capturing_group_span(
            self.captures,
            PatternID::ZERO,
            0,
            self.match_start,
            self.match_end,
        );
        self.match_pattern_id = PatternID::ZERO;
        self.captures.set_pattern(Some(PatternID::ZERO));
        Ok(MatchResult::ok(MatchInfo::new(
            SourceSpan::from_range_unchecked(
                self.cursor.source_id(),
                self.match_start..self.match_end,
            ),
            self.span,
        )))
    }

    fn match_part<'context>(
        &mut self,
        part: &MatchOp<'a>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        let result = match part {
            MatchOp::Literal(ref pattern) => self.match_literal(pattern, context),
            MatchOp::Regex {
                source,
                pattern,
                captures,
                ..
            } if captures.is_empty() => self.match_regex(source.span(), pattern, context),
            MatchOp::Regex {
                source,
                pattern,
                captures,
            } => {
                self.match_regex_with_captures(source.span(), pattern, captures.as_slice(), context)
            }
            MatchOp::Numeric {
                span,
                format,
                capture,
            } => self.match_number(*span, *format, *capture, context),
            MatchOp::Substitution { ty, expr } => {
                return self.match_substitution(*ty, expr, context)
            }
            _ => unreachable!("non-pattern parts are handled in search_captures"),
        };
        self.match_pattern_id = PatternID::new_unchecked(self.match_pattern_id.one_more());
        result
    }

    fn match_literal<'context>(
        &mut self,
        matcher: &SubstringMatcher<'a>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        match matcher.try_match(self.cursor, context) {
            Ok(MatchResult {
                info: Some(info),
                ty,
            }) => {
                set_capturing_group_span(
                    self.captures,
                    self.match_pattern_id,
                    0,
                    info.span.start().to_usize(),
                    info.span.end().to_usize(),
                );
                Ok(MatchResult::new(ty, Some(info)))
            }
            result => result,
        }
    }

    fn match_regex<'context>(
        &mut self,
        span: SourceSpan,
        pattern: &Regex,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        if let Some(matched) = pattern.find(self.cursor) {
            let range = matched.range();
            set_capturing_group_span(
                self.captures,
                self.match_pattern_id,
                0,
                range.start,
                range.end,
            );
            Ok(MatchResult::ok(MatchInfo::new(
                SourceSpan::from_range_unchecked(self.cursor.source_id(), range),
                span,
            )))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }

    fn match_regex_with_captures<'context>(
        &mut self,
        pattern_span: SourceSpan,
        pattern: &Regex,
        capture_groups: &[CaptureGroup],
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        let input = self.cursor.into();
        let mut captures = pattern.create_captures();
        pattern.search_captures(&input, &mut captures);
        if let Some(matched) = captures.get_match() {
            // Record the overall match
            let range = matched.range();
            self.part_matched(..range.end);
            // Record local captures of this pattern in the global captures
            let mut captured = Vec::with_capacity(capture_groups.len());
            for capture_group in capture_groups.iter() {
                if let Some(capture_span) = captures.get_group(capture_group.group_id) {
                    set_capturing_group_span(
                        self.captures,
                        capture_group.pattern_id,
                        capture_group.group_id,
                        capture_span.start,
                        capture_span.end,
                    );

                    let content = self.cursor.as_str(capture_span.range());
                    match capture_group.info {
                        Capture::Ignore(_) => continue,
                        capture => {
                            let captured_info = CaptureInfo {
                                span: SourceSpan::from_range_unchecked(
                                    input.source_id(),
                                    capture_span.range(),
                                ),
                                index: capture_group.group_id,
                                pattern_span,
                                value: Value::Str(Cow::Borrowed(content)),
                                capture,
                            };
                            self.stack.push(Operand(captured_info.clone()));
                            captured.push(captured_info);
                        }
                    }
                } else {
                    unset_capturing_group_span(
                        self.captures,
                        capture_group.pattern_id,
                        capture_group.group_id,
                    );
                }
            }
            Ok(MatchResult::ok(
                MatchInfo::new(
                    SourceSpan::from_range_unchecked(input.source_id(), range),
                    pattern_span,
                )
                .with_captures(captured),
            ))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: pattern_span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }

    fn match_number<'context>(
        &mut self,
        span: SourceSpan,
        format: Option<NumberFormat>,
        capture_group: Option<CaptureGroup>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        log::trace!(target: "smart:searcher", "attempting to match number (format = {})", format.map(|f| Cow::Owned(f.to_string())).unwrap_or(Cow::Borrowed("default")));
        let format = format.unwrap_or_default();
        if self.cursor.as_slice().is_empty() {
            return Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span,
                    match_file: context.match_file(),
                    note: Some(
                        "unable to match because end of the searchable input was reached"
                            .to_string(),
                    ),
                },
            ));
        }

        let group_name = capture_group.as_ref().and_then(|cg| cg.info.group_name());
        let source = if let Some(group_name) = group_name {
            format.pattern(Some(group_name.as_str()))
        } else {
            format.pattern_nocapture()
        };
        let pattern = Regex::new(&source).expect("syntax error in numeric regex");

        let input = self.cursor.into();
        let input_source_id = self.cursor.source_id();
        if let Some(capture_group) = capture_group {
            let mut captures = pattern.create_captures();
            pattern.search_captures(&input, &mut captures);
            if let Some(matched) = captures.get_match() {
                let range = Range::from(matched.range());
                let pattern_span = span;
                let span = SourceSpan::from_range_unchecked(input_source_id, matched.range());
                let raw = self.cursor.as_str(matched.range());
                match Number::parse_with_format(Span::new(span, raw), format) {
                    Ok(parsed) => {
                        set_capturing_group_span(
                            self.captures,
                            self.match_pattern_id,
                            1,
                            range.start,
                            range.end,
                        );
                        let captured = CaptureInfo {
                            span,
                            pattern_span,
                            index: capture_group.group_id,
                            value: Value::Num(Expr::Num(parsed)),
                            capture: capture_group.info,
                        };
                        self.stack.push(Operand(captured.clone()));
                        Ok(MatchResult::ok(
                            MatchInfo::new(span, pattern_span).with_captures(vec![captured]),
                        ))
                    }
                    Err(error) => Ok(MatchResult::failed(
                        CheckFailedError::MatchFoundConstraintFailed {
                            span,
                            input_file: context.input_file(),
                            pattern: Some(RelatedCheckError {
                                span,
                                match_file: context.match_file(),
                            }),
                            error: Some(RelatedError::new(Report::new(error))),
                            help: None,
                        },
                    )),
                }
            } else {
                Ok(MatchResult::failed(
                    CheckFailedError::MatchNoneButExpected {
                        span,
                        match_file: context.match_file(),
                        note: None,
                    },
                ))
            }
        } else if let Some(matched) = pattern.find(input) {
            let range = matched.range();
            set_capturing_group_span(
                self.captures,
                self.match_pattern_id,
                0,
                range.start,
                range.end,
            );
            // Record the overall match
            Ok(MatchResult::ok(MatchInfo::new(
                SourceSpan::from_range_unchecked(input_source_id, range),
                span,
            )))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }

    fn evaluate_constraint<'context>(
        &mut self,
        span: SourceSpan,
        constraint: Constraint,
        expr: &Expr,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        assert_eq!(constraint, Constraint::Eq);
        let lhs = self.stack.last().expect("expected operand on stack");
        match evaluate_expr(span, expr, None, context)? {
            Left(rhs) => {
                let lhs = lhs.as_numeric(span, rhs.format, context)?;
                if lhs.value != rhs.value {
                    self.captures.set_pattern(None);
                }

                Ok(MatchResult::ok(MatchInfo::new(lhs.span(), span)))
            }
            Right(_) => Ok(MatchResult::failed(
                CheckFailedError::MatchFoundConstraintFailed {
                    span: lhs.0.span(),
                    input_file: context.input_file(),
                    pattern: Some(RelatedCheckError {
                        span,
                        match_file: context.match_file(),
                    }),
                    error: Some(RelatedError::new(Report::new(InvalidNumericCastError {
                        span: Some(span),
                        kind: std::num::IntErrorKind::InvalidDigit,
                        specific_span: Some(expr.span()),
                        match_file: context.match_file(),
                    }))),
                    help: Some(
                        "the expression evaluated to a string, but a number was expected"
                            .to_string(),
                    ),
                },
            )),
        }
    }

    fn match_substitution<'context>(
        &mut self,
        ty: Option<ValueType>,
        expr: &Expr,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        let span = expr.span();
        let pattern = match evaluate_expr(span, expr, context)? {
            Left(num) => Cow::Owned(match ty {
                Some(ValueType::Number(Some(format))) => {
                    format!(
                        "{}",
                        &Number {
                            format: Some(format),
                            ..num
                        }
                    )
                }
                _ => format!("{num}"),
            }),
            Right(s) => s,
        };
        let pattern = Regex::builder()
            .syntax(
                regex_automata::util::syntax::Config::new()
                    .multi_line(true)
                    .case_insensitive(context.config().options.ignore_case),
            )
            .configure(
                regex_automata::meta::Config::new()
                    .match_kind(regex_automata::MatchKind::LeftmostFirst),
            )
            .build(&pattern)
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| span))?;
        self.match_regex(span, &pattern, context)
    }

    #[inline]
    fn part_matched<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let range =
            range::range_from_bounds_with_defaults(range, self.match_start, self.cursor.end());
        self.match_start = range.start;
        self.match_end = range.end;
        self.cursor.set_start(range.end);
    }
}

fn evaluate_expr<'input>(
    span: SourceSpan,
    expr: &Expr,
    ty: Option<ValueType>,
    context: &ContextGuard<'_, 'input, '_>,
) -> DiagResult<Either<Number, Cow<'input, str>>> {
    match expr {
        Expr::Num(num) => Ok(Left(num.clone())),
        Expr::Var(var) => {
            let var_span = var.span();
            let value = match var {
                VariableName::User(name) | VariableName::Global(name) => {
                    let env = context.env();
                    let value = env.get(var).cloned().ok_or_else(|| {
                        let match_file = context.match_file();
                        UndefinedVariableError {
                            span: var_span,
                            match_file,
                            name: name.into_inner(),
                        }
                    })?;
                    match value {
                        Value::Undef => {
                            return Err(Report::new(UndefinedVariableError {
                                span: var_span,
                                match_file: context.match_file(),
                                name: name.into_inner(),
                            }));
                        }
                        Value::Num(expr) => return evaluate_expr(span, &expr, ty, context),
                        Value::Str(value) => Right(value),
                    }
                }
                VariableName::Pseudo(name) => match name.into_inner() {
                    symbols::Line => {
                        let line = context.pseudo_line_for_offset(name.span().start().to_usize());
                        Left(Number {
                            span: var_span,
                            format: Default::default(),
                            value: line.try_into().unwrap(),
                        })
                    }
                    name => {
                        return Err(Report::new(UndefinedVariableError {
                            span: var_span,
                            match_file: context.match_file(),
                            name,
                        }));
                    }
                },
            };
            Ok(value)
        }
        Expr::Binary {
            span: op_span,
            op,
            lhs,
            rhs,
        } => {
            let op_span = *op_span;
            let l = evaluate_expr(op_span, lhs, ty, context)?;
            let r = evaluate_expr(op_span, rhs, ty, context)?;
            let explicit_format = match ty {
                Some(ValueType::Number(format)) => format,
                _ => None,
            };
            match (l, r) {
                (Left(l), Left(r)) => {
                    if let Some(format) = l.infer_expression_format(&r).or(explicit_format) {
                        let result = match op {
                            BinaryOp::Add => l.value + r.value,
                            BinaryOp::Sub => l.value - r.value,
                            BinaryOp::Mul => l.value * r.value,
                            BinaryOp::Div => l.value / r.value,
                            BinaryOp::Min => core::cmp::min(l.value, r.value),
                            BinaryOp::Max => core::cmp::max(l.value, r.value),
                            BinaryOp::Eq => {
                                if l.value == r.value {
                                    1
                                } else {
                                    0
                                }
                            }
                        };
                        let format = if l.format.is_none()
                            && r.format.is_none()
                            && explicit_format.is_none()
                        {
                            None
                        } else {
                            Some(format)
                        };
                        Ok(Left(Number {
                            span,
                            format,
                            value: result,
                        }))
                    } else {
                        Err(litcheck::diagnostics::diagnostic!(
                            labels = vec![
                                litcheck::diagnostics::LabeledSpan::new_with_span(
                                    Some(format!(
                                        "the format of this operand is {}",
                                        l.format.unwrap_or_default()
                                    )),
                                    lhs.span(),
                                ),
                                litcheck::diagnostics::LabeledSpan::new_with_span(
                                    Some(format!(
                                        "the format of this operand is {}",
                                        r.format.unwrap_or_default()
                                    )),
                                    rhs.span(),
                                ),
                            ],
                            "implicit format conflict, need an explicit format specifier",
                        )
                        .into())
                    }
                }
                (Left(_), Right(_)) => Err(Report::new(InvalidNumericCastError {
                    span: Some(op_span),
                    kind: std::num::IntErrorKind::InvalidDigit,
                    specific_span: Some(rhs.span()),
                    match_file: context.match_file(),
                })),
                (Right(_), _) => Err(Report::new(InvalidNumericCastError {
                    span: Some(op_span),
                    kind: std::num::IntErrorKind::InvalidDigit,
                    specific_span: Some(lhs.span()),
                    match_file: context.match_file(),
                })),
            }
        }
    }
}

fn set_capturing_group_span(
    captures: &mut Captures,
    pid: PatternID,
    gid: usize,
    start: usize,
    end: usize,
) {
    let match_start_slot = captures
        .group_info()
        .slot(pid, gid)
        .expect("the implicit match group should always be available");
    let match_end_slot = match_start_slot + 1;
    let slots = captures.slots_mut();
    slots[match_start_slot] = Some(NonMaxUsize::new(start).unwrap());
    slots[match_end_slot] = Some(NonMaxUsize::new(end).unwrap());
}

fn unset_capturing_group_span(captures: &mut Captures, pid: PatternID, gid: usize) {
    let match_start_slot = captures
        .group_info()
        .slot(pid, gid)
        .expect("the implicit match group should always be available");
    let match_end_slot = match_start_slot + 1;
    let slots = captures.slots_mut();
    slots[match_start_slot] = None;
    slots[match_end_slot] = None;
}
