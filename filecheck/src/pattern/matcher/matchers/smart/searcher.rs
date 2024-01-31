use either::{
    Either,
    Either::{Left, Right},
};
use regex_automata::{
    util::{captures::Captures, primitives::NonMaxUsize},
    PatternID,
};

use crate::{
    common::*,
    errors::{InvalidNumericCastError, UndefinedVariableError},
    expr::{Number, ValueType},
    pattern::matcher::{regex, SubstringMatcher},
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
            match_start: start,
            match_end: start,
        }
    }

    pub fn reset(&mut self, start: usize) {
        self.captures.set_pattern(None);
        self.cursor = self.input;
        self.stack.clear();
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
        //dbg!(&prefix, self.cursor.bounds());
        let result = match self.match_part(prefix, &guard) {
            Ok(result) => result,
            Err(err) => return ControlFlow::Break(Err(err)),
        };
        /*
                dbg!(
                    &result,
                    self.match_start,
                    self.match_end,
                    self.cursor.as_str(self.match_start..self.cursor.end())
                );
        */
        if let Some(info) = result.info.as_ref() {
            //dbg!(info.span.range());
            self.part_matched(info.span.range());
            self.cursor.set_anchored(true);
            //dbg!(self.cursor.bounds());

            let result = match self.match_parts(rest, &mut guard) {
                Ok(result) => result,
                Err(err) => return ControlFlow::Break(Err(err)),
            };
            self.cursor.set_anchored(anchored);
            //dbg!(&result);
            if result.is_ok() {
                // Match was successful, we're done!
                guard.save();
                return ControlFlow::Break(Ok(()));
            }

            // Match failed, so let's start over again and look for
            // another match of the prefix pattern to try from
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
                MatchOp::Bind { span, name, ty } => {
                    let value = self.stack.pop().expect("expected value on operand stack");
                    let value = match ty {
                        None | Some(ValueType::String) => value.into_value(),
                        Some(ValueType::Number(format)) => value
                            .as_numeric(*span, *format, context)
                            .map(Expr::Num)
                            .map(Value::Num)
                            .map_err(|mut err| {
                                err.span = Some(self.span);
                                err.specific_span = Some(*span);
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
                        self.part_matched(..matched.span.end());
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
        self.captures.set_pattern(Some(PatternID::ZERO));
        // TODO: Record actual captures
        Ok(MatchResult::ok(MatchInfo::new(
            self.match_start..self.match_end,
            self.span,
        )))
    }

    fn match_part<'context>(
        &mut self,
        part: &MatchOp<'a>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        match part {
            MatchOp::Literal(ref pattern) => self.match_literal(pattern, context),
            MatchOp::Substring(ref matcher) => self.match_substring(matcher, context),
            MatchOp::Regex {
                source,
                pattern,
                capture: None,
                ..
            } => self.match_regex(source.span(), pattern, context),
            MatchOp::Regex {
                source,
                pattern,
                capture: Some(capture),
            } => self.match_regex_with_captures(source.span(), pattern, *capture, context),
            MatchOp::Numeric {
                span,
                format,
                capture,
            } => self.match_number(*span, *format, *capture, context),
            MatchOp::Substitution { ty, expr } => self.match_substitution(*ty, expr, context),
            _ => unreachable!("non-pattern parts are handled in search_captures"),
        }
    }

    fn match_literal<'context>(
        &mut self,
        pattern: &Span<Cow<'_, str>>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        let buffer = self.cursor.as_str(self.cursor.bounds());
        //dbg!(buffer, pattern);
        let note = if self.cursor.is_anchored() {
            if buffer.starts_with(pattern.as_ref()) {
                let start = self.cursor.start();
                let end = start + pattern.as_bytes().len();
                return Ok(MatchResult::ok(MatchInfo::new(start..end, pattern.span())));
            }
            Some(format!(
                "search is anchored: expected match to occur starting at offset {}",
                self.cursor.start()
            ))
        } else {
            let bytes = pattern.as_bytes();
            if let Some(offset) = memchr::memmem::find(self.cursor.as_slice(), bytes) {
                let start = self.cursor.start() + offset;
                let end = start + bytes.len();
                return Ok(MatchResult::ok(MatchInfo::new(start..end, pattern.span())));
            }
            None
        };

        // Pattern did not match
        Ok(MatchResult::failed(
            CheckFailedError::MatchNoneButExpected {
                span: pattern.span(),
                match_file: context.match_file(),
                note,
            },
        ))
    }

    fn match_substring<'context>(
        &mut self,
        matcher: &SubstringMatcher<'a>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        matcher.try_match(self.cursor, context)
    }

    fn match_regex<'context>(
        &mut self,
        span: SourceSpan,
        pattern: &Regex,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        if let Some(matched) = pattern.find(self.cursor) {
            Ok(MatchResult::ok(MatchInfo::new(matched.range(), span)))
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
        capture: CaptureGroup,
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
            let group_infos = pattern.group_info();
            let num_groups = group_infos.group_len(PatternID::ZERO);
            let mut captured = None;
            for group_id in 0..num_groups {
                let (start_slot, end_slot) = self
                    .captures
                    .group_info()
                    .slots(capture.pattern_id, group_id)
                    .expect("invalid group id");
                let slots = self.captures.slots_mut();
                match captures.get_group(group_id) {
                    None => {
                        slots[start_slot] = None;
                        slots[end_slot] = None;
                    }
                    Some(span) => {
                        if group_id == capture.group_id {
                            captured = Some(span);
                        }
                        slots[start_slot] = Some(NonMaxUsize::new(span.start).unwrap());
                        slots[end_slot] = Some(NonMaxUsize::new(span.end).unwrap());
                    }
                }
            }
            // Collect the contents of the specified capture group, and push
            // it on the operand stack
            let span = captured.expect("no match for capturing group that was expected");
            let content = self.cursor.as_str(span.range());
            let captured = CaptureInfo {
                span: span.range().into(),
                index: capture.group_id,
                pattern_span,
                value: Value::Str(Cow::Borrowed(content)),
            };
            self.stack.push(Operand(captured.clone()));
            Ok(MatchResult::ok(
                MatchInfo::new_with_pattern(
                    span.range(),
                    pattern_span,
                    capture.pattern_id.as_usize(),
                )
                .with_captures(vec![captured]),
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
        format: NumberFormat,
        capture: Option<CaptureGroup>,
        context: &ContextGuard<'_, 'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
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

        let pattern = Regex::new(&format.pattern()).expect("syntax error in numeric regex");

        let input = self.cursor.into();
        if let Some(capture) = capture {
            let mut captures = pattern.create_captures();
            pattern.search_captures(&input, &mut captures);
            if let Some(matched) = captures.get_match() {
                let range = Range::from(matched.range());
                let pattern_span = span;
                let span = SourceSpan::from(range);
                let (_, [digits]) = captures.extract(self.cursor.as_str(0..));

                let radix = if format.is_hex() { 16 } else { 10 };
                match i64::from_str_radix(digits, radix) {
                    Ok(value) => {
                        let (start_slot, end_slot) = self
                            .captures
                            .group_info()
                            .slots(capture.pattern_id, capture.group_id)
                            .expect("invalid capture group");
                        let slots = self.captures.slots_mut();
                        slots[start_slot] = Some(NonMaxUsize::new(range.start).unwrap());
                        slots[end_slot] = Some(NonMaxUsize::new(range.end).unwrap());
                        let captured = CaptureInfo {
                            span,
                            pattern_span,
                            index: capture.group_id,
                            value: Value::Num(Expr::Num(Number::new_with_format(
                                range.into(),
                                value,
                                format,
                            ))),
                        };
                        self.stack.push(Operand(captured.clone()));
                        Ok(MatchResult::ok(
                            MatchInfo::new(span, pattern_span).with_captures(vec![captured]),
                        ))
                    }
                    Err(error) => Ok(MatchResult::failed(CheckFailedError::MatchFoundErrorNote {
                        span,
                        input_file: context.input_file(),
                        pattern: Some(RelatedCheckError {
                            span,
                            match_file: context.match_file(),
                        }),
                        help: Some(format!("unable to parse matched value: {error}")),
                    })),
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
            // Record the overall match
            Ok(MatchResult::ok(MatchInfo::new(matched.range(), span)))
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
        let lhs = lhs.as_numeric(span, Default::default(), context)?;
        match evaluate_expr(span, expr, context)? {
            Left(rhs) => {
                if lhs.value != rhs.value {
                    self.captures.set_pattern(None);
                }

                Ok(MatchResult::ok(MatchInfo::new(lhs.span(), span)))
            }
            Right(_) => Ok(MatchResult::failed(
                CheckFailedError::MatchFoundConstraintFailed {
                    span: lhs.span(),
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
                Some(ValueType::Number(format)) => {
                    format!("{}", Number { format, ..num })
                }
                _ => format!("{num}"),
            }),
            Right(s) => s,
        };
        let pattern = Regex::new(&pattern)
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
    context: &ContextGuard<'_, 'input, '_>,
) -> DiagResult<Either<Number, Cow<'input, str>>> {
    match expr {
        Expr::Num(num) => Ok(Left(num.clone())),
        Expr::Var(ref var) => {
            let var_span = var.span();
            let value = match var {
                VariableName::User(name) | VariableName::Global(name) => {
                    let env = context.env();
                    let value = env.get(var).cloned().ok_or_else(|| {
                        let match_file = context.match_file();
                        UndefinedVariableError {
                            span: var_span,
                            match_file,
                            name: env.resolve(**name).to_string(),
                        }
                    })?;
                    value
                        .as_number()
                        .map(Left)
                        .unwrap_or_else(|| Right(value.unwrap_string()))
                }
                VariableName::Pseudo(ref name) => match context.env().resolve(**name) {
                    "LINE" => {
                        let line = context.pseudo_line_for_offset(name.start());
                        Left(Number {
                            span: var_span,
                            format: Default::default(),
                            value: line.try_into().unwrap(),
                        })
                    }
                    other => {
                        return Err(Report::new(UndefinedVariableError {
                            span: var_span,
                            match_file: context.match_file(),
                            name: other.to_string(),
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
            let l = evaluate_expr(op_span, lhs, context)?;
            let r = evaluate_expr(op_span, rhs, context)?;
            match (l, r) {
                (Left(l), Left(r)) => {
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
                    Ok(Left(Number {
                        span,
                        format: l.format,
                        value: result,
                    }))
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
