use std::{
    borrow::Cow,
    collections::BTreeMap,
    fmt,
    ops::{Bound, RangeBounds},
};

use either::{
    Either,
    Either::{Left, Right},
};
use litcheck::{
    diagnostics::{Diag, DiagResult, LabeledSpan, Report, SourceSpan, Span, Spanned},
    StringInterner, Symbol,
};
use regex_automata::{
    meta::Regex,
    util::{
        captures::{Captures, GroupInfo},
        primitives::NonMaxUsize,
    },
    PatternID,
};

use crate::{
    check::{
        CheckFailedError, Constraint, Input, InvalidNumericCastError, Operand, RelatedCheckError,
        RelatedError, UndefinedVariableError,
    },
    expr::{Expr, Number, NumberFormat, Value, ValueType, VariableName},
};

use super::*;

/// This is a combination, jack-of-all trades matcher, which is
/// executes a set of smaller matches to form a large one. Some
/// parts of the "pattern" are known statically, while others
/// rely on runtime variable bindings and expression evaluation,
/// hence "smart". This is the default matcher that is used any
/// time match blocks or substitutions are present in a pattern.
pub struct SmartMatcher<'a> {
    span: SourceSpan,
    /// The underlying "smart" components of this matcher.
    ///
    /// These parts are matched and evaluated left-to-right,
    /// each one is fully processed before moving to the next
    /// part. The matcher maintains its own workspace for
    /// locally-bound variables and the like, so that if the
    /// pattern fails, none of the state changes are applied
    /// globally
    parts: Vec<MatchOp<'a>>,
    /// Capture group metadata for this regex
    group_info: GroupInfo,
}
impl<'a> fmt::Debug for SmartMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SmartMatcher")
            .field("parts", &self.parts)
            .field(
                "group_info",
                &self
                    .group_info
                    .all_names()
                    .collect::<smallvec::SmallVec<[_; 4]>>(),
            )
            .finish()
    }
}
impl<'a> Spanned for SmartMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl<'a> SmartMatcher<'a> {
    pub fn build<'config>(
        span: SourceSpan,
        interner: &'config mut StringInterner,
    ) -> SmartMatcherBuilder<'a, 'config> {
        SmartMatcherBuilder::new(span, interner)
    }

    fn new(
        span: SourceSpan,
        parts: Vec<MatchOp<'a>>,
        groups: Vec<Vec<Option<Cow<'a, str>>>>,
    ) -> Self {
        let group_info =
            GroupInfo::new(groups).expect("expected capturing groups to meet all requirements");
        Self {
            span,
            parts,
            group_info,
        }
    }

    pub fn group_info(&self) -> &GroupInfo {
        &self.group_info
    }
}
impl<'a> MatcherMut for SmartMatcher<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut captures = Captures::matches(self.group_info().clone());
        let searcher = Searcher::new(self.span, input, &mut captures);
        searcher.search_captures(&self.parts, context)?;
        if let Some(matched) = captures.get_match() {
            // TODO: Add captures to result
            Ok(MatchResult::ok(MatchInfo::new(matched.range(), self.span)))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}

pub struct SmartMatcherBuilder<'a, 'config> {
    span: SourceSpan,
    interner: &'config mut StringInterner,
    parts: Vec<MatchOp<'a>>,
    group_name_ids: Vec<BTreeMap<Symbol, usize>>,
    pattern_groups: Vec<Vec<Option<Cow<'a, str>>>>,
    captures: Vec<ValueType>,
}
impl<'a, 'config> SmartMatcherBuilder<'a, 'config> {
    pub fn new(span: SourceSpan, interner: &'config mut StringInterner) -> Self {
        Self {
            span,
            interner,
            parts: vec![],
            group_name_ids: vec![],
            pattern_groups: vec![vec![None]],
            captures: vec![],
        }
    }

    pub fn build(self) -> SmartMatcher<'a> {
        SmartMatcher::new(self.span, self.parts, self.pattern_groups)
    }

    pub fn literal(&mut self, pattern: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.parts.push(MatchOp::Literal(pattern));
        Ok(self)
    }

    pub fn regex(&mut self, source: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        let pattern = Regex::new(source.as_ref()).map_err(|error| {
            super::regex::build_error_to_diagnostic(error, 1, |_| source.span())
        })?;
        self.parts.push(MatchOp::Regex {
            source,
            pattern,
            capture: None,
        });
        Ok(self)
    }

    pub fn numeric(&mut self, span: SourceSpan, format: NumberFormat) -> &mut Self {
        self.parts.push(MatchOp::Numeric {
            span,
            format,
            capture: None,
        });
        self
    }

    pub fn numeric_with_constraint(
        &mut self,
        span: SourceSpan,
        format: NumberFormat,
        constraint: Constraint,
        expr: Expr,
    ) -> &mut Self {
        self.captures.push(ValueType::Number(format));
        let group = self.register_unnamed_group(PatternID::ZERO);
        self.parts.push(MatchOp::Numeric {
            span,
            format,
            capture: Some(group),
        });
        self.parts.push(MatchOp::Constraint {
            span,
            constraint,
            expr,
        });
        self.parts.push(MatchOp::Drop);
        self
    }

    pub fn substitution(&mut self, expr: Expr) -> &mut Self {
        self.parts.push(MatchOp::Substitution { expr, ty: None });
        self
    }

    pub fn substitution_with_format(&mut self, expr: Expr, ty: ValueType) -> &mut Self {
        self.parts
            .push(MatchOp::Substitution { expr, ty: Some(ty) });
        self
    }

    pub fn capture(&mut self, name: Symbol, source: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.captures.push(ValueType::String);
        let group = self.register_named_group(PatternID::ZERO, name);
        let group_name = self.pattern_groups[0][group.group_id].as_ref().unwrap();
        let pattern = Regex::new(&format!("(?P<{group_name}>{source})")).map_err(|error| {
            super::regex::build_error_to_diagnostic(error, 1, |_| source.span())
        })?;

        let span = source.span();
        self.parts.push(MatchOp::Regex {
            source,
            pattern,
            capture: Some(group),
        });
        self.parts.push(MatchOp::Bind {
            span,
            name,
            ty: None,
        });
        Ok(self)
    }

    pub fn capture_numeric(
        &mut self,
        span: SourceSpan,
        name: Symbol,
        format: NumberFormat,
    ) -> &mut Self {
        self.captures.push(ValueType::Number(format));
        let group = self.register_named_group(PatternID::ZERO, name);
        self.parts.push(MatchOp::Numeric {
            span,
            format,
            capture: Some(group),
        });
        self.parts.push(MatchOp::Bind {
            span,
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    pub fn capture_numeric_with_constraint(
        &mut self,
        span: SourceSpan,
        name: Symbol,
        format: NumberFormat,
        constraint: Constraint,
        expr: Expr,
    ) -> &mut Self {
        self.captures.push(ValueType::Number(format));
        let group = self.register_named_group(PatternID::ZERO, name);
        self.parts.push(MatchOp::Numeric {
            span,
            format,
            capture: Some(group),
        });
        let expr_span = expr.span();
        self.parts.push(MatchOp::Constraint {
            span: expr_span,
            constraint,
            expr,
        });
        self.parts.push(MatchOp::Bind {
            span,
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    /// This permits us to store captures for nested patterns in the same [Captures]
    #[allow(unused)]
    fn register_pattern_group(
        &mut self,
        span: SourceSpan,
        group_info: &GroupInfo,
    ) -> DiagResult<PatternID> {
        use std::collections::btree_map::Entry;

        assert_eq!(
            group_info.pattern_len(),
            1,
            "unsupported multi-pattern pattern group registration"
        );
        assert_eq!(self.group_name_ids.len(), self.pattern_groups.len());
        let pid = PatternID::new_unchecked(self.pattern_groups.len());
        let mut group_name_ids = BTreeMap::default();
        let mut groups = vec![];
        for group_name in group_info.pattern_names(PatternID::ZERO) {
            match group_name {
                None => {
                    groups.push(None);
                }
                Some(group_name) => {
                    let symbol = self.interner.get_or_intern(group_name);
                    match group_name_ids.entry(symbol) {
                        Entry::Occupied(_) => {
                            let label = format!("the capturing group '{group_name}' conflicts with another in the same pattern");
                            return Err(Report::new(Diag::new("invalid regular expression pattern")
                                .with_label(LabeledSpan::at(span, label))
                                .with_help("try using a different group name, or removing the explicit name if not needed")));
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(0);
                        }
                    }
                }
            }
        }

        self.pattern_groups.push(groups);
        self.group_name_ids.push(group_name_ids);

        Ok(pid)
    }

    fn register_unnamed_group(&mut self, pid: PatternID) -> CaptureGroup {
        let pattern_group = &mut self.pattern_groups[pid.as_usize()];
        let group_id = pattern_group.len();
        pattern_group.push(None);
        CaptureGroup {
            pattern_id: pid,
            group_id,
        }
    }

    fn register_named_group(&mut self, pid: PatternID, var: Symbol) -> CaptureGroup {
        use std::collections::btree_map::Entry;

        let name = self.interner.resolve(var);
        let pattern_group_index = pid.as_usize();
        let name = match self.group_name_ids[pattern_group_index].entry(var) {
            Entry::Vacant(entry) => {
                entry.insert(0);
                Cow::Owned(name.to_string())
            }
            Entry::Occupied(mut entry) => {
                let next_id = entry.get_mut();
                let id = *next_id;
                *next_id += 1;
                Cow::Owned(format!("{name}[{id}]"))
            }
        };
        let pattern_group = &mut self.pattern_groups[pattern_group_index];
        let group_id = pattern_group.len();
        pattern_group.push(Some(name));
        CaptureGroup {
            pattern_id: pid,
            group_id,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CaptureGroup {
    pattern_id: PatternID,
    group_id: usize,
}

/// A single instruction for the [SmartMatcher] evaluation loop.
///
/// Each op which performs a match is anchored at the current position
/// in the input, except for the first one, which will search for the
/// first occurrance of that pattern anywhere in the input.
///
/// If an op fails, the entire match attempt fails.
enum MatchOp<'a> {
    /// Match an literal string (always anchored)
    Literal(Span<Cow<'a, str>>),
    /// Match a literal string anywhere in the input
    #[allow(dead_code)]
    Substring(SubstringMatcher<'a>),
    /// Match a regular expression, optionally capturing a string value.
    Regex {
        /// The original pattern as a string
        source: Span<Cow<'a, str>>,
        /// The compiled regular expression
        pattern: Regex,
        /// If set, this is the capture group data corresponding to
        /// the `captures` set in [SmartMatcher]. This will be used to
        /// collect the captured value and push it on the operand
        /// stack.
        capture: Option<CaptureGroup>,
    },
    /// Match a numeric value of the given format
    Numeric {
        /// The span of the pattern
        span: SourceSpan,
        /// The format of the number to match
        format: NumberFormat,
        /// If set, this is the capture group data corresponding to
        /// the `captures` set in [SmartMatcher]. This will be used to
        /// collect the captured value and push it on the operand
        /// stack.
        capture: Option<CaptureGroup>,
    },
    /// Match a regular expression that is constructed dynamically
    /// by substituting a value produced by evaluating the given
    /// expression, and then formatting it as specified.
    Substitution {
        /// The expression whose value once evaluated will be substituted for this part
        expr: Expr,
        /// The type to format the value of `expr` as, if specified
        ///
        /// Defaults to the format of the value returned by `expr`
        ty: Option<ValueType>,
    },
    /// Pop a value off the operand stack and bind it to `name`
    Bind {
        /// The span of the pattern from this bind is derived
        span: SourceSpan,
        /// The name to bind
        name: Symbol,
        /// If set, the value on the operand stack will be converted
        /// to a value of the given type, otherwise the match fails.
        ty: Option<ValueType>,
    },
    /// Drop the top value on the operand stack
    Drop,
    /// Evaluate a constraint at the current execution point against
    /// the value on top of the operand stack.
    Constraint {
        /// The span of the pattern which is being constrained
        span: SourceSpan,
        /// The constraint type
        constraint: Constraint,
        /// The constraint
        expr: Expr,
    },
}
impl<'a> fmt::Debug for MatchOp<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Literal(lit) => f.debug_tuple("Literal").field(lit).finish(),
            Self::Substring(matcher) => f.debug_tuple("Substring").field(matcher).finish(),
            Self::Regex {
                source,
                pattern,
                capture,
                ..
            } => {
                let config = pattern.get_config();
                let group_info = pattern.group_info();
                f.debug_struct("Regex")
                    .field("pattern", source)
                    .field("pattern_len", &pattern.pattern_len())
                    .field(
                        "groups",
                        &group_info
                            .all_names()
                            .collect::<smallvec::SmallVec<[_; 4]>>(),
                    )
                    .field("match_kind", &config.get_match_kind())
                    .field("capture", capture)
                    .field("memory_usage", &pattern.memory_usage())
                    .finish()
            }
            Self::Numeric {
                format, capture, ..
            } => f
                .debug_struct("Numeric")
                .field("format", format)
                .field("capture", capture)
                .finish(),
            Self::Bind { name, ty, .. } => f
                .debug_struct("Bind")
                .field("name", name)
                .field("ty", ty)
                .finish(),
            Self::Drop => f.write_str("Drop"),
            Self::Constraint {
                constraint, expr, ..
            } => f
                .debug_struct("Constraint")
                .field("constraint", constraint)
                .field("expr", expr)
                .finish(),
            Self::Substitution { expr, ty, .. } => f
                .debug_struct("Substitution")
                .field("expr", expr)
                .field("ty", ty)
                .finish(),
        }
    }
}

struct Searcher<'a, 'input> {
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
impl<'a, 'input> Searcher<'a, 'input> {
    fn new(span: SourceSpan, input: Input<'input>, captures: &'a mut Captures) -> Self {
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

    #[inline]
    fn part_matched<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            Bound::Unbounded => self.match_start,
            Bound::Included(&start) => start,
            Bound::Excluded(&start) => start + 1,
        };
        let end = match range.end_bound() {
            Bound::Unbounded => self.cursor.end(),
            Bound::Included(&end) => end + 1,
            Bound::Excluded(&end) => end,
        };
        self.match_start = start;
        self.match_end = end;
        self.cursor.set_start(end);
    }

    fn reset(&mut self, start: usize) {
        self.captures.set_pattern(None);
        self.cursor = self.input;
        self.stack.clear();
        self.match_start = start;
        self.match_end = start;
        self.cursor.set_start(start);
    }

    fn search_captures<'context, C>(
        mut self,
        ops: &[MatchOp<'a>],
        context: &mut C,
    ) -> DiagResult<()>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        use std::ops::ControlFlow;
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
        use std::ops::ControlFlow;

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
            self.stack.push(Operand::Captured(captured.clone()));
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
                        self.stack.push(Operand::Captured(captured.clone()));
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
            .map_err(|error| super::regex::build_error_to_diagnostic(error, 1, |_| span))?;
        self.match_regex(span, &pattern, context)
    }
}

fn evaluate_expr<'input>(
    span: SourceSpan,
    expr: &Expr,
    context: &ContextGuard<'_, 'input, '_>,
) -> DiagResult<Either<Number, Cow<'input, str>>> {
    use crate::expr::BinaryOp;
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
