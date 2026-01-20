use regex_automata::{PatternID, util::captures::GroupInfo};

use crate::{
    ast::{Capture, RegexPattern},
    common::*,
    expr::{TypedVariable, ValueType},
    pattern::matcher::{SubstringMatcher, regex},
};

use super::{CaptureGroup, MatchOp, SmartMatcher};

pub struct SmartMatcherBuilder<'a, 'config> {
    span: SourceSpan,
    config: &'config Config,
    parts: Vec<MatchOp<'a>>,
    raw_group_info: Vec<Vec<Option<Symbol>>>,
}

impl<'a, 'config> SmartMatcherBuilder<'a, 'config> {
    pub fn new(span: SourceSpan, config: &'config Config) -> Self {
        Self {
            span,
            config,
            parts: vec![],
            raw_group_info: vec![vec![None]],
        }
    }

    pub fn build(self) -> SmartMatcher<'a> {
        SmartMatcher::new(self.span, self.parts, self.raw_group_info)
    }

    pub fn lower_match(&mut self, part: crate::ast::Match<'a>) -> DiagResult<&mut Self> {
        use crate::ast::Match;

        match part {
            Match::Substitution {
                name,
                pattern: None,
                ..
            } => Ok(self.substitution(Expr::Var(name))),
            Match::Substitution {
                span,
                name,
                pattern: Some(pattern),
            } => self.capture(name, Span::new(span, pattern.into_inner())),
            Match::Numeric {
                span,
                format,
                capture: None,
                expr: None,
                ..
            } => Ok(self.numeric(span, format)),
            Match::Numeric {
                format,
                capture: None,
                expr: Some(expr),
                ..
            } => Ok(self.substitution_with_format(expr, ValueType::Number(format))),
            Match::Numeric {
                span,
                format,
                capture: Some(name),
                expr: None,
                ..
            } => Ok(self.capture_numeric(span, name, format)),
            Match::Numeric {
                span,
                format,
                capture: Some(name),
                constraint,
                expr: Some(expr),
            } => Ok(self.capture_numeric_with_constraint(span, name, format, constraint, expr)),
        }
    }

    pub fn literal(&mut self, pattern: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.register_empty_pattern_group();
        // We support both anchored/unanchored searches when the pattern is the
        // prefix for the overall SmartMatcher, because the SmartMatcher might
        // be used in both ways. Otherwise, we know the searches are anchored.
        let matcher = if self.parts.is_empty() {
            SubstringMatcher::new_with_start_kind(
                pattern,
                aho_corasick::StartKind::Both,
                self.config,
            )?
        } else {
            SubstringMatcher::new_with_start_kind(
                pattern,
                aho_corasick::StartKind::Anchored,
                self.config,
            )?
        };
        self.parts.push(MatchOp::Literal(matcher));
        Ok(self)
    }

    pub fn regex(&mut self, source: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.regex_pattern(RegexPattern::new(source))
    }

    pub fn regex_pattern(&mut self, source: RegexPattern<'a>) -> DiagResult<&mut Self> {
        let pattern = Regex::builder()
            .syntax(
                regex_automata::util::syntax::Config::new()
                    .multi_line(true)
                    .case_insensitive(self.config.options.ignore_case),
            )
            .build(source.as_ref())
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| source.span()))?;

        let groups = pattern.group_info();
        let pattern_id = self.register_pattern_group(groups);
        let mut captures = SmallVec::<[CaptureGroup; 1]>::default();
        for capture in source.captures.into_iter() {
            if let Capture::Ignore(_) = capture {
                continue;
            }
            if let Some(name) = capture.group_name() {
                let group_id = groups
                    .to_index(PatternID::ZERO, name.as_str())
                    .unwrap_or_else(|| panic!("expected group for capture of '{name}'"));
                captures.push(CaptureGroup {
                    pattern_id,
                    group_id,
                    info: capture,
                });
            } else {
                captures.push(CaptureGroup {
                    pattern_id,
                    group_id: 0,
                    info: capture,
                });
            }
        }

        self.parts.push(MatchOp::Regex {
            source: source.pattern,
            pattern,
            captures,
        });

        Ok(self)
    }

    pub fn numeric(&mut self, span: SourceSpan, format: NumberFormat) -> &mut Self {
        self.register_empty_pattern_group();
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
        let pattern_id = self.register_empty_pattern_group();
        let group =
            self.register_unnamed_group(pattern_id, Span::new(span, ValueType::Number(format)));
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
        self.register_empty_pattern_group();
        self.parts.push(MatchOp::Substitution { expr, ty: None });
        self
    }

    pub fn substitution_with_format(&mut self, expr: Expr, ty: ValueType) -> &mut Self {
        self.register_empty_pattern_group();
        self.parts
            .push(MatchOp::Substitution { expr, ty: Some(ty) });
        self
    }

    pub fn capture(
        &mut self,
        name: VariableName,
        source: Span<Cow<'a, str>>,
    ) -> DiagResult<&mut Self> {
        let symbol = name.into_inner();
        let group_name = symbol.as_str();
        let span = source.span();
        let source = format!("(?P<{group_name}>{source})");
        let pattern = Regex::builder()
            .syntax(
                regex_automata::util::syntax::Config::new()
                    .multi_line(true)
                    .case_insensitive(self.config.options.ignore_case),
            )
            .build(&source)
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| span))?;
        let groups = pattern.group_info();
        let pattern_id = self.register_pattern_group(groups);

        let group_id = groups
            .to_index(PatternID::ZERO, group_name)
            .unwrap_or_else(|| panic!("expected group for capture of '{group_name}'"));
        let captures = smallvec![CaptureGroup {
            pattern_id,
            group_id,
            info: Capture::Explicit(TypedVariable {
                name,
                ty: ValueType::String
            }),
        }];

        self.parts.push(MatchOp::Regex {
            source: Span::new(span, Cow::Owned(source)),
            pattern,
            captures,
        });
        self.parts.push(MatchOp::Bind { name, ty: None });

        Ok(self)
    }

    pub fn capture_numeric(
        &mut self,
        span: SourceSpan,
        name: VariableName,
        format: NumberFormat,
    ) -> &mut Self {
        let pattern_id = self.register_empty_pattern_group();
        let group = self.register_named_group(
            pattern_id,
            Capture::Explicit(TypedVariable {
                name,
                ty: ValueType::Number(format),
            }),
        );
        self.parts.push(MatchOp::Numeric {
            span,
            format,
            capture: Some(group),
        });
        self.parts.push(MatchOp::Bind {
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    pub fn capture_numeric_with_constraint(
        &mut self,
        span: SourceSpan,
        name: VariableName,
        format: NumberFormat,
        constraint: Constraint,
        expr: Expr,
    ) -> &mut Self {
        let pattern_id = self.register_empty_pattern_group();
        let group = self.register_named_group(
            pattern_id,
            Capture::Explicit(TypedVariable {
                name,
                ty: ValueType::Number(format),
            }),
        );
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
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    /// Register a new [Captures] pattern
    fn register_pattern_group(&mut self, group_info: &GroupInfo) -> PatternID {
        assert_eq!(
            group_info.pattern_len(),
            1,
            "unsupported multi-pattern pattern group registration"
        );
        let pid = PatternID::new_unchecked(self.raw_group_info.len());
        let mut groups = vec![];
        for group_name in group_info.pattern_names(PatternID::ZERO) {
            match group_name {
                None => {
                    groups.push(None);
                }
                Some(group_name) => {
                    groups.push(Some(Symbol::intern(group_name)));
                }
            }
        }

        self.raw_group_info.push(groups);

        pid
    }

    fn register_empty_pattern_group(&mut self) -> PatternID {
        let pid = PatternID::new_unchecked(self.raw_group_info.len());
        self.raw_group_info.push(vec![None]);
        pid
    }

    fn register_unnamed_group(
        &mut self,
        pid: PatternID,
        capture_type: Span<ValueType>,
    ) -> CaptureGroup {
        let pattern_group = &mut self.raw_group_info[pid.as_usize()];
        let group_id = pattern_group.len();
        pattern_group.push(None);
        CaptureGroup {
            pattern_id: pid,
            group_id,
            info: Capture::All(capture_type),
        }
    }

    fn register_named_group(&mut self, pid: PatternID, capture: Capture) -> CaptureGroup {
        let group_name = capture.group_name().unwrap();
        let pattern_id = pid.as_usize();
        let pattern_groups = &mut self.raw_group_info[pattern_id];
        let group_id = pattern_groups.len();
        pattern_groups.push(Some(group_name));
        CaptureGroup {
            pattern_id: pid,
            group_id,
            info: capture,
        }
    }
}
