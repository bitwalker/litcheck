use std::collections::BTreeMap;

use regex_automata::{util::captures::GroupInfo, PatternID};

use crate::{
    ast::{Capture, RegexPattern},
    common::*,
    expr::{TypedVariable, ValueType},
    pattern::matcher::regex,
};

use super::{CaptureGroup, MatchOp, SmartMatcher};

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
            } => self.capture(name.into_inner(), Span::new(span, pattern.into_inner())),
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
            } => Ok(self.capture_numeric(span, name.into_inner(), format)),
            Match::Numeric {
                span,
                format,
                capture: Some(name),
                constraint,
                expr: Some(expr),
            } => Ok(self.capture_numeric_with_constraint(
                span,
                name.into_inner(),
                format,
                constraint,
                expr,
            )),
        }
    }

    pub fn literal(&mut self, pattern: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.parts.push(MatchOp::Literal(pattern));
        Ok(self)
    }

    pub fn regex(&mut self, source: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.regex_pattern(RegexPattern::new(source))
    }

    pub fn regex_pattern(&mut self, source: RegexPattern<'a>) -> DiagResult<&mut Self> {
        let pattern = Regex::new(source.as_ref())
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| source.span()))?;

        let groups = pattern.group_info();
        let pattern_id = self.register_pattern_group(source.span(), groups)?;
        let mut captures = SmallVec::<[CaptureGroup; 1]>::default();
        for capture in source.captures.into_iter() {
            if let Capture::Ignore(_) = capture {
                continue;
            }
            if let Some(name) = capture.group_name() {
                let group_name = self.interner.resolve(name);
                let group_id = groups
                    .to_index(PatternID::ZERO, group_name)
                    .unwrap_or_else(|| panic!("expected group for capture of '{group_name}'"));
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
        let group = self.register_unnamed_group(
            PatternID::ZERO,
            Capture::All(Span::new(span, ValueType::Number(format))),
        );
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
        let group_name = self.interner.resolve(name);
        let span = source.span();
        let source = format!("(?P<{group_name}>{source})");
        let pattern = Regex::new(&source)
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| source.span()))?;
        let groups = pattern.group_info();
        let pattern_id = self.register_pattern_group(span, groups)?;

        let group_name = self.interner.resolve(name);
        let group_id = groups
            .to_index(PatternID::ZERO, group_name)
            .unwrap_or_else(|| panic!("expected group for capture of '{group_name}'"));
        let captures = smallvec![CaptureGroup {
            pattern_id,
            group_id,
            info: Capture::Explicit(TypedVariable {
                name: VariableName::User(Span::new(span, name)),
                ty: ValueType::String
            }),
        }];

        self.parts.push(MatchOp::Regex {
            source: Span::new(span, Cow::Owned(source)),
            pattern,
            captures,
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
        let group = self.register_named_group(
            PatternID::ZERO,
            Capture::Explicit(TypedVariable {
                name: VariableName::User(Span::new(span, name)),
                ty: ValueType::Number(format),
            }),
        );
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
        let group = self.register_named_group(
            PatternID::ZERO,
            Capture::Explicit(TypedVariable {
                name: VariableName::User(Span::new(span, name)),
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
            span,
            name,
            ty: Some(ValueType::Number(format)),
        });
        self
    }

    /// This permits us to store captures for nested patterns in the same [Captures]
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
                                .with_label(Label::new(span, label))
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

    fn register_unnamed_group(&mut self, pid: PatternID, capture: Capture) -> CaptureGroup {
        let pattern_group = &mut self.pattern_groups[pid.as_usize()];
        let group_id = pattern_group.len();
        pattern_group.push(None);
        CaptureGroup {
            pattern_id: pid,
            group_id,
            info: Capture::All(Span::new(capture.span(), capture.value_type())),
        }
    }

    fn register_named_group(&mut self, pid: PatternID, capture: Capture) -> CaptureGroup {
        use std::collections::btree_map::Entry;

        let group_name = capture.group_name().unwrap();
        let name = self.interner.resolve(group_name);
        let pattern_group_index = pid.as_usize();
        let (name, mapped) = match self.group_name_ids[pattern_group_index].entry(group_name) {
            Entry::Vacant(entry) => {
                entry.insert(0);
                (Cow::Owned(name.to_string()), false)
            }
            Entry::Occupied(mut entry) => {
                let next_id = entry.get_mut();
                let id = *next_id;
                *next_id += 1;
                (Cow::Owned(format!("{name}[{id}]")), true)
            }
        };
        let group_name = if mapped {
            self.interner.get_or_intern(&name)
        } else {
            group_name
        };
        let info = if !mapped {
            capture
        } else {
            match capture {
                Capture::Implicit(with) => Capture::Mapped {
                    group: group_name,
                    with,
                },
                Capture::Explicit(with) => Capture::Mapped {
                    group: group_name,
                    with,
                },
                Capture::Mapped { with, .. } => Capture::Mapped {
                    group: group_name,
                    with,
                },
                capture => capture,
            }
        };
        let pattern_group = &mut self.pattern_groups[pattern_group_index];
        let group_id = pattern_group.len();
        pattern_group.push(Some(name));
        CaptureGroup {
            pattern_id: pid,
            group_id,
            info,
        }
    }
}
