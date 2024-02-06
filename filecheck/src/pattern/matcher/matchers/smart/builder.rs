use std::collections::BTreeMap;

use regex_automata::{util::captures::GroupInfo, PatternID};

use crate::{common::*, expr::ValueType, pattern::matcher::regex};

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

    pub fn literal(&mut self, pattern: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        self.parts.push(MatchOp::Literal(pattern));
        Ok(self)
    }

    pub fn regex(&mut self, source: Span<Cow<'a, str>>) -> DiagResult<&mut Self> {
        let pattern = Regex::new(source.as_ref())
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| source.span()))?;
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
        let pattern = Regex::new(&format!("(?P<{group_name}>{source})"))
            .map_err(|error| regex::build_error_to_diagnostic(error, 1, |_| source.span()))?;

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
