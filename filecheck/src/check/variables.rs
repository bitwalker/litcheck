use std::collections::{BTreeMap, BTreeSet};

use crate::{ast::*, common::*, env::Bindings, expr::ValueType};

/// Validate declarations using the same system, global, and local scopes as matching.
pub(super) fn validate(
    lines: &[CheckLine<'_>],
    implicit: &[CheckLine<'_>],
    config: &Config,
) -> DiagResult<()> {
    let system = config
        .options
        .variables
        .iter()
        .map(|variable| {
            let declaration = match variable.value() {
                Value::Num(expr) => Declaration::Expression(expr.clone()),
                Value::Str(_) | Value::Undef => Declaration::String,
            };
            (variable.name().into_inner(), declaration)
        })
        .collect();
    let mut declarations = Declarations {
        config,
        variables: Bindings::new_with_system(system),
    };
    let mut previous = BTreeMap::new();
    // CLI expressions resolve lazily against the complete system environment. Audit each
    // declaration in argument order without prematurely defaulting forward references.
    for variable in &config.options.variables {
        let name = variable.name();
        let declaration = match variable.value() {
            Value::Num(expr) => Declaration::Expression(expr.clone()),
            Value::Str(_) | Value::Undef => Declaration::String,
        };
        if let Some(prior) = previous.insert(name, declaration.clone()) {
            let source = config
                .source_manager()
                .load_argument("-D", variable.definition().to_owned());
            let span = SourceSpan::from_range_unchecked(
                source.id(),
                name.span().into_slice_index().into(),
            );
            declarations.check_redefinition(name, span, &prior, &declaration)?;
        }
    }
    for line in lines {
        declarations.excluded(implicit)?;
        if config.options.enable_var_scope && matches!(line.kind(), Check::Label) {
            declarations.variables.clear();
            declarations.excluded(implicit)?;
        }
        if matches!(line.kind(), Check::Not) {
            // Each excluded pattern is evaluated in its own temporary environment.
            declarations.clone().pattern(&line.pattern)?;
        } else {
            declarations.pattern(&line.pattern)?;
        }
    }
    declarations.excluded(implicit)?;
    Ok(())
}

#[derive(Debug, Clone)]
enum Declaration {
    String,
    Number(NumberFormat),
    Expression(Expr),
}

/// Leave unknown dependencies and cycles unresolved rather than assigning a speculative
/// unsigned format during declaration validation.
struct UnresolvedFormat;

#[derive(Clone)]
struct Declarations<'a> {
    config: &'a Config,
    variables: Bindings<Declaration>,
}

impl Declarations<'_> {
    fn excluded(&self, patterns: &[CheckLine<'_>]) -> DiagResult<()> {
        for pattern in patterns {
            self.clone().pattern(&pattern.pattern)?;
        }
        Ok(())
    }

    fn pattern(&mut self, pattern: &CheckPattern<'_>) -> DiagResult<()> {
        let mut defined_here = BTreeSet::new();
        match pattern {
            CheckPattern::Match(parts) => {
                for part in parts.iter() {
                    match part {
                        CheckPatternPart::Match(Match::Substitution {
                            name,
                            pattern: Some(_),
                            ..
                        }) => {
                            self.define(*name, Declaration::String)?;
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            capture,
                            format,
                            expr,
                            ..
                        }) => {
                            if let Some(expr) = expr {
                                self.check_uses(expr, &defined_here)?;
                            }
                            if let Some(name) = capture {
                                let declaration = if let Some(format) = format {
                                    Declaration::Number(*format)
                                } else if let Some(expr) = expr {
                                    match self.format(expr, &mut BTreeSet::new()) {
                                        Ok(format) => {
                                            Declaration::Number(format.unwrap_or_default())
                                        }
                                        Err(_) => Declaration::Expression(expr.clone()),
                                    }
                                } else {
                                    Declaration::Number(NumberFormat::default())
                                };
                                self.define(*name, declaration)?;
                                defined_here.insert(*name);
                            }
                        }
                        CheckPatternPart::Regex(regex) => {
                            self.captures(regex, &mut defined_here)?
                        }
                        _ => (),
                    }
                }
            }
            CheckPattern::Regex(regex) => self.captures(regex, &mut defined_here)?,
            CheckPattern::Literal(_) | CheckPattern::Empty(_) => (),
        }
        Ok(())
    }

    fn define(&mut self, name: VariableName, declaration: Declaration) -> DiagResult<()> {
        if let Some(previous) = self.variables.get(&name) {
            self.check_redefinition(name, name.span(), previous, &declaration)?;
        }
        self.variables.insert(name, declaration);
        Ok(())
    }

    fn check_redefinition(
        &self,
        name: VariableName,
        span: SourceSpan,
        before: &Declaration,
        after: &Declaration,
    ) -> DiagResult<()> {
        let message = match (before, after) {
            (Declaration::String, Declaration::String) => None,
            (_, Declaration::String) => Some(format!(
                "numeric variable with name '{name}' already exists"
            )),
            (Declaration::String, _) => {
                Some(format!("string variable with name '{name}' already exists"))
            }
            _ => match (
                self.declaration_format(before, &mut BTreeSet::new()),
                self.declaration_format(after, &mut BTreeSet::new()),
            ) {
                (Ok(before), Ok(after))
                    if before.unwrap_or_default() != after.unwrap_or_default() =>
                {
                    Some("format different from previous variable definition".to_owned())
                }
                _ => None,
            },
        };
        match message {
            Some(message) => Err(self.error(span, message)),
            None => Ok(()),
        }
    }

    fn captures(
        &mut self,
        regex: &RegexPattern<'_>,
        defined_here: &mut BTreeSet<VariableName>,
    ) -> DiagResult<()> {
        for capture in &regex.captures {
            if let Some(name) = capture.variable_name() {
                let declaration = match capture.value_type() {
                    ValueType::String => Declaration::String,
                    ValueType::Number(format) => Declaration::Number(format.unwrap_or_default()),
                };
                if matches!(declaration, Declaration::Number(_)) {
                    defined_here.insert(name);
                }
                self.define(name, declaration)?;
            }
        }
        Ok(())
    }

    fn check_uses(&self, expr: &Expr, defined_here: &BTreeSet<VariableName>) -> DiagResult<()> {
        match expr {
            Expr::Var(name) if defined_here.contains(name) => Err(self.error(
                name.span(),
                format!("numeric variable '{name}' defined earlier in the same CHECK directive"),
            )),
            Expr::Format { expr, .. } => self.check_uses(expr, defined_here),
            Expr::Binary { lhs, rhs, .. } => {
                self.check_uses(lhs, defined_here)?;
                self.check_uses(rhs, defined_here)
            }
            _ => Ok(()),
        }
    }

    fn declaration_format(
        &self,
        declaration: &Declaration,
        active: &mut BTreeSet<VariableName>,
    ) -> Result<Option<NumberFormat>, UnresolvedFormat> {
        match declaration {
            Declaration::String => Err(UnresolvedFormat),
            Declaration::Number(format) => Ok(Some(*format)),
            Declaration::Expression(expr) => self.format(expr, active),
        }
    }

    fn format(
        &self,
        expr: &Expr,
        active: &mut BTreeSet<VariableName>,
    ) -> Result<Option<NumberFormat>, UnresolvedFormat> {
        match expr {
            Expr::Num(number) => Ok(number.format),
            Expr::Var(VariableName::Pseudo(_)) => Ok(None),
            Expr::Var(name) => {
                if !active.insert(*name) {
                    return Err(UnresolvedFormat);
                }
                let format = self
                    .variables
                    .get(name)
                    .ok_or(UnresolvedFormat)
                    .and_then(|declaration| self.declaration_format(declaration, active));
                active.remove(name);
                format
            }
            Expr::Format { format, .. } => Ok(Some(*format)),
            Expr::Binary { lhs, rhs, .. } => {
                match (self.format(lhs, active)?, self.format(rhs, active)?) {
                    (None, None) => Ok(None),
                    (lhs, rhs) => NumberFormat::infer_expression_format(lhs, rhs)
                        .map(Some)
                        .ok_or(UnresolvedFormat),
                }
            }
        }
    }

    fn error(&self, span: SourceSpan, message: String) -> Report {
        let report = Report::new(Diag::new(message).with_label(Label::new(span, "occurs here")));
        match self.config.source_manager().get(span.source_id()) {
            Ok(source) => report.with_source_code(source),
            Err(_) => report,
        }
    }
}
