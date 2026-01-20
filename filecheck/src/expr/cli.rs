use std::str::FromStr;

use litcheck::variables::{self, TypedVariable, VariableError};

use crate::common::*;

use super::{parser::NumericVarParser, Var};

#[derive(Debug, Clone)]
pub struct CliVariable {
    pub name: variables::VariableName<Box<str>>,
    pub value: Value<'static>,
}
impl FromStr for CliVariable {
    type Err = Report;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let span = SourceSpan::from_range_unchecked(SourceId::UNKNOWN, 0..input.len());
        <CliVariable as TypedVariable>::try_parse(Span::new(span, input))
    }
}
impl CliVariable {
    pub(crate) fn from_numeric_var(var: Var<'_>, interner: &StringInterner) -> Self {
        let name = var
            .name
            .map(|sym| interner.resolve(sym).to_string().into_boxed_str());
        let value = match var.value {
            Value::Undef => Value::Undef,
            Value::Str(s) => Value::Str(Cow::Owned(s.to_string())),
            Value::Num(n) => Value::Num(n),
        };
        Self { name, value }
    }
}
impl TypedVariable for CliVariable {
    type Key<'a> = Box<str>;
    type Value<'a> = Value<'static>;

    fn try_parse(input: Span<&str>) -> Result<Self, Report> {
        let (span, s) = input.into_parts();
        if s.is_empty() {
            Err(VariableError::Empty(span)
                .into_report()
                .with_source_code(s.to_string()))
        } else if s.starts_with('#') {
            let mut interner = StringInterner::new();
            let mut parser = NumericVarParser::new(&mut interner);
            let var = parser.parse(input)?;
            Ok(CliVariable::from_numeric_var(var, &interner))
        } else if let Some((k, v)) = s.split_once('=') {
            if k.is_empty() {
                return Err(VariableError::EmptyName(span)
                    .into_report()
                    .with_source_code(s.to_string()));
            }
            if !variables::is_valid_variable_name(k) {
                return Err(miette::miette!(
                    labels = vec![Label::at(SourceSpan::from_range_unchecked(
                        span.source_id(),
                        0..k.len()
                    ))
                    .into()],
                    help = "must be non-empty, and match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                    "invalid variable name"
                )
                .with_source_code(s.to_string()));
            }
            let k = k.to_string().into_boxed_str();
            let v = if v.is_empty() {
                Value::Undef
            } else {
                Value::Str(Cow::Owned(v.to_string()))
            };
            let span = SourceSpan::from_range_unchecked(span.source_id(), 0..k.len());
            Ok(CliVariable {
                name: variables::VariableName::User(Span::new(span, k)),
                value: v,
            })
        } else {
            Err(VariableError::MissingEquals(span)
                .into_report()
                .with_source_code(s.to_string()))
        }
    }
}
