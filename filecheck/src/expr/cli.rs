use std::str::FromStr;

use litcheck::variables::TypedVariable;

use crate::common::*;

use super::{Value, parser::NumericVarParser};

#[derive(Debug, Clone)]
pub struct CliVariable(super::Variable<'static>);

impl CliVariable {
    #[inline(always)]
    pub const fn name(&self) -> VariableName {
        self.0.name
    }

    #[inline(always)]
    pub const fn value(&self) -> &Value<'static> {
        &self.0.value
    }
}

impl FromStr for CliVariable {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let span = SourceSpan::from_range_unchecked(SourceId::UNKNOWN, 0..input.len());
        <CliVariable as TypedVariable>::try_parse(Span::new(span, input))
            .map_err(|err| litcheck::reporting::PrintDiagnostic::new(err).to_string())
    }
}

impl TypedVariable for CliVariable {
    type Key<'a> = Symbol;
    type Value<'a> = Value<'static>;
    type Variable<'a> = CliVariable;

    fn try_parse<'input>(input: Span<&'input str>) -> Result<Self::Variable<'input>, Report> {
        if input.starts_with('#') {
            let mut parser = NumericVarParser;
            let var = parser.parse(input)?;
            let name = var.name;
            Ok(Self(match var.value {
                Value::Undef => super::Variable::new(name, Value::Undef),
                Value::Num(n) => super::Variable::new(name, Value::Num(n)),
                Value::Str(s) => super::Variable::new(name, Value::Str(Cow::Owned(s.into_owned()))),
            }))
        } else {
            super::Variable::<'static>::try_parse(input).map(Self)
        }
    }
}
