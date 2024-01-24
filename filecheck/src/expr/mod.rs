mod parser;

use std::{fmt, str::FromStr};

use litcheck::{
    diagnostics::{Diagnostic, LabeledSpan, Report, SourceSpan, Span, Spanned},
    variables::{self, TypedVariable, VariableError},
    StringInterner, Symbol,
};

use self::parser::NumericVarParser;

#[derive(Default, Diagnostic, Clone, Debug, thiserror::Error)]
pub enum ExprError {
    #[error("invalid token")]
    #[diagnostic()]
    InvalidToken {
        #[label("occurs here")]
        span: SourceSpan,
    },
    #[error("unrecognized token")]
    #[diagnostic(help("expected one of: {}", expected.as_slice().join(", ")))]
    UnrecognizedToken {
        #[label("lexed a {token} here")]
        span: SourceSpan,
        token: String,
        expected: Vec<String>,
    },
    #[error("unexpected trailing tokens")]
    #[diagnostic()]
    ExtraToken {
        #[label("{token} was found here, but was not expected")]
        span: SourceSpan,
        token: String,
    },
    #[error("unexpected end of file")]
    #[diagnostic(help("expected one of: {}", expected.as_slice().join(", ")))]
    UnrecognizedEof {
        #[label("reached end of file here")]
        span: SourceSpan,
        expected: Vec<String>,
    },
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidCall(#[from] InvalidCallExprError),
    #[error("invalid numeric expression")]
    #[diagnostic(help("make sure the value is parseable as a 64-bit signed integer"))]
    Number {
        #[label("{error}")]
        span: SourceSpan,
        #[source]
        error: core::num::ParseIntError,
    },
    #[error("invalid format specifier")]
    #[diagnostic()]
    InvalidFormatSpecifier {
        #[label("this numeric format specifier is not recognized")]
        span: SourceSpan,
    },
    #[error("invalid numeric precision")]
    #[diagnostic()]
    InvalidNumericPrecision {
        #[label("the precision value here is too large")]
        span: SourceSpan,
    },
    /// This is only used for the default implementation required by logos
    #[default]
    #[error("an unknown error occurred")]
    #[diagnostic()]
    Unknown,
}
impl PartialEq for ExprError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InvalidToken { .. }, Self::InvalidToken { .. }) => true,
            (
                Self::UnrecognizedToken {
                    token: at,
                    expected: a,
                    ..
                },
                Self::UnrecognizedToken {
                    token: bt,
                    expected: b,
                    ..
                },
            ) => at == bt && a == b,
            (Self::ExtraToken { token: a, .. }, Self::ExtraToken { token: b, .. }) => a == b,
            (
                Self::UnrecognizedEof { expected: a, .. },
                Self::UnrecognizedEof { expected: b, .. },
            ) => a == b,
            (Self::InvalidCall(a), Self::InvalidCall(b)) => a == b,
            (Self::Number { .. }, Self::Number { .. }) => true,
            (Self::InvalidFormatSpecifier { .. }, Self::InvalidFormatSpecifier { .. }) => true,
            (Self::InvalidNumericPrecision { .. }, Self::InvalidNumericPrecision { .. }) => true,
            (Self::Unknown, Self::Unknown) => true,
            _ => false,
        }
    }
}
impl Spanned for ExprError {
    fn span(&self) -> SourceSpan {
        match self {
            Self::InvalidCall(err) => err.span(),
            Self::InvalidToken { span, .. }
            | Self::UnrecognizedToken { span, .. }
            | Self::ExtraToken { span, .. }
            | Self::UnrecognizedEof { span, .. }
            | Self::Number { span, .. }
            | Self::InvalidFormatSpecifier { span, .. }
            | Self::InvalidNumericPrecision { span, .. } => *span,
            Self::Unknown => SourceSpan::from(0..0),
        }
    }
}

#[derive(Debug, Clone, Diagnostic, thiserror::Error)]
pub enum InvalidCallExprError {
    #[error("undefined function")]
    #[diagnostic()]
    Undefined {
        #[label("no such function defined in this context")]
        span: SourceSpan,
        callee: String,
    },
    #[error("function '{callee}' expects {expected} arguments, but was given {given}")]
    #[diagnostic()]
    InvalidArity {
        #[label("this function only takes {expected} arguments")]
        span: SourceSpan,
        callee: String,
        expected: u8,
        given: u8,
    },
}
impl Spanned for InvalidCallExprError {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Undefined { span, .. } | Self::InvalidArity { span, .. } => *span,
        }
    }
}
impl PartialEq for InvalidCallExprError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Undefined { callee: a, .. }, Self::Undefined { callee: b, .. }) => a == b,
            (
                Self::InvalidArity {
                    callee: a,
                    expected: ae,
                    given: ag,
                    ..
                },
                Self::InvalidArity {
                    callee: b,
                    expected: be,
                    given: bg,
                    ..
                },
            ) => a == b && ae == be && ag == bg,
            _ => false,
        }
    }
}

pub type VariableName = variables::VariableName<Symbol>;
pub type Variable<'a> = variables::Variable<Symbol, Value<'a>>;

#[derive(Debug, Clone)]
pub struct CliVariable {
    pub name: variables::VariableName<Box<str>>,
    pub value: Value<'static>,
}
impl FromStr for CliVariable {
    type Err = VariableError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let span = SourceSpan::from(0..input.as_bytes().len());
        <CliVariable as TypedVariable>::try_parse(Span::new(span, input))
    }
}
impl CliVariable {
    fn from_numeric_var(var: Var<'_>, interner: &StringInterner) -> Self {
        let name = var
            .name
            .map(|sym| interner.resolve(sym).to_string().into_boxed_str());
        let value = match var.value {
            Value::Undef => Value::Undef,
            Value::Str(s) => Value::Str(Box::leak::<'static>(s.to_string().into_boxed_str())),
            Value::Num(n) => Value::Num(n),
        };
        Self { name, value }
    }
}
impl TypedVariable for CliVariable {
    type Key<'a> = Box<str>;
    type Value<'a> = Value<'static>;

    fn try_parse(input: Span<&str>) -> Result<Self, VariableError> {
        let (span, s) = input.into_parts();
        if s.is_empty() {
            Err(VariableError::Empty(span))
        } else if s.starts_with('#') {
            let mut interner = StringInterner::new();
            let mut parser = NumericVarParser::new(&mut interner);
            let var = parser
                .parse(s)
                .map_err(|err| VariableError::Format(Report::new(err)))?;
            Ok(CliVariable::from_numeric_var(var, &interner))
        } else if let Some((k, v)) = s.split_once('=') {
            if k.is_empty() {
                return Err(VariableError::EmptyName(span));
            }
            if !variables::is_valid_variable_name(k) {
                return Err(VariableError::Name(miette::miette!(
                    labels = vec![LabeledSpan::new_with_span(
                        None,
                        SourceSpan::from(0..k.as_bytes().len())
                    )],
                    help = "must be non-empty, and match the pattern `[A-Za-z_][A-Za-z0-9_]*`",
                    "invalid variable name"
                )));
            }
            let k = k.to_string().into_boxed_str();
            let v = if v.is_empty() {
                Value::Undef
            } else {
                Value::Str(Box::leak::<'static>(v.to_string().into_boxed_str()))
            };
            let span = SourceSpan::from(0..(k.as_bytes().len()));
            Ok(CliVariable {
                name: variables::VariableName::User(Span::new(span, k)),
                value: v,
            })
        } else {
            Err(VariableError::MissingEquals(span))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var<'a> {
    pub name: VariableName,
    pub value: Value<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    Undef,
    Str(&'a str),
    Num(Expr),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    String,
    Number(NumberFormat),
}

#[derive(Debug, Clone)]
pub struct Number {
    pub span: SourceSpan,
    pub format: NumberFormat,
    pub value: i64,
}
impl Number {
    pub fn new(span: SourceSpan, value: i64) -> Self {
        Self {
            span,
            format: NumberFormat::default(),
            value,
        }
    }

    pub fn new_with_format(span: SourceSpan, value: i64, format: NumberFormat) -> Self {
        Self {
            span,
            format,
            value,
        }
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.format == other.format
    }
}
impl Spanned for Number {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.value;
        match self.format {
            NumberFormat::Unsigned { precision: 0 } => write!(f, "{}", value as u64),
            NumberFormat::Unsigned { precision: n } => {
                write!(f, "{:0n$}", value as u64, n = n as usize)
            }
            NumberFormat::Signed { precision: 0 } => write!(f, "{value}"),
            NumberFormat::Signed { precision: n } => write!(f, "{value:0n$}", n = n as usize),
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
            } => write!(f, "{value:#x?}"),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
            } => write!(f, "{value:x?}"),
            NumberFormat::Hex {
                require_prefix: true,
                precision: n,
            } => write!(f, "{value:#0n$x?}", n = n as usize),
            NumberFormat::Hex {
                require_prefix: false,
                precision: n,
            } => write!(f, "{value:0n$x?}", n = n as usize),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumberFormat {
    Unsigned { precision: u8 },
    Signed { precision: u8 },
    Hex { require_prefix: bool, precision: u8 },
}
impl Default for NumberFormat {
    fn default() -> Self {
        Self::Unsigned { precision: 0 }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum FormatSpecifier {
    #[default]
    Unsigned,
    Signed,
    Hex,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(Number),
    Var(VariableName),
    Binary {
        span: SourceSpan,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    #[allow(dead_code)]
    Call {
        span: SourceSpan,
        callee: Span<Symbol>,
        args: Vec<Expr>,
    },
}
impl Spanned for Expr {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Num(spanned) => spanned.span(),
            Self::Var(spanned) => spanned.span(),
            Self::Binary { span, .. } | Self::Call { span, .. } => *span,
        }
    }
}
impl Expr {
    pub fn from_call(
        interner: &mut StringInterner,
        span: SourceSpan,
        callee: Span<Symbol>,
        mut args: Vec<Expr>,
    ) -> Result<Self, InvalidCallExprError> {
        match args.len() {
            2 => {
                let op = match interner.resolve(callee.into_inner()) {
                    "add" => BinaryOp::Add,
                    "sub" => BinaryOp::Sub,
                    "mul" => BinaryOp::Mul,
                    "div" => BinaryOp::Div,
                    "min" => BinaryOp::Min,
                    "max" => BinaryOp::Max,
                    callee => {
                        return Err(InvalidCallExprError::Undefined {
                            span,
                            callee: callee.to_string(),
                        })
                    }
                };
                let rhs = Box::new(args.pop().unwrap());
                let lhs = Box::new(args.pop().unwrap());
                Ok(Self::Binary { span, op, lhs, rhs })
            }
            arity => match interner.resolve(*callee) {
                callee @ ("add" | "sub" | "mul" | "div" | "min" | "max") => {
                    Err(InvalidCallExprError::InvalidArity {
                        span,
                        callee: callee.to_string(),
                        expected: 2,
                        given: arity as u8,
                    })
                }
                callee => Err(InvalidCallExprError::Undefined {
                    span,
                    callee: callee.to_string(),
                }),
            },
        }
    }
}
impl Eq for Expr {}
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a == b,
            (Self::Var(a), Self::Var(b)) => a == b,
            (
                Self::Binary {
                    op: aop,
                    lhs: al,
                    rhs: ar,
                    ..
                },
                Self::Binary {
                    op: bop,
                    lhs: bl,
                    rhs: br,
                    ..
                },
            ) => aop == bop && al == bl && ar == br,
            (
                Self::Call {
                    callee: ac,
                    args: aargs,
                    ..
                },
                Self::Call {
                    callee: bc,
                    args: bargs,
                    ..
                },
            ) => ac == bc && aargs == bargs,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
}
