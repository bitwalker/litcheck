#![expect(unused_assignments)]

use crate::common::*;

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
            Self::Unknown => SourceSpan::UNKNOWN,
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
