use litcheck::diagnostics::{Diagnostic, SourceSpan};

#[derive(Debug, Diagnostic, thiserror::Error, Clone)]
pub enum LexerError {
    #[error("invalid CHECK-COUNT")]
    #[diagnostic()]
    BadCount {
        #[label("the count value for this directive was invalid: {error}")]
        span: SourceSpan,
        #[source]
        error: core::num::ParseIntError,
    },
    #[error("invalid number")]
    #[diagnostic()]
    InvalidNumber {
        #[label("this numeric value could not be parsed: {error}")]
        span: SourceSpan,
        #[source]
        error: core::num::ParseIntError,
    },
    #[error("invalid identifier")]
    #[diagnostic()]
    InvalidIdentifier {
        #[label("this identifier has characters outside the allowed alphabet")]
        span: SourceSpan,
    },
    #[error("unexpected character '{unexpected}'")]
    #[diagnostic()]
    UnexpectedCharacter {
        #[label("'{unexpected}' is not valid here")]
        span: SourceSpan,
        unexpected: char,
    },
}
