mod lexer;
mod parser;
#[cfg(test)]
mod tests;

use crate::common::*;

pub use self::lexer::{Lexed, Lexer, LexerError, Token};
pub use self::parser::CheckFileParser;

pub type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, ParserError>;

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum ParserError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(#[from] LexerError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    Expr(#[from] crate::expr::ExprError),
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
    #[error("invalid check type")]
    #[diagnostic()]
    InvalidCheckType {
        #[label("this is not a valid check type")]
        span: SourceSpan,
    },
    #[error("invalid pattern")]
    #[diagnostic(help("CHECK-EMPTY should be used to check for empty lines"))]
    EmptyPattern {
        #[label("expected a non-empty pattern")]
        span: SourceSpan,
    },
    #[error("invalid check modifier")]
    #[diagnostic()]
    InvalidCheckModifier {
        #[label("this modifier is not recognized, valid modifiers are: LITERAL")]
        span: SourceSpan,
    },
    #[error("unclosed substitution block")]
    #[diagnostic()]
    UnclosedSubstitution {
        #[label("no closing ']]' found for this block")]
        span: SourceSpan,
    },
    #[error("unclosed regex block")]
    #[diagnostic()]
    UnclosedRegex {
        #[label("no closing '}}' found for this block")]
        span: SourceSpan,
    },
}

pub type ParseResult<T> = Result<T, ParserError>;

pub trait Parser<'parser> {
    type Value<'a>
    where
        'a: 'parser;

    fn parse<'a: 'parser, S>(&mut self, code: &'a S) -> ParseResult<Self::Value<'a>>
    where
        S: SourceFile + ?Sized + 'a;
}
