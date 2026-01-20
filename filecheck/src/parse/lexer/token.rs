use std::{borrow::Cow, fmt};

use crate::ast::{Check, CheckModifier};

use super::LexerError;

/// The token type produced by [Lexer]
#[derive(Debug, Clone)]
pub enum Token<'input> {
    Eof,
    Lf,
    Error(LexerError),
    Check(Check),
    Modifier(CheckModifier),
    Comment(Cow<'input, str>),
    Raw(&'input str),
    Ident(&'input str),
    Num(i128),
    // '[['
    MatchStart,
    // ']]'
    MatchEnd,
    // '{{'
    RegexStart,
    // '}}'
    RegexEnd,
    // '{'
    LBrace,
    // '}'
    RBrace,
    // '('
    LParen,
    // ')'
    RParen,
    // '#'
    Hash,
    // '$'
    Dollar,
    // '%'
    Percent,
    // '@'
    At,
    // ','
    Comma,
    // '.'
    Dot,
    // ':'
    Colon,
    // '+'
    Plus,
    // '-'
    Minus,
    // '='
    Equal,
    // '=='
    Equals,
    // LITERAL
    Literal,
    // LINE
    Line,
    // add
    Add,
    // sub
    Sub,
    // mul
    Mul,
    // div
    Div,
    // min
    Min,
    // max
    Max,
}
impl<'input> Token<'input> {
    pub fn from_keyword_or_ident(s: &'input str) -> Self {
        match s {
            "LITERAL" => Self::Literal,
            "LINE" => Self::Line,
            "add" => Self::Add,
            "sub" => Self::Sub,
            "mul" => Self::Mul,
            "div" => Self::Div,
            "min" => Self::Min,
            "max" => Self::Max,
            _ => Self::Ident(s),
        }
    }
}
impl<'input> Eq for Token<'input> {}
impl<'input> PartialEq for Token<'input> {
    fn eq(&self, other: &Token<'input>) -> bool {
        match (self, other) {
            (Self::Error(_), Self::Error(_)) => true,
            (Self::Check(a), Self::Check(b)) => a == b,
            (Self::Modifier(a), Self::Modifier(b)) => a == b,
            (Self::Comment(a), Self::Comment(b)) => a == b,
            (Self::Raw(a), Self::Raw(b)) => a == b,
            (Self::Ident(a), Self::Ident(b)) => a == b,
            (Self::Num(a), Self::Num(b)) => a == b,
            (a, b) => core::mem::discriminant(a) == core::mem::discriminant(b),
        }
    }
}
impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Write;
        match self {
            Self::Eof => f.write_str("end of file"),
            Self::Lf => f.write_str("newline"),
            Self::Error(_) => f.write_str("error"),
            Self::Check(c) => write!(f, "{c}"),
            Self::Modifier(c) => write!(f, "{c:?}"),
            Self::Comment(_) => write!(f, "a comment"),
            Self::Raw(s) => f.write_str(s),
            Self::Ident(s) => f.write_str(s),
            Self::Num(n) => write!(f, "{n}"),
            Self::MatchStart => f.write_str("[["),
            Self::MatchEnd => f.write_str("]]"),
            Self::RegexStart => f.write_str("{{"),
            Self::RegexEnd => f.write_str("}}"),
            Self::LBrace => f.write_char('{'),
            Self::RBrace => f.write_char('}'),
            Self::LParen => f.write_char('('),
            Self::RParen => f.write_char(')'),
            Self::Hash => f.write_char('#'),
            Self::Dollar => f.write_char('$'),
            Self::Percent => f.write_char('%'),
            Self::At => f.write_char('@'),
            Self::Colon => f.write_char(':'),
            Self::Comma => f.write_char(','),
            Self::Dot => f.write_char('.'),
            Self::Plus => f.write_char('+'),
            Self::Minus => f.write_char('-'),
            Self::Equal => f.write_str("="),
            Self::Equals => f.write_str("=="),
            Self::Line => f.write_str("LINE"),
            Self::Literal => f.write_str("LITERAL"),
            Self::Add => f.write_str("add"),
            Self::Sub => f.write_str("sub"),
            Self::Mul => f.write_str("mul"),
            Self::Div => f.write_str("div"),
            Self::Min => f.write_str("min"),
            Self::Max => f.write_str("max"),
        }
    }
}
