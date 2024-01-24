/// Simple macro used in the grammar definition for constructing spans
macro_rules! span {
    ($l:expr, $r:expr) => {
        litcheck::diagnostics::SourceSpan::from($l..$r)
    };
    ($i:expr) => {
        litcheck::diagnostics::SourceSpan::from($i..$i)
    };
}

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/expr/grammar.rs"
);

use std::fmt;

use logos::{Lexer, Logos};

use litcheck::{diagnostics::SourceSpan, StringInterner};

use crate::expr::{ExprError, Var};

pub type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, ExprError>;

pub struct NumericVarParser<'config> {
    interner: &'config mut StringInterner,
}
impl<'config> NumericVarParser<'config> {
    pub fn new(interner: &'config mut StringInterner) -> Self {
        Self { interner }
    }

    pub fn parse<'a>(&mut self, source: &'a str) -> Result<Var<'a>, ExprError> {
        let lexer = Token::lexer(source)
            .spanned()
            .map(|(t, span)| t.map(|t| (span.start, t, span.end)));
        grammar::NumericVarParser::new()
            .parse(source, self.interner, lexer)
            .map_err(handle_parse_error)
    }
}

fn handle_parse_error(err: ParseError) -> ExprError {
    match err {
        ParseError::InvalidToken { location: at } => ExprError::InvalidToken {
            span: SourceSpan::from(at..at),
        },
        ParseError::UnrecognizedToken {
            token: (l, tok, r),
            expected,
        } => ExprError::UnrecognizedToken {
            span: SourceSpan::from(l..r),
            token: tok.to_string(),
            expected,
        },
        ParseError::ExtraToken { token: (l, tok, r) } => ExprError::ExtraToken {
            span: SourceSpan::from(l..r),
            token: tok.to_string(),
        },
        ParseError::UnrecognizedEof {
            location: at,
            expected,
        } => ExprError::UnrecognizedEof {
            span: SourceSpan::from(at..at),
            expected,
        },
        ParseError::User { error } => error,
    }
}

#[derive(Debug, Clone, Logos)]
#[logos(error = ExprError)]
#[logos(skip r"[ \t]+")]
pub enum Token<'input> {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("#")]
    Hash,
    #[token("$")]
    Dollar,
    #[token("%")]
    Percent,
    #[token("@")]
    At,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("=")]
    Equal,
    #[token("==")]
    Equals,
    #[token("add")]
    Add,
    #[token("sub")]
    Sub,
    #[token("mul")]
    Mul,
    #[token("div")]
    Div,
    #[token("min")]
    Min,
    #[token("max")]
    Max,
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice())]
    Ident(&'input str),
    #[regex(r"-?[0-9]+", parse_int)]
    Num(i64),
}
impl<'input> Eq for Token<'input> {}
impl<'input> PartialEq for Token<'input> {
    fn eq(&self, other: &Token<'input>) -> bool {
        match (self, other) {
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
            Self::Ident(s) => f.write_str(s),
            Self::Num(n) => write!(f, "{n}"),
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
            Self::Add => f.write_str("add"),
            Self::Sub => f.write_str("sub"),
            Self::Mul => f.write_str("mul"),
            Self::Div => f.write_str("div"),
            Self::Min => f.write_str("min"),
            Self::Max => f.write_str("max"),
        }
    }
}

fn parse_int<'input>(lexer: &mut Lexer<'input, Token<'input>>) -> Result<i64, ExprError> {
    lexer
        .slice()
        .parse::<i64>()
        .map_err(|error| ExprError::Number {
            span: SourceSpan::from(lexer.span()),
            error,
        })
}
