/// Simple macro used in the grammar definition for constructing spans
#[macro_export]
macro_rules! span {
    ($source_id:expr, $l:expr, $r:expr) => {
        litcheck::diagnostics::SourceSpan::new(
            $source_id,
            litcheck::range::Range::new($l as u32, $r as u32),
        )
    };
    ($source_id:expr, $i:expr) => {
        litcheck::diagnostics::SourceSpan::at($source_id, $i as u32)
    };
}

lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    grammar,
    "/expr/grammar.rs"
);

use std::fmt;

use logos::{Lexer, Logos};

use litcheck::{
    diagnostics::{Report, SourceId, SourceSpan, Span},
    StringInterner,
};

use crate::expr::{ExprError, Var};

pub type ParseError<'a> = lalrpop_util::ParseError<usize, Token<'a>, ExprError>;

pub struct NumericVarParser<'config> {
    interner: &'config mut StringInterner,
}
impl<'config> NumericVarParser<'config> {
    pub fn new(interner: &'config mut StringInterner) -> Self {
        Self { interner }
    }

    pub fn parse<'a>(&mut self, source: Span<&'a str>) -> Result<Var<'a>, Report> {
        let (span, source) = source.into_parts();
        let lexer = Token::lexer(source)
            .spanned()
            .map(|(t, span)| t.map(|t| (span.start, t, span.end)));
        grammar::NumericVarParser::new()
            .parse(span.source_id(), source, self.interner, lexer)
            .map_err(|err| handle_parse_error(span.source_id(), err))
            .map_err(|err| Report::from(err).with_source_code(source.to_string()))
    }
}

fn handle_parse_error(source_id: SourceId, err: ParseError) -> ExprError {
    match err {
        ParseError::InvalidToken { location: at } => ExprError::InvalidToken {
            span: SourceSpan::at(source_id, at as u32),
        },
        ParseError::UnrecognizedToken {
            token: (l, tok, r),
            expected,
        } => ExprError::UnrecognizedToken {
            span: SourceSpan::from_range_unchecked(source_id, l..r),
            token: tok.to_string(),
            expected,
        },
        ParseError::ExtraToken { token: (l, tok, r) } => ExprError::ExtraToken {
            span: SourceSpan::from_range_unchecked(source_id, l..r),
            token: tok.to_string(),
        },
        ParseError::UnrecognizedEof {
            location: at,
            expected,
        } => ExprError::UnrecognizedEof {
            span: SourceSpan::at(source_id, at as u32),
            expected,
        },
        ParseError::User { error } => error,
    }
}

#[derive(Debug, Clone, Logos)]
#[logos(error = ExprError)]
#[logos(extras = SourceId)]
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
            span: SourceSpan::try_from_range(lexer.extras, lexer.span()).unwrap(),
            error,
        })
}
