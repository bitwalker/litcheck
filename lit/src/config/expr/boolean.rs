#![expect(unused_assignments)]

use std::{borrow::Borrow, collections::BTreeSet, fmt, ops::Range, str::FromStr};

use regex::Regex;

use litcheck::diagnostics::{Diagnostic, SourceSpan, Span, Spanned};

use crate::config::FeatureSet;

/// This error is produced when attempting to parse a [BooleanExpr]
#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum InvalidBooleanExprError {
    #[error("unexpected token")]
    #[diagnostic(help("expected one of: {}", .expected.join(", ")))]
    UnexpectedToken {
        #[label("{token} is not valid here")]
        span: SourceSpan,
        token: String,
        expected: Vec<&'static str>,
    },
    #[error("unexpected character")]
    #[diagnostic()]
    UnexpectedChar {
        #[label("'{c}' is not valid here")]
        span: SourceSpan,
        c: char,
    },
    #[error("unexpected end of expression")]
    #[diagnostic(help("expected one of: {}", .expected.join(", ")))]
    UnexpectedEof {
        #[label("occurs here")]
        span: SourceSpan,
        expected: Vec<&'static str>,
    },
    #[error("invalid regex")]
    #[diagnostic()]
    InvalidRegex {
        #[label("{error}")]
        span: SourceSpan,
        #[source]
        error: regex::Error,
    },
}
impl InvalidBooleanExprError {
    pub fn with_span_offset(mut self, offset: usize) -> Self {
        match &mut self {
            Self::UnexpectedToken { ref mut span, .. }
            | Self::UnexpectedChar { ref mut span, .. }
            | Self::UnexpectedEof { ref mut span, .. }
            | Self::InvalidRegex { ref mut span, .. } => {
                let start = span.start() + offset;
                let end = span.len() + start;
                *span = SourceSpan::from(start..end);
                self
            }
        }
    }
}

/// This corresponds to the boolean expressions allowed in lit's `DEFINE` and `REDEFINE` commands
#[derive(Debug, Clone)]
pub enum BooleanExpr {
    /// A literal boolean value
    Lit(bool),
    /// A variable name.
    ///
    /// If the variable exists, this evaluates the value of that variable, otherwise false.
    Var(String),
    /// A pattern which, if it matches any variable which is true, evaluates to true, otherwise false.
    Pattern(Regex),
    /// The logical NOT operator
    Not(Box<BooleanExpr>),
    /// The logical AND operator
    And(Box<BooleanExpr>, Box<BooleanExpr>),
    /// The logical OR operator
    Or(Box<BooleanExpr>, Box<BooleanExpr>),
}
impl Default for BooleanExpr {
    #[inline(always)]
    fn default() -> Self {
        Self::Lit(false)
    }
}
impl FromStr for BooleanExpr {
    type Err = InvalidBooleanExprError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parser = BooleanExprParser::new(s);
        parser.parse()
    }
}
impl std::convert::AsRef<BooleanExpr> for BooleanExpr {
    fn as_ref(&self) -> &BooleanExpr {
        self
    }
}
impl Eq for BooleanExpr {}
impl PartialEq for BooleanExpr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Lit(x), Self::Lit(y)) => x == y,
            (Self::Var(x), Self::Var(y)) => x == y,
            (Self::Pattern(x), Self::Pattern(y)) => x.as_str() == y.as_str(),
            (Self::Not(x), Self::Not(y)) => x == y,
            (Self::And(xl, xr), Self::And(yl, yr)) => xl == yl && xr == yr,
            (Self::Or(xl, xr), Self::Or(yl, yr)) => xl == yl && xr == yr,
            (_, _) => false,
        }
    }
}
impl fmt::Display for BooleanExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lit(x) => write!(f, "{x}"),
            Self::Var(x) => write!(f, "{x}"),
            Self::Pattern(x) => write!(f, "{{{{{}}}}}", x.as_str()),
            Self::Not(ref x) => write!(f, "!{x}"),
            Self::And(ref x, ref y) => write!(f, "({x} && {y})"),
            Self::Or(ref x, ref y) => write!(f, "({x} || {y})"),
        }
    }
}
impl<'de> serde::Deserialize<'de> for BooleanExpr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Visitor;

        struct BooleanExprVisitor;
        impl<'de> Visitor<'de> for BooleanExprVisitor {
            type Value = BooleanExpr;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a valid boolean feature expression")
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                s.parse::<BooleanExpr>().map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(BooleanExprVisitor)
    }
}
impl BooleanExpr {
    /// Evaluate this boolean expression with `variables` as the context.
    ///
    /// If a variable is present in `variables`, it is considered to have an implicit
    /// value of true. As a result, if an identifier or pattern matches a variable in
    /// the set, that expression evaluates to true.
    pub fn evaluate<E: Environment>(&self, env: &E) -> bool {
        match self {
            Self::Lit(value) => *value,
            Self::Var(ref id) => env.is_defined(id.as_str()),
            Self::Pattern(ref pattern) => env.has_matching_definition(pattern),
            Self::Not(ref expr) => !expr.evaluate(env),
            Self::And(ref lhs, ref rhs) => lhs.evaluate(env) && rhs.evaluate(env),
            Self::Or(ref lhs, ref rhs) => lhs.evaluate(env) || rhs.evaluate(env),
        }
    }
}

pub trait Environment {
    fn is_defined(&self, name: &str) -> bool;
    fn has_matching_definition(&self, pattern: &Regex) -> bool;
}
impl<T: Borrow<str>> Environment for Vec<T> {
    #[inline]
    fn is_defined(&self, name: &str) -> bool {
        self.iter().any(|v| v.borrow() == name)
    }
    #[inline]
    fn has_matching_definition(&self, pattern: &Regex) -> bool {
        self.iter().any(|v| pattern.is_match(v.borrow()))
    }
}
impl<T: Borrow<str>> Environment for &[T] {
    #[inline]
    fn is_defined(&self, name: &str) -> bool {
        self.iter().any(|v| v.borrow() == name)
    }
    #[inline]
    fn has_matching_definition(&self, pattern: &Regex) -> bool {
        self.iter().any(|v| pattern.is_match(v.borrow()))
    }
}
impl Environment for FeatureSet {
    #[inline]
    fn is_defined(&self, var: &str) -> bool {
        self.contains(var)
    }
    #[inline]
    fn has_matching_definition(&self, pattern: &Regex) -> bool {
        self.iter().any(|v| pattern.is_match(v))
    }
}
impl<T: Borrow<str> + Ord> Environment for BTreeSet<T> {
    #[inline]
    fn is_defined(&self, var: &str) -> bool {
        self.contains(var)
    }
    #[inline]
    fn has_matching_definition(&self, pattern: &Regex) -> bool {
        self.iter().any(|v| pattern.is_match(v.borrow()))
    }
}

/// This struct parses a [BooleanExpr] from a given input string
struct BooleanExprParser<'a> {
    tokenizer: Tokenizer<'a>,
    current: Span<Token<'a>>,
    /// The current expression parsed thus far
    expr: BooleanExpr,
}
impl<'a> BooleanExprParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokenizer: Tokenizer::new(input),
            current: Span::new(SourceSpan::from(input.len()), Token::Eof),
            expr: BooleanExpr::default(),
        }
    }

    /// This corresponds to the following grammar:
    ///
    /// ```text,ignore
    /// expr        ::= or_expr
    /// or_expr     ::= and_expr ('||' and_expr)*
    /// and_expr    ::= not_expr ('&&' not_expr)*
    /// not_expr    ::= '!' not_expr
    ///               | '(' or_expr ')'
    ///               | match_expr
    /// match_expr  ::= pattern
    ///               | var
    /// var         ::= identifier
    /// pattern     ::= regex pattern
    ///               | identifier pattern
    ///               | regex
    ///               | identifier
    /// identifier  ::= [-+=._a-zA-Z0-9]+
    /// regex       ::= '{{' <any valid regular expression> '}}'
    /// ```
    pub fn parse(mut self) -> Result<BooleanExpr, InvalidBooleanExprError> {
        self.next()?;
        self.parse_or_expr()?;
        self.expect(Token::Eof)?;

        Ok(self.expr)
    }

    fn parse_or_expr(&mut self) -> Result<(), InvalidBooleanExprError> {
        self.parse_and_expr()?;
        while self.accept(Token::Or)? {
            let left = core::mem::take(&mut self.expr);
            self.parse_and_expr()?;
            let right = core::mem::take(&mut self.expr);
            self.expr = BooleanExpr::Or(Box::new(left), Box::new(right));
        }

        Ok(())
    }

    fn parse_and_expr(&mut self) -> Result<(), InvalidBooleanExprError> {
        self.parse_not_expr()?;
        while self.accept(Token::And)? {
            let left = core::mem::take(&mut self.expr);
            self.parse_not_expr()?;
            let right = core::mem::take(&mut self.expr);
            self.expr = BooleanExpr::And(Box::new(left), Box::new(right));
        }

        Ok(())
    }

    fn parse_not_expr(&mut self) -> Result<(), InvalidBooleanExprError> {
        match &*self.current {
            Token::Not => {
                self.next()?;
                self.parse_not_expr()?;
                let value = core::mem::take(&mut self.expr);
                self.expr = BooleanExpr::Not(Box::new(value));
                Ok(())
            }
            Token::Lparen => {
                self.next()?;
                self.parse_or_expr()?;
                self.expect(Token::Rparen)
            }
            Token::Ident(_) | Token::Pattern(_) => self.parse_match_expr(),
            Token::Eof => Err(InvalidBooleanExprError::UnexpectedEof {
                span: self.current.span(),
                expected: vec!["'!'", "'('", "'{{'", "identifier"],
            }),
            token => Err(InvalidBooleanExprError::UnexpectedToken {
                span: self.current.span(),
                token: token.to_string(),
                expected: vec!["'!'", "'('", "'{{'", "identifier"],
            }),
        }
    }

    fn parse_match_expr(&mut self) -> Result<(), InvalidBooleanExprError> {
        use smallvec::SmallVec;

        let start = self.current.start();
        let mut end = self.current.end();
        let mut is_ident = true;
        let mut reserve = 0;
        let mut parts = SmallVec::<[(SourceSpan, Token<'_>); 4]>::default();
        loop {
            let current_span = self.current.span();
            let current_end = self.current.end();
            match &*self.current {
                tok @ Token::Pattern(raw) => {
                    end = current_end;
                    is_ident = false;
                    reserve += raw.len() + 2;
                    parts.push((current_span, *tok));
                    self.next()?;
                }
                tok @ Token::Ident(raw) => {
                    end = current_end;
                    parts.push((current_span, *tok));
                    reserve += raw.len() + 8;
                    self.next()?;
                }
                _ => break,
            }
        }

        if parts.is_empty() {
            return if self.current == Token::Eof {
                Err(InvalidBooleanExprError::UnexpectedEof {
                    span: self.current.span(),
                    expected: vec!["pattern", "identifier"],
                })
            } else {
                Err(InvalidBooleanExprError::UnexpectedToken {
                    span: self.current.span(),
                    token: self.current.to_string(),
                    expected: vec!["pattern", "identifier"],
                })
            };
        }

        if is_ident {
            match parts.as_slice() {
                [(_, Token::Ident(name))] => {
                    self.expr = match *name {
                        "true" => BooleanExpr::Lit(true),
                        "false" => BooleanExpr::Lit(false),
                        name => BooleanExpr::Var(name.to_string()),
                    };
                }
                [_, (span, tok), ..] => {
                    return Err(InvalidBooleanExprError::UnexpectedToken {
                        span: *span,
                        token: tok.to_string(),
                        expected: vec!["end of expression"],
                    });
                }
                _ => panic!(
                    "expected ident expression to consist of a single part: {:?}",
                    parts
                ),
            }
        } else {
            let pattern =
                parts
                    .into_iter()
                    .fold(
                        String::with_capacity(reserve),
                        |mut acc, (_, token)| match token {
                            Token::Pattern(pat) => {
                                acc.push('(');
                                acc.push_str(pat);
                                acc.push(')');
                                acc
                            }
                            Token::Ident(raw) => {
                                regex_syntax::escape_into(raw, &mut acc);
                                acc
                            }
                            _ => unsafe { core::hint::unreachable_unchecked() },
                        },
                    );
            self.expr = Regex::new(&pattern)
                .map(BooleanExpr::Pattern)
                .map_err(|error| InvalidBooleanExprError::InvalidRegex {
                    span: SourceSpan::from(start..end),
                    error,
                })?;
        }

        Ok(())
    }

    fn accept(&mut self, expected: Token<'a>) -> Result<bool, InvalidBooleanExprError> {
        if self.current == expected {
            self.next().map(|_| true)
        } else {
            Ok(false)
        }
    }

    fn expect(&mut self, expected: Token<'a>) -> Result<(), InvalidBooleanExprError> {
        if self.current == expected {
            if self.current != Token::Eof {
                self.next()?;
            }
            Ok(())
        } else if self.current == Token::Eof {
            Err(InvalidBooleanExprError::UnexpectedEof {
                span: self.current.span(),
                expected: vec![expected.label()],
            })
        } else {
            Err(InvalidBooleanExprError::UnexpectedToken {
                span: self.current.span(),
                token: self.current.to_string(),
                expected: vec![expected.label()],
            })
        }
    }

    #[inline(always)]
    fn next(&mut self) -> Result<(), InvalidBooleanExprError> {
        let token = self.tokenizer.next().unwrap_or_else(|| {
            let span = SourceSpan::from(self.current.end());
            Ok(Span::new(span, Token::Eof))
        })?;
        self.current = token;
        Ok(())
    }
}

/// The token type produced by [Tokenizer]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Token<'a> {
    Ident(&'a str),
    Pattern(&'a str),
    And,
    Or,
    Not,
    Lparen,
    Rparen,
    Eof,
}
impl<'a> Token<'a> {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Ident(_) => "identifier",
            Self::Pattern(_) => "pattern",
            Self::And => "'&&'",
            Self::Or => "'||'",
            Self::Not => "'!'",
            Self::Lparen => "'('",
            Self::Rparen => "')'",
            Self::Eof => "end of expression",
        }
    }
}
impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "'{ident}'"),
            Self::Pattern(pattern) => write!(f, "'{{{{{pattern}}}}}'"),
            Self::And => f.write_str("'&&'"),
            Self::Or => f.write_str("'||'"),
            Self::Not => f.write_str("'!'"),
            Self::Lparen => f.write_str("'('"),
            Self::Rparen => f.write_str("')'"),
            Self::Eof => f.write_str("end of expression"),
        }
    }
}

/// The tokenizer for [BooleanExprParser]
struct Tokenizer<'a> {
    input: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    token_start: usize,
    token_end: usize,
    token: Token<'a>,
    error: Option<InvalidBooleanExprError>,
    eof: bool,
    current: char,
    pos: usize,
}

macro_rules! pop {
    ($tokenizer:ident, $tok:expr) => {{
        $tokenizer.pop();
        Ok($tok)
    }};
}

macro_rules! pop2 {
    ($tokenizer:ident, $tok:expr) => {{
        $tokenizer.pop();
        $tokenizer.pop();
        Ok($tok)
    }};
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars().peekable();
        let current = chars.next();
        let end = current.map(|c| c.len_utf8()).unwrap_or(0);
        let pos = 0;
        let current = current.unwrap_or('\0');
        let mut tokenizer = Self {
            input,
            chars,
            token_start: 0,
            token_end: end,
            token: Token::Eof,
            error: None,
            eof: false,
            current,
            pos,
        };
        tokenizer.advance();
        tokenizer
    }

    fn lex(&mut self) -> Option<Result<Span<Token<'a>>, InvalidBooleanExprError>> {
        if self.error.is_some() {
            return self.lex_error();
        }

        if self.eof && matches!(self.token, Token::Eof) {
            return None;
        }

        let token = core::mem::replace(&mut self.token, Token::Eof);
        let span = self.span();
        self.advance();

        Some(Ok(Span::new(span, token)))
    }

    #[cold]
    fn lex_error(&mut self) -> Option<Result<Span<Token<'a>>, InvalidBooleanExprError>> {
        self.eof = true;
        self.token = Token::Eof;
        self.error.take().map(Err)
    }

    fn advance(&mut self) {
        let (pos, c) = self.read();

        if c == '\0' {
            self.eof = true;
            return;
        }

        self.token_start = pos;
        match self.tokenize() {
            Ok(Token::Eof) => {
                self.token = Token::Eof;
                self.eof = true;
            }
            Ok(token) => {
                self.token = token;
            }
            Err(err) => {
                self.error = Some(err);
            }
        }
    }

    fn pop(&mut self) -> char {
        let c = self.current;
        self.pos += c.len_utf8();
        self.token_end = self.pos;
        match self.chars.next() {
            None => {
                self.eof = true;
                self.current = '\0';
                c
            }
            Some(next) => {
                self.current = next;
                c
            }
        }
    }

    fn drop(&mut self) {
        self.pos += self.current.len_utf8();
        self.token_end = self.pos;
        self.token_start = self.token_end;
        match self.chars.next() {
            None => {
                self.eof = true;
                self.current = '\0';
            }
            Some(next) => {
                self.current = next;
            }
        }
    }

    #[inline]
    fn skip(&mut self) {
        self.pop();
    }

    fn read(&self) -> (usize, char) {
        (self.pos, self.current)
    }

    fn peek(&mut self) -> char {
        self.chars.peek().copied().unwrap_or('\0')
    }

    fn span(&self) -> SourceSpan {
        SourceSpan::from(self.token_start..self.token_end)
    }

    fn slice(&self) -> &'a str {
        unsafe {
            core::str::from_utf8_unchecked(&self.input.as_bytes()[self.token_start..self.token_end])
        }
    }

    fn span_slice(&self, span: impl Into<Range<usize>>) -> &'a str {
        let span = span.into();
        unsafe { core::str::from_utf8_unchecked(&self.input.as_bytes()[span.start..span.end]) }
    }

    fn tokenize(&mut self) -> Result<Token<'a>, InvalidBooleanExprError> {
        self.drop_while(char::is_whitespace);

        let (pos, c) = self.read();
        match c {
            '(' => pop!(self, Token::Lparen),
            ')' => pop!(self, Token::Rparen),
            '&' => match self.peek() {
                '&' => pop2!(self, Token::And),
                '\0' => Err(InvalidBooleanExprError::UnexpectedEof {
                    span: SourceSpan::from(pos..self.token_end),
                    expected: vec!["'&'"],
                }),
                c => Err(InvalidBooleanExprError::UnexpectedChar {
                    span: SourceSpan::from(pos..(self.token_end + c.len_utf8())),
                    c,
                }),
            },
            '|' => match self.peek() {
                '|' => pop2!(self, Token::Or),
                '\0' => Err(InvalidBooleanExprError::UnexpectedEof {
                    span: SourceSpan::from(pos..self.token_end),
                    expected: vec!["'|'"],
                }),
                c => Err(InvalidBooleanExprError::UnexpectedChar {
                    span: SourceSpan::from(pos..(self.token_end + c.len_utf8())),
                    c,
                }),
            },
            '!' => pop!(self, Token::Not),
            '-' | '+' | '=' | '.' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => {
                self.skip_while(|c| matches!(c, '-' | '+' | '=' | '.' | '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'));
                Ok(Token::Ident(self.slice()))
            }
            '{' => {
                if self.peek() == '{' {
                    self.skip();
                    self.skip();
                }
                let start = self.pos;
                self.skip_while(|c| c != '}');
                let end = self.pos;
                let next = self.peek();
                match self.read() {
                    // Can only happen if there is nothing in the braces
                    (_, '}') if next == '}' => {
                        self.pop();
                        self.pop();
                        let pattern = self.span_slice(start..end);
                        if pattern.is_empty() {
                            Err(InvalidBooleanExprError::UnexpectedToken {
                                span: SourceSpan::from(start..end),
                                token: "'}}'".to_string(),
                                expected: vec!["pattern"],
                            })
                        } else {
                            Ok(Token::Pattern(pattern))
                        }
                    }
                    (_, '}') if next != '\0' => Err(InvalidBooleanExprError::UnexpectedChar {
                        span: SourceSpan::from(end),
                        c: next,
                    }),
                    (_, _) => Err(InvalidBooleanExprError::UnexpectedEof {
                        span: SourceSpan::from(end),
                        expected: vec!["'}'"],
                    }),
                }
            }
            '\0' => Ok(Token::Eof),
            c => Err(InvalidBooleanExprError::UnexpectedChar {
                span: SourceSpan::from(pos),
                c,
            }),
        }
    }

    #[inline]
    fn drop_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.read() {
                (_, '\0') => break,
                (_, c) if predicate(c) => self.drop(),
                _ => break,
            }
        }
    }

    #[inline]
    fn skip_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.read() {
                (_, '\0') => break,
                (_, c) if predicate(c) => self.skip(),
                _ => break,
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Span<Token<'a>>, InvalidBooleanExprError>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use super::*;
    use litcheck::diagnostics::Report;

    use pretty_assertions::assert_eq;

    macro_rules! parse {
        ($input:literal, $expected:expr) => {
            match $input.parse::<BooleanExpr>() {
                Ok(expr) => {
                    assert_eq!(expr, $expected);
                    expr
                }
                Err(err) => panic!("failed to parse boolean expression: {err}"),
            }
        };
    }

    macro_rules! assert_parse_error {
        ($input:literal) => {
            match $input
                .parse::<BooleanExpr>()
                .map_err(|err| Report::new(err).with_source_code($input))
            {
                Err(err) => {
                    let mut msg = format!("{}", &err);
                    if let Some(labels) = err.labels() {
                        let mut n = 0;
                        for label in labels {
                            if let Some(label) = label.label() {
                                if n > 0 {
                                    msg.push_str(", ");
                                } else {
                                    msg.push_str(": ");
                                }
                                msg.push_str(label);
                                n += 1;
                            }
                        }
                    }
                    if let Some(help) = err.help() {
                        write!(&mut msg, "\nhelp: {help}").unwrap();
                    }
                    panic!("{msg}");
                }
                _ => (),
            }
        };
    }

    macro_rules! assert_eval_true {
        ($expr:ident, $vars:ident) => {{
            assert_eval(&$expr, &$vars, true);
        }};

        ($expr:ident, $vars:ident, $($var:literal),+) => {
            $vars.clear();
            $vars.extend([$($var),*]);
            assert_eval(&$expr, &$vars, true);
        }
    }

    macro_rules! assert_eval_false {
        ($expr:ident, $vars:ident) => {{
            assert_eval(&$expr, &$vars, false);
        }};

        ($expr:ident, $vars:ident, $($var:literal),+) => {{
            $vars.clear();
            $vars.extend([$($var),*]);
            assert_eval(&$expr, &$vars, false);
        }}
    }

    macro_rules! and {
        ($lhs:expr, $rhs:expr) => {
            BooleanExpr::And(Box::new($lhs), Box::new($rhs))
        };
    }

    macro_rules! or {
        ($lhs:expr, $rhs:expr) => {
            BooleanExpr::Or(Box::new($lhs), Box::new($rhs))
        };
    }

    macro_rules! not {
        ($lhs:expr) => {
            BooleanExpr::Not(Box::new($lhs))
        };
    }

    macro_rules! var {
        ($name:literal) => {
            BooleanExpr::Var($name.to_string())
        };
    }

    macro_rules! pattern {
        ($regex:literal) => {
            BooleanExpr::Pattern(Regex::new($regex).unwrap())
        };
    }

    #[test]
    fn test_script_boolean_expr_variables() {
        let vars = BTreeSet::from([
            "its-true",
            "false-lol-true",
            "under_score",
            "e=quals",
            "d1g1ts",
        ]);

        let expr = parse!("true", BooleanExpr::Lit(true));
        assert_eval_true!(expr, vars);
        let expr = parse!("false", BooleanExpr::Lit(false));
        assert_eval_false!(expr, vars);
        let expr = parse!("its-true", var!("its-true"));
        assert_eval_true!(expr, vars);
        let expr = parse!("under_score", var!("under_score"));
        assert_eval_true!(expr, vars);
        let expr = parse!("e=quals", var!("e=quals"));
        assert_eval_true!(expr, vars);
        let expr = parse!("d1g1ts", var!("d1g1ts"));
        assert_eval_true!(expr, vars);
        let expr = parse!("{{its.+}}", pattern!("(its.+)"));
        assert_eval_true!(expr, vars);
        let expr = parse!("{{false-[lo]+-true}}", pattern!("(false-[lo]+-true)"));
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "{{(true|false)-lol-(true|false)}}",
            pattern!("((true|false)-lol-(true|false))")
        );
        assert_eval_true!(expr, vars);
        let expr = parse!("d1g{{[0-9]}}ts", pattern!("d1g([0-9])ts"));
        assert_eval_true!(expr, vars);
        let expr = parse!("d1g{{[0-9]}}t{{[a-z]}}", pattern!("d1g([0-9])t([a-z])"));
        assert_eval_true!(expr, vars);
        let expr = parse!("d1{{(g|1)+}}ts", pattern!("d1((g|1)+)ts"));
        assert_eval_true!(expr, vars);
    }

    #[test]
    fn test_script_boolean_expr_operators_or() {
        let vars = BTreeSet::default();

        let expr = parse!(
            "true || true",
            or!(BooleanExpr::Lit(true), BooleanExpr::Lit(true))
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "true || false",
            or!(BooleanExpr::Lit(true), BooleanExpr::Lit(false))
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "false || true",
            or!(BooleanExpr::Lit(false), BooleanExpr::Lit(true))
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "false || false",
            or!(BooleanExpr::Lit(false), BooleanExpr::Lit(false))
        );
        assert_eval_false!(expr, vars);
    }

    #[test]
    fn test_script_boolean_expr_operators_and() {
        let vars = BTreeSet::default();

        let expr = parse!(
            "true && true",
            and!(BooleanExpr::Lit(true), BooleanExpr::Lit(true))
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "true && false",
            and!(BooleanExpr::Lit(true), BooleanExpr::Lit(false))
        );
        assert_eval_false!(expr, vars);
        let expr = parse!(
            "false && true",
            and!(BooleanExpr::Lit(false), BooleanExpr::Lit(true))
        );
        assert_eval_false!(expr, vars);
        let expr = parse!(
            "false && false",
            and!(BooleanExpr::Lit(false), BooleanExpr::Lit(false))
        );
        assert_eval_false!(expr, vars);
    }

    #[test]
    fn test_script_boolean_expr_operators_not() {
        let vars = BTreeSet::default();

        let expr = parse!("!true", not!(BooleanExpr::Lit(true)));
        assert_eval_false!(expr, vars);
        let expr = parse!("!false", not!(BooleanExpr::Lit(false)));
        assert_eval_true!(expr, vars);
        let expr = parse!("!!false", not!(not!(BooleanExpr::Lit(false))));
        assert_eval_false!(expr, vars);
    }

    #[test]
    fn test_script_boolean_expr_operators_mixed() {
        let vars = BTreeSet::default();

        let expr = parse!("    ((!((false) ))    )  ", not!(BooleanExpr::Lit(false)));
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "true && (true && (true))",
            and!(
                BooleanExpr::Lit(true),
                and!(BooleanExpr::Lit(true), BooleanExpr::Lit(true))
            )
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "!false && !false && !! !false",
            and!(
                and!(not!(BooleanExpr::Lit(false)), not!(BooleanExpr::Lit(false))),
                not!(not!(not!(BooleanExpr::Lit(false))))
            )
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "false && false || true",
            or!(
                and!(BooleanExpr::Lit(false), BooleanExpr::Lit(false)),
                BooleanExpr::Lit(true)
            )
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "(false && false) || true",
            or!(
                and!(BooleanExpr::Lit(false), BooleanExpr::Lit(false)),
                BooleanExpr::Lit(true)
            )
        );
        assert_eval_true!(expr, vars);
        let expr = parse!(
            "false && (false || true)",
            and!(
                BooleanExpr::Lit(false),
                or!(BooleanExpr::Lit(false), BooleanExpr::Lit(true))
            )
        );
        assert_eval_false!(expr, vars);
    }

    #[test]
    fn test_script_boolean_expr_evaluate() {
        let mut vars = BTreeSet::from(["linux", "target=x86_64-unknown-linux-gnu"]);

        let expr = parse!(
            "linux && (target={{aarch64-.+}} || target={{x86_64-.+}})",
            and!(
                var!("linux"),
                or!(
                    pattern!("target=(aarch64-.+)"),
                    pattern!("target=(x86_64-.+)")
                )
            )
        );
        assert!(expr.evaluate(&vars));
        vars.clear();
        vars.extend(["linux", "target=i386-unknown-linux-gnu"]);
        assert!(!expr.evaluate(&vars));

        let expr = parse!(
            "use_system_cxx_lib && target={{.+}}-apple-macosx10.{{9|10|11|12}} && !no-exceptions",
            and!(
                and!(
                    var!("use_system_cxx_lib"),
                    pattern!("target=(.+)\\-apple\\-macosx10\\.(9|10|11|12)")
                ),
                not!(var!("no-exceptions"))
            )
        );
        vars.clear();
        vars.extend(["use_system_cxx_lib", "target=arm64-apple-macosx10.12"]);
        assert!(expr.evaluate(&vars));
        vars.insert("no-exceptions");
        assert!(!expr.evaluate(&vars));
        vars.clear();
        vars.extend(["use_system_cxx_lib", "target=arm64-apple-macosx10.15"]);
        assert!(!expr.evaluate(&vars));
    }

    #[test]
    #[should_panic(expected = "unexpected character: '#' is not valid here")]
    fn test_script_boolean_expr_invalid_ident() {
        assert_parse_error!("ba#d");
    }

    #[test]
    #[should_panic(
        expected = "unexpected token: 'and' is not valid here\nhelp: expected one of: end of expression"
    )]
    fn test_script_boolean_expr_invalid_operator() {
        assert_parse_error!("true and true");
    }

    #[test]
    #[should_panic(
        expected = "unexpected token: '||' is not valid here\nhelp: expected one of: '!', '(', '{{', identifier"
    )]
    fn test_script_boolean_expr_missing_lhs_operand() {
        assert_parse_error!("|| true");
    }

    #[test]
    #[should_panic(
        expected = "unexpected end of expression: occurs here\nhelp: expected one of: '!', '(', '{{', identifier"
    )]
    fn test_script_boolean_expr_missing_rhs_operand() {
        assert_parse_error!("true &&");
    }

    #[test]
    #[should_panic(
        expected = "unexpected end of expression: occurs here\nhelp: expected one of: '!', '(', '{{', identifier"
    )]
    fn test_script_boolean_expr_empty_input() {
        assert_parse_error!("");
    }

    #[test]
    #[should_panic(
        expected = "unexpected end of expression: occurs here\nhelp: expected one of: ')'"
    )]
    fn test_script_boolean_expr_unclosed_group() {
        assert_parse_error!("(((true && true) || true)");
    }

    #[test]
    #[should_panic(
        expected = "unexpected token: '(' is not valid here\nhelp: expected one of: end of expression"
    )]
    fn test_script_boolean_expr_ident_followed_by_group() {
        assert_parse_error!("true (true)");
    }

    #[test]
    #[should_panic(
        expected = "unexpected token: ')' is not valid here\nhelp: expected one of: '!', '(', '{{', identifier"
    )]
    fn test_script_boolean_expr_empty_group() {
        assert_parse_error!("(  )");
    }

    #[test]
    #[should_panic(
        expected = "unexpected end of expression: occurs here\nhelp: expected one of: '}'"
    )]
    fn test_script_boolean_expr_unclosed_pattern() {
        assert_parse_error!("abc{def");
    }

    #[test]
    #[should_panic(
        expected = "unexpected end of expression: occurs here\nhelp: expected one of: '}'"
    )]
    fn test_script_boolean_expr_empty_pattern() {
        assert_parse_error!("{}");
    }

    #[track_caller]
    #[inline(never)]
    fn assert_eval(expr: &BooleanExpr, vars: &BTreeSet<&'static str>, expected_result: bool) {
        let result = expr.evaluate(vars);
        assert_eq!(
            result, expected_result,
            "expected {expr} to evaluate to {expected_result} with environment: {vars:?}"
        );
    }
}
