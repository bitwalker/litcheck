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
    "/parse/grammar.rs"
);

use litcheck::{
    diagnostics::{SourceFile, SourceSpan, Span, Spanned},
    StringInterner,
};
use std::borrow::Cow;

use super::{Lexed, Lexer, ParseError, ParseResult, ParserError, Token};
use crate::{ast::*, Config};

macro_rules! lex {
    ($lexer:ident) => {
        match $lexer.next().transpose()? {
            Some((_, Token::Error(error), _)) => return Err(ParserError::from(error)),
            lexed => lexed,
        }
    };
}

macro_rules! expected_token {
    ($expected_ty:ident :: $expected_variant:ident ($expected_pat:pat)) => {
        $expected_ty::$expected_variant(Default::default()).to_string()
    };

    ($expected_ty:ident :: $expected_variant:ident) => {
        $expected_ty::$expected_variant.to_string()
    };
}

macro_rules! expect {
    ($lexer:ident, $($expected_ty:ident :: $expected_variant:ident $(($expected_pat:pat))?)|+) => {
        match lex!($lexer) {
            $(
                Some(lexed @ (_, $expected_ty :: $expected_variant $(($expected_pat))?, _)) => lexed,
            )*
            Some((start, token, end)) => {
                return Err(ParserError::UnrecognizedToken {
                    span: SourceSpan::from(start..end),
                    token: token.to_string(),
                    expected: vec![$(expected_token!($expected_ty :: $expected_variant $(($expected_pat))?)),*],
                });
            }
            None => {
                return Err(ParserError::UnrecognizedEof {
                    span: $lexer.current_offset(),
                    expected: vec![$(expected_token!($expected_ty :: $expected_variant $(($expected_pat))?)),*],
                });
            }
        }
    };
}

macro_rules! expect_literal {
    ($lexer:ident) => {
        match lex!($lexer) {
            Some((start, Token::Raw(raw), end)) => Span::new(SourceSpan::from(start..end), raw),
            Some((start, token, end)) => {
                return Err(ParserError::UnrecognizedToken {
                    span: SourceSpan::from(start..end),
                    token: token.to_string(),
                    expected: vec!["literal".to_string()],
                });
            }
            None => {
                return Err(ParserError::UnrecognizedEof {
                    span: $lexer.current_offset(),
                    expected: vec!["literal".to_string()],
                });
            }
        }
    };
}

macro_rules! expect_ignore {
    ($lexer:ident, $($expected_ty:ident :: $expected_variant:ident $(($expected_pat:pat))?)|+) => {
        match lex!($lexer) {
            Some((start, token, end)) => {
                if !matches!(token, $($expected_ty :: $expected_variant $(($expected_pat))?)|*) {
                    return Err(ParserError::UnrecognizedToken {
                        span: SourceSpan::from(start..end),
                        token: token.to_string(),
                        expected: vec![$(expected_token!($expected_ty :: $expected_variant $(($expected_pat))?)),*],
                    });
                }
            }
            None => {
                return Err(ParserError::UnrecognizedEof {
                    span: $lexer.current_offset(),
                    expected: vec![$(expected_token!($expected_ty :: $expected_variant $(($expected_pat))?)),*],
                });
            }
        }
    }
}

pub struct CheckFileParser<'config> {
    config: &'config Config,
    pub interner: &'config mut StringInterner,
}
impl<'config> CheckFileParser<'config> {
    pub fn new(config: &'config Config, interner: &'config mut StringInterner) -> Self {
        Self { config, interner }
    }

    pub fn parse<'a, S>(&mut self, code: &'a S) -> ParseResult<CheckFile<'a>>
    where
        S: SourceFile + ?Sized + 'a,
    {
        let source = code.source();
        let mut lexer = Lexer::<'a>::new(
            code,
            &self.config.check_prefixes,
            &self.config.comment_prefixes,
        );
        let mut comment = vec![];
        let mut lines = vec![];
        while let Some(lexed) = lexer.next() {
            let (start, token, end) = lexed?;
            match token {
                Token::Comment(s) => {
                    comment.push(s);
                    continue;
                }
                Token::Check(ty) => {
                    let mut line =
                        self.parse_check(Span::new(start..end, ty), source, &mut lexer)?;
                    line.comment.append(&mut comment);
                    lines.push(line);
                }
                Token::Lf => continue,
                Token::Error(err) => return Err(ParserError::from(err)),
                token => {
                    return Err(ParserError::ExtraToken {
                        span: SourceSpan::from(start..end),
                        token: token.to_string(),
                    })
                }
            }
        }

        let unused_prefixes = lexer.unused_prefixes();
        if !unused_prefixes.is_empty() && !self.config.allow_unused_prefixes {
            return Err(ParserError::UnusedCheckPrefixes(unused_prefixes));
        }

        Ok(CheckFile::new(lines))
    }

    fn parse_check<'a>(
        &mut self,
        ty: Span<Check>,
        source: &'a str,
        lexer: &mut Lexer<'a>,
    ) -> ParseResult<CheckLine<'a>> {
        let line_start = ty.start();
        let check_end = ty.end();

        // Modifiers (optional)
        let mut modifiers = CheckModifier::default();
        let mut modifier_start = ty.end();
        let mut modifier_end = modifier_start;
        while let (start, Token::Modifier(modifier), end) =
            expect!(lexer, Token::Modifier(_) | Token::Colon)
        {
            modifier_start = start;
            modifier_end = end;
            modifiers |= modifier;
        }
        let modifiers = Span::new(modifier_start..modifier_end, modifiers);

        // CheckType
        let ty_span = ty.range();
        let ty = ty.into_inner();
        let ty = CheckType::new(ty_span.into(), ty).with_modifiers(modifiers);

        // CheckPattern
        if modifiers.contains(CheckModifier::LITERAL) {
            match expect!(lexer, Token::Raw(_) | Token::Lf) {
                (start, Token::Raw(pattern), end) => {
                    let pattern = Span::new(start..end, pattern);
                    Ok(CheckLine::new(
                        SourceSpan::from(line_start..end),
                        ty,
                        CheckPattern::Literal(pattern.map(Cow::Borrowed)),
                    ))
                }
                (_, _, end) if matches!(ty.kind, Check::Empty) => Ok(CheckLine::new(
                    SourceSpan::from(line_start..end),
                    ty,
                    CheckPattern::Empty(SourceSpan::from(line_start..end)),
                )),
                (_, _, end) => {
                    // Expected a non-empty pattern
                    Err(ParserError::EmptyPattern {
                        span: SourceSpan::from(line_start..end),
                    })
                }
            }
        } else {
            let mut parts = vec![];
            loop {
                match lexer.peek() {
                    Some(Token::MatchStart) => {
                        let part = self.parse_match_block(source, lexer)?;
                        parts.push(part);
                    }
                    Some(Token::RegexStart) => {
                        expect_ignore!(lexer, Token::RegexStart);
                        let pattern = expect_literal!(lexer);
                        parts.push(CheckPatternPart::Regex(RegexPattern::new(
                            pattern.map(Cow::Borrowed),
                        )));
                        expect_ignore!(lexer, Token::RegexEnd);
                    }
                    Some(Token::Raw(_)) => {
                        let pattern = expect_literal!(lexer);
                        parts.push(CheckPatternPart::Literal(pattern.map(Cow::Borrowed)));
                    }
                    Some(Token::Lf) => {
                        expect_ignore!(lexer, Token::Lf);
                        break;
                    }
                    Some(_) => {
                        let (start, token, end) = lex!(lexer).unwrap();
                        return Err(ParserError::UnrecognizedToken {
                            span: SourceSpan::from(start..end),
                            token: token.to_string(),
                            expected: vec![
                                "literal".to_string(),
                                "[[".to_string(),
                                "{{".to_string(),
                                Token::Lf.to_string(),
                            ],
                        });
                    }
                    None => {
                        break;
                    }
                }
            }

            match parts.len() {
                0 if matches!(ty.kind, Check::Empty) => Ok(CheckLine::new(
                    SourceSpan::from(line_start..check_end),
                    ty,
                    CheckPattern::Empty(SourceSpan::from(line_start..check_end)),
                )),
                0 => Err(ParserError::EmptyPattern {
                    span: SourceSpan::from(line_start..check_end),
                }),
                1 => {
                    let pattern = match parts.pop().unwrap() {
                        CheckPatternPart::Literal(literal) => CheckPattern::Literal(literal),
                        CheckPatternPart::Regex(regex) => CheckPattern::Regex(regex),
                        part @ CheckPatternPart::Match(_) => {
                            let span = part.span();
                            parts.push(part);
                            CheckPattern::Match(Span::new(span, parts))
                        }
                    };
                    Ok(CheckLine::new(
                        SourceSpan::from(line_start..pattern.end()),
                        ty,
                        pattern,
                    ))
                }
                _ => {
                    let start = parts.first().unwrap().start();
                    let end = parts.last().unwrap().end();
                    Ok(CheckLine::new(
                        SourceSpan::from(line_start..end),
                        ty,
                        CheckPattern::Match(Span::new(start..end, parts)),
                    ))
                }
            }
        }
    }

    fn parse_match_block<'a>(
        &mut self,
        source: &'a str,
        lexer: &mut Lexer<'a>,
    ) -> ParseResult<CheckPatternPart<'a>> {
        let mut tokens: Vec<Lexed<'a>> = vec![];
        tokens.push(Ok(expect!(lexer, Token::MatchStart)));

        loop {
            if matches!(lexer.peek(), Some(Token::MatchEnd)) {
                break;
            }
            match lex!(lexer) {
                Some((start, Token::Lf, end)) => {
                    return Err(ParserError::UnrecognizedToken {
                        span: SourceSpan::from(start..end),
                        token: Token::Lf.to_string(),
                        expected: vec!["]]".to_string()],
                    });
                }
                Some(token) => {
                    tokens.push(Ok(token));
                }
                None => {
                    return Err(ParserError::UnrecognizedEof {
                        span: lexer.current_offset(),
                        expected: vec!["]]".to_string()],
                    });
                }
            }
        }

        let (match_end_start, match_end, match_end_end) = expect!(lexer, Token::MatchEnd);

        if tokens.len() == 1 {
            return Err(ParserError::UnrecognizedToken {
                span: SourceSpan::from(match_end_start..match_end_end),
                token: match_end.to_string(),
                expected: vec!["a non-empty match expression".to_string()],
            });
        }

        tokens.push(Ok((match_end_start, match_end, match_end_end)));

        Ok(CheckPatternPart::Match(self.parse_match(source, tokens)?))
    }

    fn parse_match<'a>(
        &mut self,
        source: &'a str,
        tokens: Vec<Lexed<'a>>,
    ) -> ParseResult<Match<'a>> {
        let lexer = tokens.into_iter();
        grammar::MatchParser::new()
            .parse(source, self.interner, lexer)
            .map_err(handle_parse_error)
    }
}

fn handle_parse_error(err: ParseError) -> ParserError {
    match err {
        ParseError::InvalidToken { location: at } => ParserError::InvalidToken {
            span: SourceSpan::from(at..at),
        },
        ParseError::UnrecognizedToken {
            token: (l, tok, r),
            expected,
        } => ParserError::UnrecognizedToken {
            span: SourceSpan::from(l..r),
            token: tok.to_string(),
            expected,
        },
        ParseError::ExtraToken { token: (l, tok, r) } => ParserError::ExtraToken {
            span: SourceSpan::from(l..r),
            token: tok.to_string(),
        },
        ParseError::UnrecognizedEof {
            location: at,
            expected,
        } => ParserError::UnrecognizedEof {
            span: SourceSpan::from(at..at),
            expected,
        },
        ParseError::User { error } => error,
    }
}
