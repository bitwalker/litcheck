mod delimiter;
mod error;
mod patterns;
mod token;

use std::collections::VecDeque;

use crate::{
    ast::{self, CheckModifier},
    common::*,
    parse::ParserError,
    pattern::search::{AhoCorasickSearcher, RegexSearcher},
};

pub use self::error::LexerError;
pub use self::token::Token;

/// The value produced by the [Lexer] when iterated
pub type Lexed<'input> = Result<(usize, Token<'input>, usize), ParserError>;

use self::delimiter::Delimiter;
use self::patterns::{Check, Pattern};

/// The lexer that is used to tokenize a check file
pub struct Lexer<'input> {
    input: Input<'input>,
    patterns: Vec<Pattern>,
    regex: Regex,
    searcher: RegexSearcher<'input>,
    cache: regex_automata::meta::Cache,
    captures: regex_automata::util::captures::Captures,
    delimiter_patterns: aho_corasick::AhoCorasick,
    delimiter_searcher: AhoCorasickSearcher<'input>,
    /// When we have reached true Eof, this gets set to true, and the only token
    /// produced after that point is Token::Eof, or None, depending on how you are
    /// consuming the lexer
    eof: bool,
    /// When we first start parsing, this is true until we've observed a token
    /// which is not Lf or Eof, so that we trim all leading empty lines which
    /// will just be ignored anyway. This simplifies parsing by ensuring the
    /// rules only need to account for actual content lines in the file.
    leading_lf: bool,
    /// The token buffer, used when tokenizing lines
    buffer: VecDeque<Lexed<'input>>,
}
impl<'input> Lexer<'input> {
    /// Produces an instance of the lexer with the lexical analysis to be performed on the `input`
    /// string. Note that no lexical analysis occurs until the lexer has been iterated over.
    pub fn new<S>(
        source: &'input S,
        check_prefixes: &[Box<str>],
        comment_prefixes: &[Box<str>],
    ) -> Self
    where
        S: SourceFile + ?Sized + 'input,
    {
        let buffer = source.source().as_bytes();
        let input = Input::new(buffer, false);
        let mut patterns = Pattern::generate_check_patterns(check_prefixes).collect::<Vec<_>>();
        patterns.extend(Pattern::generate_comment_patterns(comment_prefixes));
        let regex =
            Regex::new_many(&patterns).expect("expected valid prefix searcher configuration");
        let searcher = RegexSearcher::new(input.into());
        let eof = input.is_empty();
        let captures = regex.create_captures();
        let cache = regex.create_cache();

        let mut builder = aho_corasick::AhoCorasickBuilder::new();
        builder
            .match_kind(aho_corasick::MatchKind::LeftmostLongest)
            .start_kind(aho_corasick::StartKind::Both)
            .kind(Some(aho_corasick::AhoCorasickKind::DFA));
        let delimiter_patterns = builder
            .build(Delimiter::ALL)
            .expect("expected delimiter searcher configuration");
        let delimiter_searcher = AhoCorasickSearcher::new(input.into());

        Lexer {
            input,
            patterns,
            regex,
            searcher,
            cache,
            captures,
            delimiter_patterns,
            delimiter_searcher,
            eof,
            leading_lf: true,
            buffer: VecDeque::with_capacity(128),
        }
    }

    pub fn current_offset(&self) -> SourceSpan {
        let at = self.input.start();
        SourceSpan::from(at..at)
    }

    pub fn peek(&mut self) -> Option<&Token<'input>> {
        loop {
            if !self.buffer.is_empty() {
                break self
                    .buffer
                    .front()
                    .and_then(|lexed| lexed.as_ref().ok().map(|(_, t, _)| t));
            } else if self.eof {
                break None;
            }

            self.tokenize();
        }
    }

    pub fn lex(&mut self) -> Option<Lexed<'input>> {
        loop {
            if !self.buffer.is_empty() {
                break self.buffer.pop_front();
            } else if self.eof {
                break None;
            }

            self.tokenize();
        }
    }

    fn tokenize(&mut self) {
        match self.input.peek_byte() {
            (_, b'\0') => {
                self.eof = true;
                return;
            }
            (offset, b'\n') => {
                let next_offset = offset + 1;
                self.input.set_start(next_offset);
                self.buffer.push_back(Ok((offset, Token::Lf, next_offset)));
            }
            _ => (),
        }

        let bytes = self.input.buffer();
        let start = self.input.start();
        let eof = self.input.end();
        let mut word_boundary = self.input.start();
        while word_boundary < eof && bytes[word_boundary].is_ascii_whitespace() {
            word_boundary += 1;
        }
        if start < word_boundary {
            self.input.set_start(word_boundary);
        }

        // Force the searcher forward if necessary to ensure it is caught up to our view of the input
        let start = self.input.start();
        if self.searcher.input().start() < start {
            self.searcher.set_last_match_end(start);
        }

        let search_result = self.searcher.advance(|input| {
            self.regex
                .search_captures_with(&mut self.cache, input, &mut self.captures);
            Ok(self.captures.get_match())
        });
        if let Some(matched) = search_result {
            let pid = matched.pattern();
            let range = Range::from(matched.range());
            let pattern = &self.patterns[pid.as_usize()];
            let pattern_ty = pattern.ty;
            match pattern_ty {
                Check::Comment => return self.tokenize_comment(range),
                Check::Count => {
                    let valid = self.tokenize_check_count_prefix(range);
                    if !valid {
                        self.captures.set_pattern(None);
                        return;
                    }
                }
                ty => {
                    self.buffer.push_back(Ok((
                        range.start,
                        Token::Check(ty.try_into().unwrap()),
                        range.end,
                    )));
                }
            }
            let literal = self.tokenize_optional_modifiers();
            self.buffer
                .push_back(Ok((range.end - 1, Token::Colon, range.end)));
            self.input.set_start(range.end);
            self.tokenize_check_pattern(literal);
            self.captures.set_pattern(None);
        } else {
            // There are no more check lines in the input
            self.input.set_start(self.input.end());
            self.eof = true;
        }
    }

    fn tokenize_optional_modifiers(&mut self) -> bool {
        if let Some(span) = self.captures.get_group_by_name("modifiers") {
            if self.input.buffer()[span.start..].starts_with(b"LITERAL") {
                self.buffer.push_back(Ok((
                    span.start,
                    Token::Modifier(CheckModifier::LITERAL),
                    span.end,
                )));
                true
            } else {
                unreachable!("no other modifiers are recognized by the regex pattern")
            }
        } else {
            false
        }
    }

    fn tokenize_comment(&mut self, range: Range<usize>) {
        // Find the next newline, and take all the input up to that point
        let span = self.captures.get_group_by_name("comment").unwrap();
        let comment = self.input.as_str(span.start..span.end);
        let comment = comment.strip_prefix(' ').unwrap_or(comment);
        self.buffer.push_back(Ok((
            range.start,
            Token::Comment(Cow::Borrowed(comment)),
            span.end,
        )));
        self.input.set_start(span.end);
        self.captures.set_pattern(None);
    }

    fn tokenize_check_count_prefix(&mut self, prefix_range: Range<usize>) -> bool {
        let count_span = self.captures.get_group_by_name("count").unwrap();
        let count = self.input.as_str(count_span.start..count_span.end);
        match count.parse::<u8>() {
            Ok(count) => {
                self.buffer.push_back(Ok((
                    prefix_range.start,
                    Token::Check(ast::Check::Plain),
                    count_span.end,
                )));
                self.buffer.push_back(Ok((
                    prefix_range.start,
                    Token::Modifier(CheckModifier::from_count(count)),
                    count_span.end,
                )));
                true
            }
            Err(error) => {
                let token = Token::Error(LexerError::BadCount {
                    span: SourceSpan::from(count_span.start..count_span.end),
                    error,
                });
                self.buffer
                    .push_back(Ok((count_span.start, token, count_span.end)));
                // Skip to end of line
                let eol = self
                    .input
                    .next_newline_from(prefix_range.end)
                    .unwrap_or_else(|| self.input.end());
                self.input.set_start(eol);
                false
            }
        }
    }

    /// We've succesfully parsed a CHECK directive.
    ///
    /// We're parsing the rest of the line as a CHECK pattern, so we need to look
    /// for `[[` `]]` and `{{` `}}` markers indicating substitution/capture and regex
    /// matches respectively.
    fn tokenize_check_pattern(&mut self, literal: bool) {
        let start = self.input.start();
        let eol = self.input.next_newline().unwrap_or(self.input.end());

        if literal {
            let raw = self.input.as_str(start..eol);
            if let Some(raw) = raw.strip_prefix(' ') {
                self.buffer.push_back(Ok((start + 1, Token::Raw(raw), eol)));
            } else {
                self.buffer.push_back(Ok((start, Token::Raw(raw), eol)));
            }
            self.input.set_start(eol);
            return;
        }

        let mut in_match: Option<Span<Delimiter>> = None;
        let mut in_regex: Option<Span<Delimiter>> = None;

        let mut last_delimiter_end = start;
        self.delimiter_searcher.set_range(start..eol);
        let mut is_first = true;
        while let Some(matched) = self
            .delimiter_searcher
            .advance(|input| Ok(self.delimiter_patterns.find(input.clone())))
        {
            let pid = matched.pattern();
            let delim_range = Range::from(matched.range());
            match Delimiter::from_pid(pid.as_usize()) {
                delim @ (Delimiter::MatchStart | Delimiter::NumericMatchStart)
                    if in_match.is_none() && in_regex.is_none() =>
                {
                    in_match = Some(Span::new(delim_range, delim));
                    if delim_range.start > last_delimiter_end {
                        let raw = &self.input.buffer()[last_delimiter_end..delim_range.start];
                        if !raw.iter().all(u8::is_ascii_whitespace) {
                            let content = self.input.as_str(last_delimiter_end..delim_range.start);
                            let content = if is_first {
                                content.strip_prefix(' ').unwrap_or(content)
                            } else {
                                content
                            };
                            self.buffer.push_back(Ok((
                                last_delimiter_end,
                                Token::Raw(content),
                                delim_range.start,
                            )));
                        }
                    }
                    if matches!(delim, Delimiter::NumericMatchStart) {
                        self.buffer.push_back(Ok((
                            delim_range.start,
                            Token::MatchStart,
                            delim_range.end - 1,
                        )));
                        self.buffer.push_back(Ok((
                            delim_range.end - 1,
                            Token::Hash,
                            delim_range.end,
                        )));
                    } else {
                        self.buffer.push_back(Ok((
                            delim_range.start,
                            Token::MatchStart,
                            delim_range.end,
                        )));
                    }
                    is_first = false;
                }
                Delimiter::RegexStart if in_match.is_none() && in_regex.is_none() => {
                    in_regex = Some(Span::new(delim_range, Delimiter::RegexStart));
                    if delim_range.start > last_delimiter_end {
                        let raw = &self.input.buffer()[last_delimiter_end..delim_range.start];
                        if !raw.iter().all(u8::is_ascii_whitespace) {
                            let content = self.input.as_str(last_delimiter_end..delim_range.start);
                            let content = if is_first {
                                content.strip_prefix(' ').unwrap_or(content)
                            } else {
                                content
                            };
                            self.buffer.push_back(Ok((
                                last_delimiter_end,
                                Token::Raw(content),
                                delim_range.start,
                            )));
                        }
                    }
                    self.buffer.push_back(Ok((
                        delim_range.start,
                        Token::RegexStart,
                        delim_range.end,
                    )));
                    is_first = false;
                }
                Delimiter::MatchEnd if in_match.is_some() => {
                    last_delimiter_end = delim_range.end;
                    let match_start = in_match.take().unwrap();
                    if matches!(match_start.into_inner(), Delimiter::NumericMatchStart) {
                        self.tokenize_capture_or_match_numeric(Range::new(
                            match_start.end(),
                            delim_range.start,
                        ));
                    } else {
                        self.tokenize_capture_or_match(Range::new(
                            match_start.end(),
                            delim_range.start,
                        ));
                    }
                    self.buffer.push_back(Ok((
                        delim_range.start,
                        Token::MatchEnd,
                        delim_range.end,
                    )));
                    self.input.set_start(delim_range.end);
                    self.searcher.set_last_match_end(delim_range.end);
                }
                Delimiter::RegexEnd if in_regex.is_some() => {
                    last_delimiter_end = delim_range.end;
                    let regex_start = in_regex.take().unwrap();
                    let pattern_start = regex_start.end();
                    let raw = self.input.as_str(pattern_start..delim_range.start).trim();
                    self.buffer
                        .push_back(Ok((pattern_start, Token::Raw(raw), delim_range.start)));
                    self.buffer.push_back(Ok((
                        delim_range.start,
                        Token::RegexEnd,
                        delim_range.end,
                    )));
                    self.input.set_start(delim_range.end);
                    self.searcher.set_last_match_end(delim_range.end);
                }
                delim @ (Delimiter::RegexEnd | Delimiter::MatchEnd)
                    if in_match.is_none() && in_regex.is_none() =>
                {
                    self.buffer.push_back(Err(ParserError::UnrecognizedToken {
                        span: delim_range.into(),
                        token: AsRef::<str>::as_ref(&delim).to_string(),
                        expected: vec![
                            "literal".to_string(),
                            Token::MatchStart.to_string(),
                            Token::RegexStart.to_string(),
                        ],
                    }));
                }
                _ => continue,
            }
        }

        // The line has been sliced up in delimited parts, but we have to handle any trailing content
        if last_delimiter_end < eol && in_match.is_none() && in_regex.is_none() {
            let line = self.input.as_str(last_delimiter_end..eol);
            if !line.trim().is_empty() {
                let line = if is_first {
                    line.strip_prefix(' ').unwrap_or(line)
                } else {
                    line
                };
                self.buffer
                    .push_back(Ok((last_delimiter_end, Token::Raw(line), eol)));
            }
            self.input.set_start(eol);
            self.searcher.set_last_match_end(eol);
            self.delimiter_searcher.set_last_match_end(eol);
        }

        // Handle unclosed delimiters
        match (in_match, in_regex) {
            (None, None) => (),
            (Some(delim), _) => {
                // Unclosed match start, i.e. [[ or [[#
                self.buffer
                    .push_back(Err(ParserError::UnclosedSubstitution {
                        span: delim.span(),
                    }));
            }
            (_, Some(delim)) => {
                // Unclosed regex start, i.e. [[ or [[#
                self.buffer
                    .push_back(Err(ParserError::UnclosedRegex { span: delim.span() }));
            }
        }
    }

    fn tokenize_capture_or_match(&mut self, range: Range<usize>) {
        let mut chars = self.input.as_str(range).chars().peekable();
        let mut offset = range.start;
        while let Some(c) = chars.next() {
            let next_offset = offset + c.len_utf8();
            match c {
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let start = offset;
                    let mut end = next_offset;

                    while let Some(&c) = chars.peek() {
                        match c {
                            c if c.is_ascii_alphanumeric() => {
                                end += c.len_utf8();
                                chars.next();
                            }
                            '_' => {
                                end += '_'.len_utf8();
                                chars.next();
                            }
                            c if c.is_whitespace() || c == ':' => {
                                break;
                            }
                            _ => {
                                self.buffer.push_back(Ok((
                                    start,
                                    Token::Error(LexerError::InvalidIdentifier {
                                        span: SourceSpan::from(start..(end + 1)),
                                    }),
                                    end + 1,
                                )));
                                self.buffer.push_back(Ok((
                                    end + 1,
                                    Token::Raw(self.input.as_str((end + 1)..range.end)),
                                    range.end,
                                )));
                                return;
                            }
                        }
                    }
                    self.buffer.push_back(Ok((
                        start,
                        Token::from_keyword_or_ident(self.input.as_str(start..end)),
                        end,
                    )));
                    offset = end;
                    continue;
                }
                '@' => self.buffer.push_back(Ok((offset, Token::At, next_offset))),
                '$' => self
                    .buffer
                    .push_back(Ok((offset, Token::Dollar, next_offset))),
                ':' => {
                    self.buffer
                        .push_back(Ok((offset, Token::Colon, next_offset)));
                    // The remainder of the match block is raw
                    let raw = self.input.as_str(next_offset..range.end);
                    self.buffer
                        .push_back(Ok((offset + 1, Token::Raw(raw), range.end)));
                    return;
                }
                c if c.is_whitespace() => (),
                unexpected => {
                    self.buffer.push_back(Ok((
                        offset,
                        Token::Error(LexerError::UnexpectedCharacter {
                            span: SourceSpan::from(offset..next_offset),
                            unexpected,
                        }),
                        next_offset,
                    )));
                    self.buffer.push_back(Ok((
                        next_offset,
                        Token::Raw(self.input.as_str(next_offset..range.end)),
                        range.end,
                    )));
                    return;
                }
            }
            offset = next_offset;
        }
    }

    fn tokenize_capture_or_match_numeric(&mut self, range: Range<usize>) {
        let mut chars = self.input.as_str(range).chars().peekable();
        let mut offset = range.start;
        let mut strip_whitespace = true;
        while let Some(c) = chars.next() {
            let mut next_offset = offset + c.len_utf8();
            match c {
                '#' => {
                    strip_whitespace = false;
                    self.buffer
                        .push_back(Ok((offset, Token::Hash, next_offset)));
                }
                '%' => {
                    strip_whitespace = false;
                    self.buffer
                        .push_back(Ok((offset, Token::Percent, next_offset)));
                }
                '.' => {
                    strip_whitespace = false;
                    self.buffer.push_back(Ok((offset, Token::Dot, next_offset)));
                }
                ',' => {
                    self.buffer
                        .push_back(Ok((offset, Token::Comma, next_offset)));
                }
                '+' => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::Plus, next_offset)));
                }
                '-' => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::Plus, next_offset)));
                }
                '@' => {
                    strip_whitespace = false;
                    self.buffer.push_back(Ok((offset, Token::At, next_offset)));
                }
                '$' => {
                    strip_whitespace = false;
                    self.buffer
                        .push_back(Ok((offset, Token::Dollar, next_offset)));
                }
                '=' if matches!(chars.peek(), Some(&'=')) => {
                    strip_whitespace = true;
                    chars.next();
                    next_offset += '='.len_utf8();
                    self.buffer
                        .push_back(Ok((offset, Token::Equals, next_offset)));
                }
                '(' => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::LParen, next_offset)));
                }
                ')' => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::RParen, next_offset)));
                }
                ':' => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::Colon, next_offset)));
                }
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let mut end = next_offset;
                    while let Some(&c) = chars.peek() {
                        match c {
                            c if c.is_ascii_alphanumeric() => {
                                end += c.len_utf8();
                                chars.next();
                            }
                            '_' => {
                                end += c.len_utf8();
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    self.buffer.push_back(Ok((
                        offset,
                        Token::from_keyword_or_ident(self.input.as_str(offset..end)),
                        end,
                    )));
                    strip_whitespace = true;
                    offset = end;
                    continue;
                }
                c if c.is_ascii_digit() => {
                    let mut end = next_offset;
                    while let Some(&c) = chars.peek() {
                        match c {
                            c if c.is_ascii_digit() => {
                                end += 1;
                                chars.next();
                            }
                            _ => break,
                        }
                    }
                    match self.input.as_str(offset..end).parse::<i64>() {
                        Ok(value) => {
                            self.buffer.push_back(Ok((offset, Token::Num(value), end)));
                        }
                        Err(err) => {
                            self.buffer.push_back(Ok((
                                offset,
                                Token::Error(LexerError::InvalidNumber {
                                    span: SourceSpan::from(offset..end),
                                    error: err,
                                }),
                                end,
                            )));
                        }
                    }
                    strip_whitespace = true;
                    offset = end;
                    continue;
                }
                c if c.is_ascii_whitespace() && strip_whitespace => (),
                unexpected => {
                    self.buffer.push_back(Ok((
                        offset,
                        Token::Error(LexerError::UnexpectedCharacter {
                            span: SourceSpan::from(offset..next_offset),
                            unexpected,
                        }),
                        next_offset,
                    )));
                    self.buffer.push_back(Ok((
                        next_offset,
                        Token::Raw(self.input.as_str(next_offset..range.end)),
                        range.end,
                    )));
                    return;
                }
            }
            offset = next_offset;
        }
    }
}
impl<'input> Iterator for Lexer<'input> {
    type Item = Lexed<'input>;

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let mut res = self.lex();
        loop {
            if let Some(Ok((_, Token::Lf, _))) = res.as_ref() {
                // Drop leading newlines
                if self.leading_lf {
                    res = self.lex();
                    continue;
                }
                // Collapse newlines into last newline token
                if let Some(Ok((_, Token::Lf, _))) = self.buffer.front() {
                    res = self.lex();
                    continue;
                }
                break;
            } else {
                self.leading_lf = false;
                break;
            }
        }
        res
    }
}
