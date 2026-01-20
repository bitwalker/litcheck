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

/// Used to recognize false MatchEnd delimiters (i.e. ']]') that occur when searching for the end
/// of a match pattern that may contain brackets.
static FALSE_MATCH_END_PATTERN: std::sync::LazyLock<Regex> = std::sync::LazyLock::new(|| {
    // This pattern matches false ends due to character classes in capture patterns, e.g.:
    //
    // [[REG:r[[:digit:]]]]
    // ^- match start  ^ ^- match end
    //                 `- false end
    //
    // [[REG:r[0-9]]]
    // ^- match start
    //            ^^- match end
    //            `- false end
    //
    // The haystack for the search starts immediately after the opening `[[` and includes all
    // content up to (and including) the closing `]]` we're looking at.
    //
    // 1. From the start of the haystack
    // 2. Match any char until we hit an opening `[`, which must be a nested character class
    // 3. Either:
    //   * Expect a named ASCII character class, e.g. `:space:`
    //   * Expect any character other than an unescaped `]`
    // 4. Repeat 2-3 until we reach a closing `]` at the end of the haystack
    Regex::new(r"\A.*\[(\[:[a-z]+:\]|([^\]\\]|\\.)+\])\]\z").unwrap()
});

/// The lexer that is used to tokenize a check file
pub struct Lexer<'input> {
    input: Input<'input>,
    patterns: Vec<Pattern>,
    check_prefixes: Vec<Symbol>,
    seen_prefixes: Vec<bool>,
    regex: Regex,
    searcher: RegexSearcher<'input>,
    cache: regex_automata::meta::Cache,
    captures: regex_automata::util::captures::Captures,
    delimiter_patterns: aho_corasick::AhoCorasick,
    delimiter_searcher: AhoCorasickSearcher<'input>,
    /// The token buffer, used when tokenizing lines
    buffer: VecDeque<Lexed<'input>>,
    /// When we have reached true Eof, this gets set to true, and the only token
    /// produced after that point is Token::Eof, or None, depending on how you are
    /// consuming the lexer
    eof: bool,
    /// When we first start parsing, this is true until we've observed a token
    /// which is not Lf or Eof, so that we trim all leading empty lines which
    /// will just be ignored anyway. This simplifies parsing by ensuring the
    /// rules only need to account for actual content lines in the file.
    leading_lf: bool,
    /// Is --strict-whitespace enabled
    strict_whitespace: bool,
    /// Is --match-full-lines enabled
    match_full_lines: bool,
}
impl<'input> Lexer<'input> {
    /// Produces an instance of the lexer with the lexical analysis to be performed on the `input`
    /// string. Note that no lexical analysis occurs until the lexer has been iterated over.
    pub fn new<S>(source_id: SourceId, source: &'input S, config: &Config) -> Self
    where
        S: ?Sized + AsRef<str> + 'input,
    {
        let source = source.as_ref();
        let buffer = source.as_bytes();
        let input = Input::new(source_id, buffer, false);
        let mut patterns = Pattern::generate_comment_patterns(&config.options.comment_prefixes)
            .collect::<Vec<_>>();
        patterns.extend(Pattern::generate_check_patterns(
            &config.options.check_prefixes,
        ));
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
            check_prefixes: config.options.check_prefixes.to_vec(),
            seen_prefixes: vec![false; config.options.check_prefixes.len()],
            regex,
            searcher,
            cache,
            captures,
            delimiter_patterns,
            delimiter_searcher,
            buffer: VecDeque::with_capacity(128),
            eof,
            leading_lf: true,
            strict_whitespace: config.options.strict_whitespace,
            match_full_lines: config.options.match_full_lines,
        }
    }

    pub fn unused_prefixes(&self) -> Vec<Symbol> {
        self.check_prefixes
            .iter()
            .zip(self.seen_prefixes.iter().copied())
            .filter_map(|(prefix, used)| if used { None } else { Some(*prefix) })
            .collect()
    }

    pub fn current_offset(&self) -> SourceSpan {
        SourceSpan::at(self.input.source_id(), self.input.start() as u32)
    }

    #[inline(always)]
    pub fn source_id(&self) -> SourceId {
        self.input.source_id()
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
            let span =
                SourceSpan::from_range_unchecked(self.input.source_id(), range.start..range.end);
            let pattern = &self.patterns[pid.as_usize()];
            let pattern_ty = pattern.ty;
            if let Check::Comment = pattern_ty {
                return self.tokenize_comment(range);
            }

            let prefix_span = self.captures.get_group_by_name("prefix").unwrap();
            let prefix = self.input.as_str(prefix_span.start..prefix_span.end);
            if let Some(index) = self
                .check_prefixes
                .iter()
                .position(|pfx| pfx.as_ref() == prefix)
            {
                self.seen_prefixes[index] = true;
            }
            match pattern_ty {
                Check::Count => {
                    let valid = self.tokenize_check_count_prefix(range);
                    if !valid {
                        self.captures.set_pattern(None);
                        return;
                    }
                }
                ty => {
                    self.buffer.push_back(Ok((
                        span.start().to_usize(),
                        Token::Check(ty.try_into().unwrap()),
                        span.end().to_usize(),
                    )));
                }
            }
            let literal = self.tokenize_optional_modifiers();
            self.buffer.push_back(Ok((
                span.end().to_usize() - 1,
                Token::Colon,
                span.end().to_usize(),
            )));
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
            let modifiers = core::str::from_utf8(&self.input.buffer()[span.range()])
                .expect("expected capture to be valid utf8");
            let mut literal = false;
            let mut cursor = 0;
            for (i, modifier) in modifiers.split(',').enumerate() {
                if i > 0 {
                    cursor += 1;
                }
                let partially_trimmed_modifier = modifier.trim_start();
                let modifier_start = cursor + modifier.len() - partially_trimmed_modifier.len();
                let trimmed_modifier = partially_trimmed_modifier.trim_end();
                let modifier_len = trimmed_modifier.len();
                match trimmed_modifier {
                    "LITERAL" => {
                        self.buffer.push_back(Ok((
                            span.start + modifier_start,
                            Token::Modifier(CheckModifier::LITERAL),
                            span.start + modifier_start + modifier_len,
                        )));
                        literal = true;
                        cursor += modifier.len();
                    }
                    _ => {
                        let start = span.start + modifier_start;
                        self.buffer
                            .push_back(Err(ParserError::InvalidCheckModifier {
                                span: SourceSpan::from_range_unchecked(
                                    self.source_id(),
                                    start..(start + modifier_len),
                                ),
                            }));
                    }
                }
            }
            literal
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
                    Token::Check(ast::Check::Count(count as usize)),
                    count_span.end,
                )));
                true
            }
            Err(error) => {
                let token = Token::Error(LexerError::BadCount {
                    span: SourceSpan::from_range_unchecked(
                        self.input.source_id(),
                        count_span.start..count_span.end,
                    ),
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
        let mut start = self.input.start();
        let eol = self.input.next_newline().unwrap_or(self.input.end());

        if literal {
            let raw = self.input.as_str(start..eol);
            let stripped = if self.strict_whitespace || self.match_full_lines {
                raw.strip_prefix(' ').unwrap_or(raw)
            } else {
                raw.trim_start()
            };
            let shift = raw.len().abs_diff(stripped.len());
            self.buffer
                .push_back(Ok((start + shift, Token::Raw(stripped), eol)));
            self.input.set_start(eol);
            return;
        } else if self.strict_whitespace || self.match_full_lines {
            // If --strict-whitespace/--match-full-lines are in use, do not remove excess leading
            // whitespace
            let raw = self.input.as_str(start..eol);
            let stripped = raw.strip_prefix(' ').unwrap_or(raw);
            let shift = raw.len().abs_diff(stripped.len());
            start += shift;
            self.input.set_start(start);
        } else {
            // Otherwise, remove all leading space separating prefix and pattern
            let raw = self.input.as_str(start..eol);
            let stripped = raw.trim_ascii_start();
            let shift = raw.len().abs_diff(stripped.len());
            start += shift;
            self.input.set_start(start);
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
                    // Detect false starts where a `[[` appears within brackets that are part of the
                    // overall pattern being matched, e.g. `[[[REG]]]`. Since match patterns can
                    // never start with a `[`, we can safely ignore these false starts by moving
                    // the search cursor back one character and trying again
                    if matches!(delim, Delimiter::MatchStart)
                        && let Some(b'[') = self.input.buffer().get(delim_range.end)
                    {
                        self.delimiter_searcher
                            .set_range((delim_range.start + 1)..eol);
                        continue;
                    }

                    in_match = Some(Span::new(
                        SourceSpan::from_range_unchecked(
                            self.input.source_id(),
                            delim_range.start..delim_range.end,
                        ),
                        delim,
                    ));
                    if delim_range.start > last_delimiter_end {
                        let raw = &self.input.buffer()[last_delimiter_end..delim_range.start];
                        if !raw.iter().all(u8::is_ascii_whitespace) || !is_first {
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
                    in_regex = Some(Span::new(
                        SourceSpan::from_range_unchecked(
                            self.input.source_id(),
                            delim_range.start..delim_range.end,
                        ),
                        Delimiter::RegexStart,
                    ));
                    if delim_range.start > last_delimiter_end {
                        let raw = &self.input.buffer()[last_delimiter_end..delim_range.start];
                        if !raw.iter().all(u8::is_ascii_whitespace) || !is_first {
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
                    // Check if this closing delimiter might be part of the underlying pattern;
                    // specifically, regexes may use character classes, e.g. [:digit:] or [0-9], and
                    // to add even more complexity, the former may be used in negations, e.g.
                    // [^[:digit:]].
                    //
                    // In both cases, the match end delimiter we're looking for is incorrectly
                    // identified and includes all or part of the character class delimiter. We
                    // check if this is the case by matching the content after the match start
                    // delimiter up to and including the current match end delimiter. If the
                    // following regex matches that content, we ignore this match and shift the
                    // delimiter searcher back by one character, forcing it to re-attempt the search
                    // while including part of the false match end delimiter, so that we properly
                    // handle the case where the delimiter overlapped the false one and the real
                    // one.
                    let match_start = in_match.take().unwrap();
                    if FALSE_MATCH_END_PATTERN.is_match(
                        self.input
                            .as_str(match_start.span().end().to_usize()..delim_range.end),
                    ) {
                        in_match = Some(match_start);
                        self.delimiter_searcher
                            .set_range((delim_range.start + 1)..eol);
                        continue;
                    }

                    last_delimiter_end = delim_range.end;

                    self.tokenize_capture_or_match(
                        Range::new(match_start.span().end().to_usize(), delim_range.start),
                        matches!(match_start.into_inner(), Delimiter::NumericMatchStart),
                    );

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
                    let pattern_start = regex_start.span().end().to_usize();
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
                // Treat the unmatched delimiter as part of a literal pattern
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

    fn tokenize_capture_or_match(&mut self, range: Range<usize>, is_numeric: bool) {
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
                        .push_back(Ok((offset, Token::Minus, next_offset)));
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
                ':' if is_numeric => {
                    strip_whitespace = true;
                    self.buffer
                        .push_back(Ok((offset, Token::Colon, next_offset)));
                }
                ':' => {
                    strip_whitespace = false;
                    self.buffer
                        .push_back(Ok((offset, Token::Colon, next_offset)));
                    // The remainder of the match block is raw
                    let raw = self.input.as_str(next_offset..range.end);
                    self.buffer
                        .push_back(Ok((offset + 1, Token::Raw(raw), range.end)));
                    return;
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
                    let mut radix = 10;
                    let is_valid_digit: fn(&char) -> bool;
                    if c == '0' {
                        match chars.peek() {
                            Some('x') => {
                                radix = 16;
                                chars.next();
                                offset = next_offset + 1;
                                next_offset += 1;
                                is_valid_digit = char::is_ascii_hexdigit;
                            }
                            Some('b') => {
                                radix = 2;
                                chars.next();
                                offset = next_offset + 1;
                                next_offset += 1;
                                is_valid_digit = is_valid_binary_digit;
                            }
                            _ => {
                                is_valid_digit = char::is_ascii_digit;
                            }
                        }
                    } else {
                        is_valid_digit = char::is_ascii_digit;
                    }

                    let mut end = next_offset;
                    while let Some(c) = chars.peek()
                        && is_valid_digit(c)
                    {
                        end += 1;
                        chars.next();
                    }
                    let n = self.input.as_str(offset..end);
                    match i128::from_str_radix(n, radix) {
                        Ok(value) => {
                            self.buffer.push_back(Ok((offset, Token::Num(value), end)));
                        }
                        Err(err) => {
                            self.buffer.push_back(Ok((
                                offset,
                                Token::Error(LexerError::InvalidNumber {
                                    span: SourceSpan::from_range_unchecked(
                                        self.input.source_id(),
                                        offset..end,
                                    ),
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
                            span: SourceSpan::from_range_unchecked(
                                self.input.source_id(),
                                offset..next_offset,
                            ),
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

#[inline(always)]
const fn is_valid_binary_digit(c: &char) -> bool {
    matches!(*c, '0' | '1')
}
