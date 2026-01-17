use std::{fmt, ops::Range};

use regex::Regex;

use super::*;

thread_local! {
    static KEYWORD_RE: Regex = Regex::new("(DEFINE:|END\\.|REDEFINE:|REQUIRES:|RUN:|UNSUPPORTED:|XFAIL:|ALLOWED_RETRIES:)(.*)$").unwrap();
    static SUBSTITUTION_NAME_RE: Regex = Regex::new(r"^%\{[_a-zA-Z][-_:0-9a-zA-Z]*\}$").unwrap();
}

pub struct TestScriptParser<'a, S: ?Sized> {
    script: TestScript,
    source: &'a S,
    lines: Lines<'a>,
    errors: Vec<TestScriptError>,
    current_line: usize,
}
impl<'a, S> TestScriptParser<'a, S>
where
    S: NamedSourceFile + ?Sized,
{
    pub fn new(source: &'a S) -> Self {
        let script = TestScript::default();
        let lines = Lines::new(source.source());

        Self {
            script,
            source,
            lines,
            errors: vec![],
            current_line: 1,
        }
    }

    pub fn parse(mut self) -> Result<TestScript, InvalidTestScriptError> {
        match self.try_parse() {
            Ok(_) if !self.errors.is_empty() => {
                Err(InvalidTestScriptError::new(self.source.name(), self.errors))
            }
            Ok(_) => Ok(self.script),
            Err(err) => {
                self.errors.push(err);
                Err(InvalidTestScriptError::new(self.source.name(), self.errors))
            }
        }
    }

    fn try_parse(&mut self) -> Result<(), TestScriptError> {
        while let Some(line) = self.next_line() {
            if let Some(directive) = RawDirective::parse(line) {
                match directive.kind {
                    DirectiveKind::End => {
                        if directive.content.trim().is_empty() {
                            break;
                        }
                        continue;
                    }
                    DirectiveKind::Run => self.parse_run(directive)?,
                    DirectiveKind::Define | DirectiveKind::Redefine => {
                        self.parse_substitution(directive)?
                    }
                    DirectiveKind::Requires | DirectiveKind::Unsupported | DirectiveKind::Xfail => {
                        self.parse_bools(directive)
                    }
                    DirectiveKind::AllowedRetries => self.parse_int(directive),
                }
            }
        }

        if !self
            .script
            .commands
            .iter()
            .any(|c| matches!(c, ScriptCommand::Run(_)))
        {
            self.errors.push(TestScriptError::MissingRunDirective);
        }

        Ok(())
    }

    fn next_line(&mut self) -> Option<Span<&'a str>> {
        let line = self.lines.next()?;
        self.current_line += 1;
        Some(line)
    }

    fn parse_line(
        &mut self,
        directive: Span<RawDirective<'a>>,
        collapse_whitespace: bool,
    ) -> Result<Span<Cow<'a, str>>, TestScriptError> {
        let mut needs_continuation;
        let rest;
        let mut stripped_eol = directive.end();
        if let Some(stripped) = directive.content.strip_suffix('\\') {
            stripped_eol -= 1;
            needs_continuation = true;
            rest = stripped;
        } else {
            needs_continuation = false;
            rest = directive.content.trim_end();
            let num_trimmed_bytes = directive.content.len() - rest.len();
            stripped_eol -= num_trimmed_bytes;
        };

        let trimmed = rest.trim_start();
        let start = directive.start() + (rest.len() - trimmed.len());
        let mut span = start..stripped_eol;
        let mut output = StringRewriter::<'a>::new("");
        push_str_replacing_line_numbers(
            trimmed,
            &mut output,
            &span,
            self.current_line,
            &mut self.errors,
        );
        while needs_continuation {
            let prev_line = span.clone();
            let Some(line) = self.next_line() else {
                break;
            };
            span.end = line.end();
            let line_span = line.span();
            let line = match RawDirective::parse(line) {
                Some(cont_directive) => {
                    if cont_directive.kind != directive.kind {
                        self.errors
                            .push(TestScriptError::InvalidDirectiveContinuation {
                                span: line_span,
                                prev_line: SourceSpan::from(prev_line),
                                expected: directive.kind,
                            });
                    }
                    let (span, cont_directive) = cont_directive.into_parts();
                    Span::new(span, cont_directive.content)
                }
                None => {
                    self.errors
                        .push(TestScriptError::MissingDirectiveContinuation {
                            span: line_span,
                            prev_line: SourceSpan::from(prev_line),
                            expected: directive.kind,
                        });
                    line
                }
            };
            let mut rest;
            if let Some(stripped) = line.strip_suffix('\\') {
                needs_continuation = true;
                if collapse_whitespace {
                    output.push(' ');
                    rest = stripped.trim();
                } else {
                    output.push('\\');
                    output.push('\n');
                    rest = stripped;
                }
            } else {
                needs_continuation = false;
                rest = line.trim_end();
                span.end -= line.len() - rest.len();
                if collapse_whitespace {
                    output.push(' ');
                    rest = rest.trim_start();
                } else {
                    output.push('\\');
                    output.push('\n');
                }
            }

            push_str_replacing_line_numbers(
                rest,
                &mut output,
                &span,
                self.current_line,
                &mut self.errors,
            );
        }

        Ok(Span::new(span, output.build()))
    }

    fn parse_run(&mut self, directive: Span<RawDirective<'a>>) -> Result<(), TestScriptError> {
        let line = self.parse_line(directive, /*collapse_whitespace=*/ false)?;
        let (span, line) = line.into_parts();
        self.script.commands.push(ScriptCommand::Run(Run {
            span,
            line: self.current_line,
            command: line.to_string(),
        }));
        Ok(())
    }

    fn parse_substitution(
        &mut self,
        directive: Span<RawDirective<'a>>,
    ) -> Result<(), TestScriptError> {
        let kind = directive.kind;
        let line = self.parse_line(directive, /*collapse_whitespace=*/ true)?;

        if let Some((key, value)) = line.split_once('=') {
            let key = key.trim();
            let value = value.trim();
            match Substitution::new(
                line.span(),
                self.current_line,
                key,
                value,
                kind == DirectiveKind::Redefine,
            ) {
                Ok(subst) => {
                    self.script
                        .commands
                        .push(ScriptCommand::Substitution(subst));
                }
                Err(err) => {
                    self.errors.push(err.into());
                }
            }
        }

        Ok(())
    }

    fn parse_bools(&mut self, directive: Span<RawDirective<'a>>) {
        let start = directive.start();
        for expr in directive.content.split(',') {
            let len = expr.len();
            let expr = expr.trim_start();
            let trimmed_len = expr.len();
            let start = start + (len - trimmed_len);
            let expr = expr.trim_end();
            let end = start + expr.len();
            let span = SourceSpan::from(start..end);
            let expr = if expr == "*" {
                Ok(Span::new(span, BooleanExpr::Lit(true)))
            } else {
                expr.parse::<BooleanExpr>()
                    .map(|expr| Span::new(span, expr))
                    .map_err(|err| err.with_span_offset(start))
            };
            match expr {
                Ok(expr) => match directive.kind {
                    DirectiveKind::Requires => self.script.requires.push(expr),
                    DirectiveKind::Unsupported => self.script.unsupported.push(expr),
                    DirectiveKind::Xfail => self.script.xfails.push(expr),
                    kind => unreachable!("{} directive does not support boolean expressions", kind),
                },
                Err(err) => self.errors.push(err.into()),
            }
        }
    }

    fn parse_int(&mut self, directive: Span<RawDirective<'a>>) {
        assert_eq!(directive.kind, DirectiveKind::AllowedRetries);

        let span = directive.span();
        if let Some(retries) = self.script.allowed_retries.as_ref() {
            self.errors.push(TestScriptError::DirectiveConflict {
                span,
                prev_span: retries.span(),
                kind: directive.kind,
            });
            return;
        }

        match directive.content.trim().parse::<usize>() {
            Ok(value) => {
                self.script.allowed_retries = Some(Span::new(span, value));
            }
            Err(error) => {
                self.errors.push(TestScriptError::InvalidIntegerDirective {
                    span,
                    kind: directive.kind,
                    error,
                });
            }
        }
    }
}

struct StringRewriter<'a> {
    buf: Cow<'a, str>,
}
impl<'a> StringRewriter<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            buf: Cow::Borrowed(s),
        }
    }

    pub fn push(&mut self, c: char) {
        match self.buf {
            Cow::Borrowed("") => {
                let mut s = String::with_capacity(32);
                s.push(c);
                self.buf = Cow::Owned(s);
            }
            Cow::Borrowed(prev) => {
                let mut buf = String::with_capacity(prev.len() + 1);
                buf.push_str(prev);
                buf.push(c);
                self.buf = Cow::Owned(buf);
            }
            Cow::Owned(ref mut s) => {
                s.push(c);
            }
        }
    }

    pub fn push_str(&mut self, s: &'a str) {
        match self.buf {
            Cow::Borrowed(ref mut buf) if buf.is_empty() => {
                *buf = s;
            }
            Cow::Borrowed(prev) => {
                let mut buf = String::with_capacity(prev.len() + s.len());
                buf.push_str(prev);
                buf.push_str(s);
                self.buf = Cow::Owned(buf);
            }
            Cow::Owned(ref mut buf) => {
                buf.push_str(s);
            }
        }
    }

    pub fn push_format(&mut self, s: fmt::Arguments<'_>) -> fmt::Result {
        use std::fmt::Write;

        match self.buf {
            Cow::Borrowed("") => {
                self.buf = Cow::Owned(s.to_string());
            }
            Cow::Borrowed(prev) => {
                let mut buf = String::with_capacity(prev.len() + 32);
                buf.push_str(prev);
                write!(&mut buf, "{s}")?;
                self.buf = Cow::Owned(buf);
            }
            Cow::Owned(ref mut buf) => {
                write!(buf, "{s}")?;
            }
        }

        Ok(())
    }

    pub fn build(self) -> Cow<'a, str> {
        self.buf
    }
}

struct Lines<'a> {
    iter: std::str::SplitInclusive<'a, char>,
    offset: usize,
}
impl<'a> Lines<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            iter: s.split_inclusive('\n'),
            offset: 0,
        }
    }
}
impl<'a> Iterator for Lines<'a> {
    type Item = Span<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.iter.next()?;
        let start = self.offset;
        self.offset += line.len();
        let (stripped, line) = line
            .strip_suffix('\n')
            .map(|line| (1, line))
            .unwrap_or((0, line));
        let (stripped, line) = line
            .strip_suffix('\r')
            .map(|line| (stripped + 1, line))
            .unwrap_or((stripped, line));
        let end = self.offset - stripped;
        Some(Span::new(SourceSpan::from(start..end), line))
    }
}

thread_local! {
    static LINE_NUMBER_RE: Regex = Regex::new(r"%\(line(?<offset> *[+-] *\d+)?\)").unwrap();
}

fn push_str_replacing_line_numbers<'a>(
    input: &'a str,
    buf: &mut StringRewriter<'a>,
    span: &Range<usize>,
    current_line: usize,
    errors: &mut Vec<TestScriptError>,
) {
    LINE_NUMBER_RE.with(|re| {
        let mut last_match = 0;
        for captures in re.captures_iter(input) {
            let range = captures.get(0).unwrap().range();
            buf.push_str(unsafe {
                core::str::from_utf8_unchecked(&input.as_bytes()[last_match..range.start])
            });
            if let Some(offset_match) = captures.name("offset") {
                let offset_span =
                    (offset_match.start() + span.start)..(offset_match.end() + span.start);
                let offset = offset_match.as_str().trim_start();
                let (sign, offset) = offset.split_at(1);
                let is_negative = sign == "-";
                let offset = match offset.trim_start().parse::<usize>() {
                    Ok(offset) => offset,
                    Err(error) => {
                        errors.push(TestScriptError::InvalidLineSubstitution {
                            span: offset_span.into(),
                            error,
                        });
                        0
                    }
                };
                let line = if is_negative {
                    current_line.saturating_sub(offset)
                } else {
                    current_line + offset
                };
                buf.push_format(format_args!("{line}")).unwrap();
            } else {
                buf.push_format(format_args!("{current_line}")).unwrap();
            }
            last_match = range.end;
        }
        buf.push_str(&input[last_match..]);
    })
}
