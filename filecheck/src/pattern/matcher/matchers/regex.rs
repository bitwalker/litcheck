use regex_automata::{util::captures::Captures, PatternID};

use crate::{
    ast::{Capture, RegexPattern},
    common::*,
    expr::ValueType,
};

/// This matcher is used to match a single regular expression
///
/// This essentially corresponds to [SubstringMatcher], but
/// with regular expressions instead of literal strings.
pub struct RegexMatcher<'a> {
    /// The source pattern from which the regex was compiled
    pattern: Span<Cow<'a, str>>,
    /// The compiled form of the input regex
    regex: Regex,
    /// Metadata about captures in the pattern
    captures: Vec<Capture>,
}
impl<'a> fmt::Debug for RegexMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexMatcher")
            .field("pattern", &self.pattern)
            .field("captures", &self.captures)
            .finish()
    }
}
impl<'a> RegexMatcher<'a> {
    pub fn new(pattern: RegexPattern<'a>, interner: &StringInterner) -> DiagResult<Self> {
        let span = pattern.span();
        let regex = Regex::builder()
            .build(pattern.as_ref())
            .map_err(|error| build_error_to_diagnostic(error, 1, |_| span))?;

        // Compute capture group information
        let groups = regex.group_info();
        let num_captures = groups.group_len(PatternID::ZERO);
        let mut captures = vec![Capture::Ignore(span); num_captures];
        for capture in pattern.captures.into_iter() {
            if let Capture::Ignore(_) = capture {
                continue;
            }
            if let Some(name) = capture.group_name() {
                let group_name = interner.resolve(name);
                let group_id = groups
                    .to_index(PatternID::ZERO, group_name)
                    .unwrap_or_else(|| panic!("expected group for capture of '{group_name}'"));
                captures[group_id] = capture;
            } else {
                assert_eq!(
                    &captures[0],
                    &Capture::Ignore(span),
                    "{capture:?} would overwrite a previous implicit capture group"
                );
                captures[0] = capture;
            }
        }

        Ok(Self {
            pattern: pattern.pattern,
            regex,
            captures,
        })
    }

    pub fn new_nocapture(pattern: Span<Cow<'a, str>>) -> DiagResult<Self> {
        let span = pattern.span();
        let regex = Regex::builder()
            .build(pattern.as_ref())
            .map_err(|error| build_error_to_diagnostic(error, 1, |_| span))?;

        // Compute capture group information
        let groups = regex.group_info();
        let num_captures = groups.group_len(PatternID::ZERO);
        let captures = vec![Capture::Ignore(span); num_captures];

        Ok(Self {
            pattern,
            regex,
            captures,
        })
    }
}
impl<'a> MatcherMut for RegexMatcher<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        self.try_match(input, context)
    }
}
impl<'a> Matcher for RegexMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let regex_input = input.into();
        let mut captures = self.regex.create_captures();
        self.regex.search_captures(&regex_input, &mut captures);
        if let Some(matched) = captures.get_match() {
            let span = SourceSpan::from(matched.range());
            let mut capture_infos = Vec::with_capacity(captures.group_len());
            for (index, (maybe_capture_span, capture)) in captures
                .iter()
                .zip(self.captures.iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = maybe_capture_span {
                    let captured = input.as_str(capture_span.range());
                    let capture_span = SourceSpan::from(capture_span.range());
                    let result = try_convert_capture_to_type(
                        matched.pattern(),
                        index,
                        self.pattern.span(),
                        span,
                        Span::new(capture_span, captured),
                        capture,
                        &captures,
                        context,
                    );
                    match result {
                        Ok(capture_info) => {
                            capture_infos.push(capture_info);
                        }
                        Err(error) => return Ok(MatchResult::failed(error)),
                    }
                }
            }
            Ok(MatchResult::ok(MatchInfo {
                span,
                pattern_span: self.pattern.span(),
                pattern_id: 0,
                captures: capture_infos,
            }))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}
impl<'a> Spanned for RegexMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }
}

#[allow(clippy::too_many_arguments)]
pub fn try_convert_capture_to_type<'input, 'context, C>(
    pattern_id: PatternID,
    group_id: usize,
    pattern_span: SourceSpan,
    overall_span: SourceSpan,
    captured: Span<&'input str>,
    capture: Capture,
    captures: &Captures,
    context: &C,
) -> Result<CaptureInfo<'input>, CheckFailedError>
where
    C: Context<'input, 'context> + ?Sized,
{
    let (capture_span, captured) = captured.into_parts();
    let name = capture.name();
    let value = match capture.value_type() {
        ValueType::String => Value::Str(Cow::Borrowed(captured)),
        ValueType::Number(format) => {
            match Number::parse_with_format(Span::new(capture_span, captured), format) {
                Ok(n) => Value::Num(Expr::Num(n)),
                Err(error) => {
                    return Err(CheckFailedError::MatchFoundConstraintFailed {
                        span: overall_span,
                        input_file: context.input_file(),
                        pattern: Some(RelatedCheckError {
                            span: pattern_span,
                            match_file: context.match_file(),
                        }),
                        error: Some(RelatedError::new(Report::new(error))),
                        help: Some(if let Some(name) = name {
                            let name = context.resolve(name);
                            format!("expected {}; the constraint was required when parsing the capture group for '{name}'", format.describe())
                        } else if let Some(group_name) =
                            captures.group_info().to_name(pattern_id, group_id)
                        {
                            format!("expected {}; the constraint was required when parsing the capture group named '{group_name}'", format.describe())
                        } else {
                            format!("expected {}; the constraint was required when parsing capture group {group_id}", format.describe())
                        }),
                    });
                }
            }
        }
    };

    Ok(CaptureInfo {
        span: capture_span,
        pattern_span,
        index: group_id,
        value,
        capture,
    })
}

pub(crate) trait RegexBuildError: std::error::Error + std::fmt::Display {
    #[inline(always)]
    fn pattern(&self) -> Option<PatternID> {
        None
    }

    #[inline(always)]
    fn syntax_error(&self) -> Option<&regex_syntax::Error> {
        None
    }
}
impl RegexBuildError for regex_automata::meta::BuildError {
    #[inline(always)]
    fn pattern(&self) -> Option<PatternID> {
        regex_automata::meta::BuildError::pattern(self)
    }

    #[inline(always)]
    fn syntax_error(&self) -> Option<&regex_syntax::Error> {
        regex_automata::meta::BuildError::syntax_error(self)
    }
}
impl RegexBuildError for regex_automata::dfa::dense::BuildError {
    #[inline(always)]
    fn syntax_error(&self) -> Option<&regex_syntax::Error> {
        <Self as std::error::Error>::source(self)
            .and_then(|e| e.downcast_ref::<regex_automata::nfa::thompson::BuildError>())
            .and_then(|e| e.syntax_error())
    }
}
impl RegexBuildError for regex_automata::dfa::onepass::BuildError {
    #[inline(always)]
    fn syntax_error(&self) -> Option<&regex_syntax::Error> {
        <Self as std::error::Error>::source(self)
            .and_then(|e| e.downcast_ref::<regex_automata::nfa::thompson::BuildError>())
            .and_then(|e| e.syntax_error())
    }
}
impl RegexBuildError for regex_automata::nfa::thompson::BuildError {
    #[inline(always)]
    fn syntax_error(&self) -> Option<&regex_syntax::Error> {
        <Self as std::error::Error>::source(self).and_then(|e| e.downcast_ref())
    }
}

pub(crate) fn build_error_to_diagnostic<E, F>(
    error: E,
    num_patterns: usize,
    get_pattern_span: F,
) -> Report
where
    E: RegexBuildError,
    F: Fn(usize) -> SourceSpan,
{
    let diagnostic = if let Some(pattern_id) = error.pattern() {
        let span = get_pattern_span(pattern_id.as_usize());
        if let Some(syntax_err) = error.syntax_error() {
            Diag::new(format!("invalid regex pattern: {error}"))
                .with_help("a syntax error prevented us from compiling this pattern")
                .with_url("https://docs.rs/regex/latest/regex/index.html#syntax")
                .and_label(Label::new(span, syntax_err.to_string()))
        } else {
            Diag::new("unable to compile regex pattern set")
                .with_help("the pattern shown exceeded preconfigured limits during construction")
                .and_label(Label::at(span))
        }
    } else if num_patterns > 1 {
        Diag::new(format!("unable to compile regex pattern set: {error}"))
            .with_help("construction of a multi-pattern regex from this set failed to due to preconfigured limits")
            .with_labels((0..num_patterns).map(|pid| Label::at(get_pattern_span(pid)).into()))
    } else {
        Diag::new(format!("unable to compile regex pattern: {error}"))
            .with_help("construction of this regex failed to due to preconfigured limits")
            .with_label(Label::at(get_pattern_span(0)))
    };
    Report::from(diagnostic)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regex_matcher() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(r"CHECK: {{Name: b[[:alpha:]]*}}")
            .with_input(
                "
Name: foo
Field: 1

Name: bar
Field: 2
",
            );

        let pattern = RegexPattern::new(Span::new(
            SourceSpan::from(0..0),
            Cow::Borrowed("Name: b[[:alpha:]]*"),
        ));
        let matcher =
            RegexMatcher::new(pattern, &context.interner).expect("expected pattern to be valid");
        let mctx = context.match_context();
        let input = mctx.search();
        let result = matcher.try_match(input, &mctx)?;
        let info = result.info.expect("expected match");
        assert_eq!(info.span.offset(), 21);
        assert_eq!(info.span.len(), 9);
        assert_eq!(
            input.as_str(info.span.offset()..(info.span.offset() + info.span.len())),
            "Name: bar"
        );

        Ok(())
    }
}
