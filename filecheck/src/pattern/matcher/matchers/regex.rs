use regex_automata::PatternID;

use crate::{common::*, expr::ValueType};

/// This matcher is used to match a single regular expression
///
/// This essentially corresponds to [SubstringMatcher], but
/// with regular expressions instead of literal strings.
pub struct RegexMatcher<'a> {
    /// The source string from which the regex was compiled
    pattern: Span<Cow<'a, str>>,
    /// The compiled form of the input regex
    regex: Regex,
    /// Metadata about captures in the pattern
    captures: Vec<ValueType>,
}
impl<'a> fmt::Debug for RegexMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexMatcher")
            .field("pattern", &self.pattern)
            .finish()
    }
}
impl<'a> RegexMatcher<'a> {
    pub fn new(pattern: Span<Cow<'a, str>>) -> DiagResult<Self> {
        let span = pattern.span();
        let regex = Regex::builder()
            .build(pattern.as_ref())
            .map_err(|error| build_error_to_diagnostic(error, 1, |_| span))?;

        // Compute capture group information
        let groups = regex.group_info();
        let num_captures = groups.group_len(PatternID::ZERO);
        let captures = vec![ValueType::String; num_captures];

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
            for (index, (capture, ty)) in captures
                .iter()
                .zip(self.captures.iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = capture {
                    let captured = input.as_str(capture_span.range());
                    capture_infos.push(CaptureInfo {
                        span: SourceSpan::from(capture_span.range()),
                        pattern_span: self.pattern.span(),
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(Cow::Borrowed(captured)),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
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

pub fn build_error_to_diagnostic<F>(
    error: regex_automata::meta::BuildError,
    num_patterns: usize,
    get_pattern_span: F,
) -> Report
where
    F: Fn(usize) -> SourceSpan,
{
    let diagnostic = if let Some(pattern_id) = error.pattern() {
        let span = get_pattern_span(pattern_id.as_usize());
        if let Some(syntax_err) = error.syntax_error() {
            Diag::new(format!("invalid regex pattern: {error}"))
                .with_help("a syntax error prevented us from compiling this pattern")
                .with_url("https://docs.rs/regex/latest/regex/index.html#syntax")
                .and_label(LabeledSpan::new_with_span(
                    Some(syntax_err.to_string()),
                    span,
                ))
        } else {
            Diag::new("unable to compile regex pattern set")
                .with_help("the pattern shown exceeded preconfigured limits during construction")
                .and_label(LabeledSpan::new_with_span(None, span))
        }
    } else if num_patterns > 1 {
        Diag::new(format!("unable to compile regex pattern set: {error}"))
            .with_help("construction of a multi-pattern regex from this set failed to due to preconfigured limits")
            .with_labels((0..num_patterns).map(|pid| LabeledSpan::new_with_span(None, get_pattern_span(pid))))
    } else {
        Diag::new(format!("unable to compile regex pattern: {error}"))
            .with_help("construction of this regex failed to due to preconfigured limits")
            .with_label(LabeledSpan::new_with_span(None, get_pattern_span(0)))
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

        let pattern = Span::new(SourceSpan::from(0..0), Cow::Borrowed("Name: b[[:alpha:]]*"));
        let matcher = RegexMatcher::new(pattern).expect("expected pattern to be valid");
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
