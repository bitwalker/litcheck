use std::{borrow::Cow, fmt};

use regex_automata::{meta::Regex, MatchKind, PatternID};

use litcheck::diagnostics::{Diag, DiagResult, LabeledSpan, Report, SourceSpan, Span, Spanned};

use super::*;
use crate::{check::Input, expr::ValueType};

/// This matcher is used to match a single regular expression
///
/// This essentially corresponds to [SubstringMatcher], but
/// with regular expressions instead of literal strings.
pub struct RegexMatcher {
    /// The span of the pattern in the check file from
    /// which this matcher was derived
    span: SourceSpan,
    /// The compiled form of the input regex
    pattern: Regex,
    /// Metadata about captures in the pattern
    captures: Vec<ValueType>,
}
impl fmt::Debug for RegexMatcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexMatcher")
            .field("regex", &self.pattern)
            .finish()
    }
}
impl RegexMatcher {
    pub fn new(pattern: Span<Cow<'_, str>>) -> DiagResult<Self> {
        let (span, pattern) = pattern.into_parts();
        let pattern = Regex::builder()
            .build(&pattern)
            .map_err(|error| build_error_to_diagnostic(error, 1, |_| span))?;

        // Compute capture group information
        let groups = pattern.group_info();
        let num_captures = groups.group_len(PatternID::ZERO);
        let captures = vec![ValueType::String; num_captures];

        Ok(Self {
            span,
            pattern,
            captures,
        })
    }
}
impl Matcher for RegexMatcher {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        let regex_input = input.into();
        let mut captures = self.pattern.create_captures();
        self.pattern.search_captures(&regex_input, &mut captures);
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
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(captured),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
                }
            }
            Some(MatchInfo {
                span,
                pattern_span: self.span,
                pattern_id: 0,
                captures: capture_infos,
            })
        } else {
            None
        }
    }
}
impl Spanned for RegexMatcher {
    fn span(&self) -> SourceSpan {
        self.span
    }
}

pub struct RegexSetMatcher<'a> {
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The compiled regex which will be used to search the input buffer
    pattern: Regex,
    /// Metadata about captures in the given patterns
    ///
    /// Each pattern gets its own vector of capture info, since
    /// there is no requirement that all patterns have the same
    /// number or type of captures
    captures: Vec<Vec<ValueType>>,
}
impl<'a> fmt::Debug for RegexSetMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexSetMatcher")
            .field("pattern", &self.pattern)
            .finish()
    }
}
impl<'a> RegexSetMatcher<'a> {
    pub fn new(patterns: Vec<Span<Cow<'a, str>>>) -> DiagResult<Self> {
        let pattern = Regex::builder()
            .configure(Regex::config().match_kind(MatchKind::All))
            .build_many(patterns.as_slice())
            .map_err(|error| {
                build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
            })?;

        // Compute capture group information
        let mut captures = vec![];
        let groups = pattern.group_info();
        for i in 0..patterns.len() {
            let pid = PatternID::new_unchecked(i);
            let num_captures = groups.group_len(pid);
            captures.push(vec![ValueType::String; num_captures]);
        }

        Ok(Self {
            patterns,
            pattern,
            captures,
        })
    }
}
impl<'a> Spanned for RegexSetMatcher<'a> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a> Matcher for RegexSetMatcher<'a> {
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>> {
        let regex_input = input.into();
        let mut captures = self.pattern.create_captures();
        self.pattern.search_captures(&regex_input, &mut captures);
        if let Some(matched) = captures.get_match() {
            let pattern_id = matched.pattern().as_usize();
            let pattern_span = self.patterns[pattern_id].span();
            let span = SourceSpan::from(matched.range());
            let mut capture_infos = Vec::with_capacity(captures.group_len());
            for (index, (capture, ty)) in captures
                .iter()
                .zip(self.captures[pattern_id].iter().copied())
                .enumerate()
            {
                if let Some(capture_span) = capture {
                    let captured = input.as_str(capture_span.range());
                    capture_infos.push(CaptureInfo {
                        span: SourceSpan::from(capture_span.range()),
                        index,
                        value: match ty {
                            ValueType::String => Value::Str(captured),
                            ValueType::Number(_) => todo!("numeric captures are unimplemented"),
                        },
                    });
                }
            }
            Some(MatchInfo {
                span,
                pattern_span,
                pattern_id,
                captures: capture_infos,
            })
        } else {
            None
        }
    }
}

fn build_error_to_diagnostic<F>(
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
    use std::fmt::Write;

    #[test]
    fn test_regex_matcher() {
        let mut input = String::new();
        writeln!(&mut input, "Name: foo").unwrap();
        writeln!(&mut input, "Field: 1").unwrap();
        writeln!(&mut input).unwrap();
        writeln!(&mut input, "Name: bar").unwrap();
        writeln!(&mut input, "Field: 2").unwrap();

        let pattern = Span::new(SourceSpan::from(0..0), Cow::Borrowed("Name: b[[:alpha:]]*"));
        let matcher = RegexMatcher::new(pattern).expect("expected pattern to be valid");
        let bytes = input.as_bytes();
        let input = Input::new(bytes, false).span(0..);
        let result = matcher.try_match(input);
        let info = result.expect("expected match");
        assert_eq!(info.span.offset(), 20);
        assert_eq!(info.span.len(), 9);
        assert_eq!(
            input.as_str(info.span.offset()..(info.span.offset() + info.span.len())),
            "Name: bar"
        );
    }
}
