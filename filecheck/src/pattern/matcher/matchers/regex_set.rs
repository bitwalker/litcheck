use regex_automata::{MatchKind, PatternID};

use crate::{common::*, expr::ValueType, pattern::search::RegexSetSearcher};

use super::regex;

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
                regex::build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
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

    pub fn search<'input, 'patterns>(
        &'patterns self,
        input: Input<'input>,
    ) -> RegexSetSearcher<'a, 'patterns, 'input> {
        let pattern = self.pattern.clone();
        RegexSetSearcher::from_matcher(
            input,
            pattern,
            Cow::Borrowed(&self.patterns),
            Cow::Borrowed(&self.captures),
        )
    }

    pub fn from_parts(
        pattern: Regex,
        patterns: Vec<Span<Cow<'a, str>>>,
        captures: Vec<Vec<ValueType>>,
    ) -> Self {
        Self {
            patterns,
            pattern,
            captures,
        }
    }

    pub fn pattern_len(&self) -> usize {
        self.patterns.len()
    }
}
impl<'a> Spanned for RegexSetMatcher<'a> {
    fn span(&self) -> SourceSpan {
        let start = self.patterns.iter().map(|p| p.start()).min().unwrap();
        let end = self.patterns.iter().map(|p| p.end()).max().unwrap();
        SourceSpan::from(start..end)
    }
}
impl<'a> MatcherMut for RegexSetMatcher<'a> {
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
impl<'a> Matcher for RegexSetMatcher<'a> {
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
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
                        pattern_span,
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
                pattern_span,
                pattern_id,
                captures: capture_infos,
            }))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span(),
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}
