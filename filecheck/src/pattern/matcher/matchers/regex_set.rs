use regex_automata::{
    Anchored, MatchKind, PatternID,
    dfa::{self, Automaton, OverlappingState, StartKind, dense, onepass},
    meta,
    nfa::thompson,
    util::{
        captures::{Captures, GroupInfo},
        syntax,
    },
};

use crate::{
    ast::{Capture, RegexPattern},
    common::*,
    pattern::{matcher::regex, search::Input as _},
};

#[derive(Default, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum CapturingRegex {
    #[default]
    None,
    Onepass {
        re: onepass::DFA,
        cache: onepass::Cache,
    },
    Default {
        re: meta::Regex,
        cache: meta::Cache,
    },
}

struct Search<'input> {
    crlf: bool,
    forward: bool,
    input: regex_automata::Input<'input>,
    reverse_input: regex_automata::Input<'input>,
    last_match_end: Option<usize>,
    captures: Captures,
    overlapping: OverlappingState,
    overlapping_reverse: OverlappingState,
}
impl<'input> Search<'input> {
    fn start(input: Input<'input>, captures: Captures) -> Self {
        let crlf = input.is_crlf();
        let input: regex_automata::Input<'input> = input.into();
        let reverse_input = input.clone().earliest(false);
        let overlapping = OverlappingState::start();
        let overlapping_reverse = OverlappingState::start();
        Self {
            crlf,
            forward: true,
            input,
            reverse_input,
            last_match_end: None,
            overlapping,
            overlapping_reverse,
            captures,
        }
    }
}

pub struct RegexSetSearcher<'a, 'input, A = dense::DFA<Vec<u32>>> {
    search: Search<'input>,
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The compiled regex which will be used to search the input buffer
    regex: dfa::regex::Regex<A>,
    /// The regex used to obtain capturing groups, if there are any
    capturing_regex: CapturingRegex,
    /// Metadata about captures in the given patterns
    ///
    /// Each pattern gets its own vector of capture info, since
    /// there is no requirement that all patterns have the same
    /// number or type of captures
    capture_types: Vec<Vec<Capture>>,
}
impl<'a, 'input> RegexSetSearcher<'a, 'input> {
    pub fn new(
        input: Input<'input>,
        patterns: Vec<RegexPattern<'a>>,
        config: &Config,
    ) -> DiagResult<Self> {
        let start_kind = if input.is_anchored() {
            StartKind::Anchored
        } else {
            StartKind::Unanchored
        };
        Ok(
            RegexSetMatcher::new_with_start_kind(start_kind, patterns, config)?
                .into_searcher(input),
        )
    }
}
impl<'a, 'input, A: Automaton> RegexSetSearcher<'a, 'input, A> {
    pub fn from_matcher(matcher: RegexSetMatcher<'a, A>, input: Input<'input>) -> Self {
        let captures = matcher.captures;
        let search = Search::start(input, captures);
        Self {
            search,
            patterns: matcher.patterns,
            regex: matcher.regex,
            capturing_regex: matcher.capturing_regex,
            capture_types: matcher.capture_types,
        }
    }
}

impl<'a, 'input, A: Automaton + Clone> RegexSetSearcher<'a, 'input, A> {
    pub fn from_matcher_ref(matcher: &RegexSetMatcher<'a, A>, input: Input<'input>) -> Self {
        let captures = matcher.captures.clone();
        let search = Search::start(input, captures);
        Self {
            search,
            patterns: matcher.patterns.clone(),
            regex: matcher.regex.clone(),
            capturing_regex: matcher.capturing_regex.clone(),
            capture_types: matcher.capture_types.clone(),
        }
    }
}
impl<'a, 'input, A> fmt::Debug for RegexSetSearcher<'a, 'input, A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexSetSearcher")
            .field("patterns", &self.patterns)
            .field("capture_types", &self.search.captures)
            .field("crlf", &self.search.crlf)
            .field("forward", &self.search.forward)
            .field("last_match_end", &self.search.last_match_end)
            .finish()
    }
}
impl<'a, 'input, A> Spanned for RegexSetSearcher<'a, 'input, A> {
    fn span(&self) -> SourceSpan {
        let start = self
            .patterns
            .iter()
            .map(|p| p.span())
            .min_by_key(|span| span.start())
            .unwrap();
        let end = self
            .patterns
            .iter()
            .map(|p| p.span())
            .max_by_key(|span| span.end())
            .unwrap();
        SourceSpan::from_range_unchecked(
            start.source_id(),
            start.start().to_usize()..end.end().to_usize(),
        )
    }
}

pub struct RegexSetMatcher<'a, A = dense::DFA<Vec<u32>>> {
    /// The set of raw input patterns from which
    /// this matcher was constructed
    patterns: Vec<Span<Cow<'a, str>>>,
    /// The compiled regex which will be used to search the input buffer
    regex: dfa::regex::Regex<A>,
    /// The regex used to obtain capturing groups, if there are any
    capturing_regex: CapturingRegex,
    /// Captures storage
    captures: Captures,
    /// Metadata about captures in the given patterns
    ///
    /// Each pattern gets its own vector of capture info, since
    /// there is no requirement that all patterns have the same
    /// number or type of captures
    capture_types: Vec<Vec<Capture>>,
}
impl<'a> RegexSetMatcher<'a> {
    pub fn new(patterns: Vec<RegexPattern<'a>>, config: &Config) -> DiagResult<Self> {
        Self::new_with_start_kind(StartKind::Both, patterns, config)
    }

    pub fn new_with_start_kind(
        start_kind: StartKind,
        patterns: Vec<RegexPattern<'a>>,
        config: &Config,
    ) -> DiagResult<Self> {
        let regex = dfa::regex::Regex::builder()
            .dense(
                dense::Config::new()
                    .match_kind(MatchKind::All)
                    .start_kind(start_kind)
                    .starts_for_each_pattern(true),
            )
            .syntax(
                syntax::Config::new()
                    .multi_line(true)
                    .case_insensitive(config.options.ignore_case),
            )
            .build_many(&patterns)
            .map_err(|error| {
                regex::build_error_to_diagnostic(error, patterns.len(), |id| patterns[id].span())
            })?;

        let has_captures = patterns.iter().any(|p| !p.captures.is_empty());
        let (capturing_regex, captures) = if !has_captures {
            (CapturingRegex::None, Captures::empty(GroupInfo::empty()))
        } else {
            onepass::DFA::builder()
                .syntax(
                    syntax::Config::new()
                        .utf8(false)
                        .multi_line(true)
                        .case_insensitive(config.options.ignore_case),
                )
                .thompson(thompson::Config::new().utf8(false))
                .configure(onepass::Config::new().starts_for_each_pattern(true))
                .build_many(&patterns)
                .map_or_else(
                    |_| {
                        let re = Regex::builder()
                            .configure(Regex::config().match_kind(MatchKind::All))
                            .syntax(
                                syntax::Config::new()
                                    .multi_line(true)
                                    .case_insensitive(config.options.ignore_case),
                            )
                            .build_many(&patterns)
                            .unwrap();
                        let cache = re.create_cache();
                        let captures = re.create_captures();
                        (CapturingRegex::Default { re, cache }, captures)
                    },
                    |re| {
                        let cache = re.create_cache();
                        let captures = re.create_captures();
                        (CapturingRegex::Onepass { re, cache }, captures)
                    },
                )
        };

        // Compute capture group information
        let mut capture_types = vec![vec![]; patterns.len()];
        let mut strings = Vec::with_capacity(patterns.len());
        let groups = captures.group_info();
        for (
            i,
            RegexPattern {
                pattern,
                captures: pattern_captures,
            },
        ) in patterns.into_iter().enumerate()
        {
            let span = pattern.span();
            strings.push(pattern);
            let pid = PatternID::new_unchecked(i);
            let num_captures = groups.group_len(pid);
            capture_types[i].resize(num_captures, Capture::Ignore(span));
            for capture in pattern_captures.into_iter() {
                if let Capture::Ignore(_) = capture {
                    continue;
                }
                if let Some(group_name) = capture.group_name() {
                    let group_id = groups
                        .to_index(pid, group_name.as_str())
                        .unwrap_or_else(|| {
                            panic!("expected group for capture of '{group_name}' in pattern {i}")
                        });
                    capture_types[i][group_id] = capture;
                } else {
                    assert_eq!(
                        &capture_types[i][0],
                        &Capture::Ignore(span),
                        "{capture:?} would overwrite a previous implicit capture group in pattern {i}"
                    );
                    capture_types[i][0] = capture;
                }
            }
        }

        Ok(Self {
            patterns: strings,
            regex,
            capturing_regex,
            captures,
            capture_types,
        })
    }

    pub fn patterns_len(&self) -> usize {
        self.patterns.len()
    }

    pub fn first_pattern(&self) -> Span<usize> {
        self.patterns
            .iter()
            .enumerate()
            .map(|(i, p)| Span::new(p.span(), i))
            .min_by_key(|span| span.span().start())
            .unwrap()
    }

    pub fn first_pattern_span(&self) -> SourceSpan {
        self.first_pattern().span()
    }
}
impl<'a, A: Automaton + Clone> RegexSetMatcher<'a, A> {
    pub fn search<'input>(&self, input: Input<'input>) -> RegexSetSearcher<'a, 'input, A> {
        RegexSetSearcher::from_matcher_ref(self, input)
    }
}
impl<'a, A: Automaton> RegexSetMatcher<'a, A> {
    #[inline]
    pub fn into_searcher<'input>(self, input: Input<'input>) -> RegexSetSearcher<'a, 'input, A> {
        RegexSetSearcher::from_matcher(self, input)
    }
}
impl<'a, A> fmt::Debug for RegexSetMatcher<'a, A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("RegexSetMatcher")
            .field("patterns", &self.patterns)
            .field("captures", &self.captures)
            .field("capture_types", &self.capture_types)
            .finish()
    }
}
impl<'a, A> Spanned for RegexSetMatcher<'a, A> {
    fn span(&self) -> SourceSpan {
        let start = self
            .patterns
            .iter()
            .map(|p| p.span())
            .min_by_key(|span| span.start())
            .unwrap();
        let end = self
            .patterns
            .iter()
            .map(|p| p.span())
            .max_by_key(|span| span.end())
            .unwrap();
        SourceSpan::from_range_unchecked(
            start.source_id(),
            start.start().to_usize()..end.end().to_usize(),
        )
    }
}
impl<'a, A: Automaton + Clone> MatcherMut for RegexSetMatcher<'a, A> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut searcher = self.search(input);
        let matched = searcher.try_match_next(context)?;
        matched.bind_captures_in(context);
        Ok(matched)
    }
}
impl<'a, 'input, A> PatternSearcher<'input> for RegexSetSearcher<'a, 'input, A>
where
    A: Automaton,
{
    type Input = regex_automata::Input<'input>;
    type PatternID = PatternID;

    fn input(&self) -> &Self::Input {
        &self.search.input
    }
    fn last_match_end(&self) -> Option<usize> {
        self.search.last_match_end
    }
    fn set_last_match_end(&mut self, end: usize) {
        self.search.last_match_end = Some(end);
        self.search.input.set_start(end);
    }
    fn patterns_len(&self) -> usize {
        self.patterns.len()
    }
    fn pattern_span(&self, id: Self::PatternID) -> SourceSpan {
        self.patterns[id.as_usize()].span()
    }
    fn try_match_next<'context, C>(&mut self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let (fwd_dfa, rev_dfa) = (self.regex.forward(), self.regex.reverse());
        let matched;
        let mut last_end = self
            .search
            .last_match_end
            .unwrap_or(self.search.input.start());
        loop {
            if self.search.forward {
                if let Some((pattern_id, end)) = {
                    fwd_dfa
                        .try_search_overlapping_fwd(
                            &self.search.input,
                            &mut self.search.overlapping,
                        )
                        .expect("match error");
                    self.search
                        .overlapping
                        .get_match()
                        .map(|hm| (hm.pattern(), hm.offset()))
                } {
                    last_end = end;
                    self.search
                        .reverse_input
                        .set_anchored(Anchored::Pattern(pattern_id));
                    self.search
                        .reverse_input
                        .set_range(self.search.input.start()..end);
                    self.search.forward = false;
                    self.search.overlapping_reverse = OverlappingState::start();
                    continue;
                } else {
                    matched = None;
                    break;
                }
            } else if let Some((pattern_id, start)) = {
                rev_dfa
                    .try_search_overlapping_rev(
                        &self.search.reverse_input,
                        &mut self.search.overlapping_reverse,
                    )
                    .expect("match error");
                self.search
                    .overlapping_reverse
                    .get_match()
                    .map(|hm| (hm.pattern(), hm.offset()))
            } {
                if start == last_end && !self.search.input.is_char_boundary(last_end) {
                    continue;
                }
                self.search.last_match_end = Some(last_end);
                matched = Some(regex_automata::Match::new(pattern_id, start..last_end));
                break;
            } else {
                self.search.forward = true;
            }
        }

        if let Some(matched) = matched {
            self.search.captures.clear();
            let pattern_id = matched.pattern();
            match self.capturing_regex {
                CapturingRegex::None => {
                    let overall_span = SourceSpan::from_range_unchecked(
                        self.search.input.source_id(),
                        matched.range(),
                    );
                    let pattern_index = pattern_id.as_usize();
                    let pattern_span = self.patterns[pattern_index].span();
                    Ok(MatchResult::ok(MatchInfo {
                        span: overall_span,
                        pattern_span,
                        pattern_id: pattern_index,
                        captures: vec![],
                    }))
                }
                CapturingRegex::Default {
                    ref re,
                    ref mut cache,
                } => {
                    let input = self
                        .search
                        .input
                        .clone()
                        .anchored(Anchored::Pattern(pattern_id))
                        .range(matched.range());
                    re.search_captures_with(cache, &input, &mut self.search.captures);
                    if let Some(matched) = self.search.captures.get_match() {
                        extract_captures_from_match(
                            matched,
                            &self.search,
                            &self.patterns,
                            &self.capture_types,
                            context,
                        )
                    } else {
                        let span = SourceSpan::from_range_unchecked(
                            self.search.input.source_id(),
                            matched.range(),
                        );
                        let pattern_span = self.patterns[pattern_id.as_usize()].span();
                        let error = CheckFailedError::MatchError {
                            span,
                            input_file: context.input_file(),
                            labels: vec![RelatedLabel::note(Label::at(pattern_span), context.source_file(pattern_span.source_id()).unwrap())],
                            help: Some("meta regex searcher failed to match the input even though an initial DFA pass found a match".to_string()),
                        };
                        Err(Report::new(error))
                    }
                }
                CapturingRegex::Onepass {
                    ref re,
                    ref mut cache,
                } => {
                    let input = self
                        .search
                        .input
                        .clone()
                        .anchored(Anchored::Pattern(pattern_id))
                        .range(matched.range());
                    re.captures(cache, input, &mut self.search.captures);
                    if let Some(matched) = self.search.captures.get_match() {
                        extract_captures_from_match(
                            matched,
                            &self.search,
                            &self.patterns,
                            &self.capture_types,
                            context,
                        )
                    } else {
                        let span = SourceSpan::from_range_unchecked(
                            self.search.input.source_id(),
                            matched.range(),
                        );
                        let pattern_span = self.patterns[pattern_id.as_usize()].span();
                        let error = CheckFailedError::MatchError {
                            span,
                            input_file: context.input_file(),
                            labels: vec![RelatedLabel::note(Label::at(pattern_span), context.source_file(pattern_span.source_id()).unwrap())],
                            help: Some("onepass regex searcher failed to match the input even though an initial DFA pass found a match".to_string()),
                        };
                        Err(Report::new(error))
                    }
                }
            }
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span(),
                    match_file: context.source_file(self.span().source_id()).unwrap(),
                    note: None,
                },
            ))
        }
    }
}

fn extract_captures_from_match<'a, 'input, 'context, C>(
    matched: regex_automata::Match,
    search: &Search<'_>,
    patterns: &[Span<Cow<'a, str>>],
    capture_types: &[Vec<Capture>],
    context: &C,
) -> DiagResult<MatchResult<'input>>
where
    C: Context<'input, 'context> + ?Sized,
{
    let pattern_id = matched.pattern();
    let pattern_index = pattern_id.as_usize();
    let pattern_span = patterns[pattern_index].span();
    let overall_span = SourceSpan::from_range_unchecked(search.input.source_id(), matched.range());
    let mut capture_infos = Vec::with_capacity(search.captures.group_len());
    for (index, (maybe_capture_span, capture)) in search
        .captures
        .iter()
        .zip(capture_types[pattern_id].iter().copied())
        .enumerate()
    {
        if let Some(capture_span) = maybe_capture_span {
            let input = context.search();
            let captured = input.as_str(capture_span.range());
            let capture_span =
                SourceSpan::from_range_unchecked(search.input.source_id(), capture_span.range());
            let result = regex::try_convert_capture_to_type(
                pattern_id,
                index,
                pattern_span,
                overall_span,
                Span::new(capture_span, captured),
                capture,
                &search.captures,
                context,
            );
            match result {
                Ok(capture_info) => {
                    capture_infos.push(capture_info);
                }
                Err(error) => {
                    return Ok(MatchResult::failed(error));
                }
            }
        }
    }
    Ok(MatchResult::ok(MatchInfo {
        span: overall_span,
        pattern_span,
        pattern_id: pattern_index,
        captures: capture_infos,
    }))
}
