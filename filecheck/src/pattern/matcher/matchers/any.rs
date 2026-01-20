use crate::common::*;

use super::{MatchAll, RegexSetMatcher, SubstringSetMatcher};

#[derive(Debug)]
pub enum MatchAny<'a> {
    /// Match a set of literal strings
    Literal(SubstringSetMatcher<'a>),
    /// Match a set of static regex patterns
    Regex(RegexSetMatcher<'a>),
    /// A highly dynamic pattern matcher, but less efficient, as
    /// it is forced to perform some number of redundant searches
    /// of the input in order to check for all patterns. This is
    /// the only searcher which can match multiple patterns with
    /// match/blocks substitutions in at least one of the pattern
    /// prefixes
    AnyPrefix {
        /// The prefix patterns to search for
        prefixes: Vec<Pattern<'a>>,
        /// The suffix patterns corresponding to each prefix
        suffixes: Vec<Vec<Pattern<'a>>>,
    },
    /// An optimized form of `AnyPrefix` when all of the prefixes
    /// can be evaluated as a set of regular expressions.
    RegexPrefix {
        prefixes: RegexSetMatcher<'a>,
        suffixes: Vec<Vec<Pattern<'a>>>,
    },
    /// An optimized form of `AnyPrefix` when all of the prefixes
    /// can be evaluated as a set of substring literals.
    SubstringPrefix {
        prefixes: SubstringSetMatcher<'a>,
        suffixes: Vec<Vec<Pattern<'a>>>,
    },
}
impl<'a> From<MatchAll<'a>> for MatchAny<'a> {
    fn from(match_all: MatchAll<'a>) -> Self {
        match match_all {
            MatchAll::Literal(lit) => Self::Literal(lit),
            MatchAll::Regex(re) => Self::Regex(re),
            MatchAll::AnyPrefix { prefixes, suffixes } => Self::AnyPrefix { prefixes, suffixes },
            MatchAll::SubstringPrefix { prefixes, suffixes } => {
                Self::SubstringPrefix { prefixes, suffixes }
            }
            MatchAll::RegexPrefix { prefixes, suffixes } => {
                Self::RegexPrefix { prefixes, suffixes }
            }
        }
    }
}
impl<'a> MatchAny<'a> {
    pub fn pattern_len(&self) -> usize {
        match self {
            Self::Literal(ref matcher) => matcher.pattern_len(),
            Self::Regex(ref matcher) => matcher.patterns_len(),
            Self::AnyPrefix { suffixes, .. }
            | Self::RegexPrefix { suffixes, .. }
            | Self::SubstringPrefix { suffixes, .. } => suffixes.iter().map(|ps| ps.len()).sum(),
        }
    }

    pub fn first_pattern(&self) -> Span<usize> {
        match self {
            Self::Literal(matcher) => matcher.first_pattern(),
            Self::Regex(matcher) => matcher.first_pattern(),
            Self::AnyPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let (prefix_id, start) = prefixes
                    .iter()
                    .enumerate()
                    .map(|(i, p)| (i, p.span()))
                    .min_by_key(|&(_, s)| s.start())
                    .unwrap();
                let (offset, end) = suffixes[prefix_id]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span()))
                    .max_by_key(|&(_, e)| e.end())
                    .unwrap();
                Span::new(
                    SourceSpan::from_range_unchecked(
                        start.source_id(),
                        start.start().to_usize()..end.end().to_usize(),
                    ),
                    prefix_id + offset,
                )
            }
            Self::RegexPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let (first_prefix_span, first_prefix) = prefixes.first_pattern().into_parts();
                let start = first_prefix_span.start().to_usize();
                let (offset, end) = suffixes[first_prefix]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span()))
                    .max_by_key(|&(_, end)| end.end())
                    .unwrap();
                Span::new(
                    SourceSpan::from_range_unchecked(
                        first_prefix_span.source_id(),
                        start..end.end().to_usize(),
                    ),
                    first_prefix + offset,
                )
            }
            Self::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let (first_prefix_span, first_prefix) = prefixes.first_pattern().into_parts();
                let start = first_prefix_span.start().to_usize();
                let (offset, end) = suffixes[first_prefix]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span()))
                    .max_by_key(|&(_, span)| span.end())
                    .unwrap();
                Span::new(
                    SourceSpan::from_range_unchecked(
                        first_prefix_span.source_id(),
                        start..end.end().to_usize(),
                    ),
                    first_prefix + offset,
                )
            }
        }
    }

    pub fn first_pattern_span(&self) -> SourceSpan {
        self.first_pattern().span()
    }
}
impl<'a> Spanned for MatchAny<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Literal(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            Self::AnyPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let start = prefixes
                    .iter()
                    .map(|prefix| prefix.span())
                    .min_by_key(|span| span.start())
                    .unwrap();
                let end = prefixes
                    .iter()
                    .zip(suffixes.iter())
                    .map(|(prefix, suffixes)| {
                        suffixes
                            .iter()
                            .map(|p| p.span())
                            .max_by_key(|span| span.end())
                            .unwrap_or(prefix.span())
                    })
                    .max_by_key(|span| span.end())
                    .unwrap();
                SourceSpan::from_range_unchecked(
                    start.source_id(),
                    start.start().to_usize()..end.end().to_usize(),
                )
            }
            Self::RegexPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let prefix_span = prefixes.span();
                let start = prefix_span.start().to_usize();
                let end = core::cmp::max(
                    prefix_span.end(),
                    suffixes
                        .iter()
                        .map(|suffixes| {
                            suffixes
                                .iter()
                                .map(|p| p.span())
                                .max_by_key(|span| span.end())
                                .unwrap_or(prefix_span)
                        })
                        .max_by_key(|span| span.end())
                        .unwrap()
                        .end(),
                );
                SourceSpan::from_range_unchecked(prefix_span.source_id(), start..end.to_usize())
            }
            Self::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let prefix_span = prefixes.span();
                let start = prefix_span.start().to_usize();
                let end = core::cmp::max(
                    prefix_span.end(),
                    suffixes
                        .iter()
                        .map(|suffixes| {
                            suffixes
                                .iter()
                                .map(|p| p.span())
                                .max_by_key(|span| span.end())
                                .unwrap_or(prefix_span)
                        })
                        .max_by_key(|span| span.end())
                        .unwrap()
                        .end(),
                );
                SourceSpan::from_range_unchecked(prefix_span.source_id(), start..end.to_usize())
            }
        }
    }
}
impl<'a> MatcherMut for MatchAny<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        use crate::pattern::search::PatternSetSearcher;

        match self {
            Self::Literal(ref matcher) => matcher.try_match_mut(input, context),
            Self::Regex(ref matcher) => matcher.try_match_mut(input, context),
            Self::AnyPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let searcher = PatternSetSearcher::new(input, prefixes)?;
                try_match_searcher(searcher, suffixes, input, context)
            }
            Self::RegexPrefix {
                ref prefixes,
                ref suffixes,
            } => try_match_searcher(prefixes.search(input), suffixes, input, context),
            Self::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => try_match_searcher(prefixes.search(input)?, suffixes, input, context),
        }
    }
}

fn try_match_searcher<'input, 'context, 'a, S, C>(
    mut searcher: S,
    suffixes: &[Vec<Pattern<'a>>],
    input: Input<'input>,
    context: &mut C,
) -> DiagResult<MatchResult<'input>>
where
    S: PatternSearcher<'input>,
    C: Context<'input, 'context> + ?Sized,
{
    loop {
        match searcher.try_match_next(context)? {
            MatchResult {
                info: Some(ref mut info),
                ty: MatchType::MatchFoundAndExpected,
            } => {
                let mut suffix_input = input;
                suffix_input.set_start(searcher.last_match_end().unwrap());
                suffix_input.set_anchored(true);
                let suffixes = &suffixes[info.pattern_id];
                if let Some(found) =
                    try_match_suffix(info, suffixes, info.pattern_id, suffix_input, context)?
                {
                    break Ok(found);
                }
            }
            result @ MatchResult { info: None, .. } => break Ok(result),
            _ => continue,
        }
    }
}

fn try_match_suffix<'input, 'context, 'a, C>(
    prefix_info: &mut MatchInfo<'input>,
    suffixes: &[Pattern<'a>],
    mut pattern_id: usize,
    input: Input<'input>,
    context: &mut C,
) -> DiagResult<Option<MatchResult<'input>>>
where
    C: Context<'input, 'context> + ?Sized,
{
    for suffix in suffixes.iter() {
        match suffix.try_match_mut(input, context)? {
            MatchResult {
                info: Some(mut suffix_info),
                ty: MatchType::MatchFoundAndExpected,
            } => {
                // Match found, we're done
                let mut captures = core::mem::take(&mut prefix_info.captures);
                captures.append(&mut suffix_info.captures);
                return Ok(Some(MatchResult::ok(MatchInfo {
                    pattern_id,
                    span: SourceSpan::new(
                        prefix_info.span.source_id(),
                        Range::new(prefix_info.span.start(), suffix_info.span.end()),
                    ),
                    captures,
                    ..suffix_info
                })));
            }
            MatchResult {
                info: Some(mut info),
                ty,
            } => {
                // We found a match, but then some error occurred, so let's propagate the error
                info.pattern_id = pattern_id;
                return Ok(Some(MatchResult {
                    info: Some(info),
                    ty,
                }));
            }
            _ => {
                // No match for this suffix
                pattern_id += 1;
            }
        }
    }

    Ok(None)
}
