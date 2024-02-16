use crate::{
    ast::{CheckLine, CheckPattern, Prefix},
    common::*,
    pattern::{matcher::AlwaysMatch, PatternPrefix},
};

use super::{RegexSetMatcher, SubstringSetMatcher};

#[derive(Debug)]
pub enum MatchAll<'a> {
    /// A matcher for a set of literal strings
    Literal(SubstringSetMatcher<'a>),
    /// A matcher for a set of regular expressions
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

impl<'a> MatchAll<'a> {
    pub fn compile(
        mut unordered: Vec<CheckLine<'a>>,
        config: &Config,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
        use std::collections::BTreeMap;

        let raw_prefixes = unordered
            .iter_mut()
            .map(|line| {
                // Compact the patterns as much as possible first
                line.pattern.compact(interner);
                line.pattern.pop_prefix()
            })
            .collect::<SmallVec<[_; 4]>>();
        let mut unique_prefixes = BTreeMap::<_, usize>::default();
        let mut prefixes = Vec::with_capacity(raw_prefixes.len());
        let mut suffixes = Vec::<Vec<Pattern<'a>>>::with_capacity(raw_prefixes.len());

        for (id, prefix) in raw_prefixes.iter().enumerate() {
            let span = prefix.span();
            if let Some(canonical_prefix_id) = unique_prefixes.get(prefix).copied() {
                let pattern =
                    core::mem::replace(&mut unordered[id].pattern, CheckPattern::Empty(span));
                if !pattern.is_empty() {
                    suffixes[canonical_prefix_id]
                        .push(Pattern::compile(pattern, config, interner)?);
                } else {
                    suffixes[canonical_prefix_id].push(Pattern::Empty(AlwaysMatch::new(span)));
                }
                continue;
            }
            let pattern_prefix = match &prefix {
                Prefix::Literal(prefix) => PatternPrefix::Literal {
                    id,
                    prefix: prefix.clone(),
                },
                Prefix::Substring(prefix) => PatternPrefix::Substring {
                    id,
                    prefix: prefix.clone(),
                },
                Prefix::Regex(prefix) => PatternPrefix::Regex {
                    id,
                    prefix: prefix.clone(),
                },
                Prefix::Match(prefix) => PatternPrefix::Dynamic {
                    id,
                    prefix: prefix.clone(),
                },
                Prefix::Empty(span) => {
                    let diag = Diag::new(
                        "unexpected empty pattern encountered during pattern compilation",
                    )
                    .with_label(Label::new(*span, "pattern defined here"))
                    .with_help("empty patterns are only valid in CHECK-NOT directives");
                    return Err(Report::from(diag));
                }
            };
            unique_prefixes.insert(prefix.clone(), id);
            prefixes.push(pattern_prefix);
            let pattern = core::mem::replace(&mut unordered[id].pattern, CheckPattern::Empty(span));
            if !pattern.is_empty() {
                suffixes.push(vec![Pattern::compile(pattern, config, interner)?]);
            } else {
                suffixes.push(vec![Pattern::Empty(AlwaysMatch::new(span))]);
            }
        }

        // We've de-duplicated the prefixes, and combined all suffixes for shared prefixes,
        // next, we need to determine what searcher to use for prefix matching, which depends
        // on the composition of the patterns

        // A single stage match means that each prefix is unique, and that the prefix represents the entire pattern
        let is_single_stage = suffixes.iter().all(|patterns| {
            patterns.is_empty() || matches!(patterns.as_slice(), [Pattern::Empty(_)])
        });

        // Literal patterns are always single stage, and can be performed with a simple substring set matcher
        let is_literal = prefixes
            .iter()
            .all(|prefix| matches!(prefix, PatternPrefix::Literal { .. }));
        if is_literal {
            assert!(is_single_stage);
            // We can use a simple substring searcher in this case
            return SubstringSetMatcher::new(
                prefixes
                    .into_iter()
                    .filter_map(PatternPrefix::into_str)
                    .collect(),
                config,
            )
            .map(MatchAll::Literal);
        }

        // Check if we have both literal+substring patterns, which require a slightly different strategy
        let is_substring = prefixes.iter().all(|prefix| {
            matches!(
                prefix,
                PatternPrefix::Literal { .. } | PatternPrefix::Substring { .. }
            )
        });
        if is_substring {
            // By definition this must be a multi-stage match
            assert!(!is_single_stage);

            let searcher = SubstringSetMatcher::new(
                prefixes
                    .into_iter()
                    .filter_map(PatternPrefix::into_str)
                    .collect(),
                config,
            )?;

            return Ok(Self::SubstringPrefix {
                prefixes: searcher,
                suffixes,
            });
        }

        // If all patterns are regular expressions, or can be safely converted to one, so we can
        // use a regex searcher for at least the prefixes, if not all of the patterns
        let is_regex = prefixes.iter().all(PatternPrefix::is_regex_compatible);
        // Single-stage regular expression matches can be performed with a single matcher
        if is_regex {
            if is_single_stage {
                return RegexSetMatcher::new(
                    prefixes
                        .into_iter()
                        .filter_map(PatternPrefix::into_regex_pattern)
                        .collect(),
                    config,
                    interner,
                )
                .map(MatchAll::Regex);
            }

            let searcher = RegexSetMatcher::new(
                prefixes
                    .into_iter()
                    .filter_map(PatternPrefix::into_regex_pattern)
                    .collect(),
                config,
                interner,
            )?;
            return Ok(Self::RegexPrefix {
                prefixes: searcher,
                suffixes,
            });
        }

        // If we reach here, it's because at least one prefix contains a match/substitution block
        // that cannot be easily grouped with other prefixes. In such cases we evaluate the search
        // entirely manually rather than delegating parts to other searchers
        let mut prefix_patterns = Vec::with_capacity(prefixes.len());
        for prefix in prefixes.into_iter() {
            prefix_patterns.push(Pattern::from_prefix(prefix, config, interner)?);
        }

        Ok(Self::AnyPrefix {
            prefixes: prefix_patterns,
            suffixes,
        })
    }
}
impl<'a> MatchAll<'a> {
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
                    .map(|(i, p)| (i, p.span().start()))
                    .min_by_key(|&(_, s)| s)
                    .unwrap();
                let (offset, end) = suffixes[prefix_id]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span().end()))
                    .min_by_key(|&(_, e)| e)
                    .unwrap();
                Span::new(SourceSpan::from(start..end), prefix_id + offset)
            }
            Self::RegexPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let (first_prefix_span, first_prefix) = prefixes.first_pattern().into_parts();
                let start = first_prefix_span.start();
                let (offset, end) = suffixes[first_prefix]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span().end()))
                    .min_by_key(|&(_, end)| end)
                    .unwrap();
                Span::new(SourceSpan::from(start..end), first_prefix + offset)
            }
            Self::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let (first_prefix_span, first_prefix) = prefixes.first_pattern().into_parts();
                let start = first_prefix_span.start();
                let (offset, end) = suffixes[first_prefix]
                    .iter()
                    .enumerate()
                    .map(|(offset, p)| (offset, p.span().end()))
                    .min_by_key(|&(_, end)| end)
                    .unwrap();
                Span::new(SourceSpan::from(start..end), first_prefix + offset)
            }
        }
    }

    pub fn first_pattern_span(&self) -> SourceSpan {
        self.first_pattern().span()
    }
}
impl<'a> Spanned for MatchAll<'a> {
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
                    .map(|prefix| prefix.span().start())
                    .min()
                    .unwrap();
                let end = prefixes
                    .iter()
                    .zip(suffixes.iter())
                    .map(|(prefix, suffixes)| {
                        suffixes
                            .iter()
                            .map(|p| p.span().end())
                            .max()
                            .unwrap_or(prefix.span().end())
                    })
                    .max()
                    .unwrap();
                SourceSpan::from(start..end)
            }
            Self::RegexPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let prefix_span = prefixes.span();
                let start = prefix_span.start();
                let prefix_end = prefix_span.end();
                let end = core::cmp::max(
                    prefix_end,
                    suffixes
                        .iter()
                        .map(|suffixes| {
                            suffixes
                                .iter()
                                .map(|p| p.span().end())
                                .max()
                                .unwrap_or(prefix_end)
                        })
                        .max()
                        .unwrap(),
                );
                SourceSpan::from(start..end)
            }
            Self::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let prefix_span = prefixes.span();
                let start = prefix_span.start();
                let prefix_end = prefix_span.end();
                let end = core::cmp::max(
                    prefix_end,
                    suffixes
                        .iter()
                        .map(|suffixes| {
                            suffixes
                                .iter()
                                .map(|p| p.span().end())
                                .max()
                                .unwrap_or(prefix_end)
                        })
                        .max()
                        .unwrap(),
                );
                SourceSpan::from(start..end)
            }
        }
    }
}
