use crate::{
    ast::{CheckLine, CheckPattern},
    common::*,
    pattern::PatternPrefix,
};

use super::{MatchAny, RegexSetMatcher, SubstringSetBuilder, SubstringSetMatcher};

#[derive(Debug)]
pub enum MatchAll<'a> {
    Literal(SubstringSetMatcher<'a>),
    Regex(RegexSetMatcher<'a>),
    Smart {
        searcher: MatchAny<'a>,
        prefixes: Vec<PatternPrefix<'a>>,
        patterns: Vec<Vec<Pattern<'a>>>,
    },
}
impl<'a> MatchAll<'a> {
    pub fn pattern_len(&self) -> usize {
        match self {
            Self::Literal(ref matcher) => matcher.pattern_len(),
            Self::Regex(ref matcher) => matcher.pattern_len(),
            Self::Smart { patterns, .. } => patterns.iter().map(|ps| ps.len()).sum(),
        }
    }

    pub fn compile(
        unordered: Vec<CheckLine<'a>>,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum PatternSetType {
            Unknown,
            Literal,
            Regex,
            Mixed,
        }

        let set_type =
            unordered
                .iter()
                .fold(PatternSetType::Unknown, |acc, line| match line.pattern {
                    CheckPattern::Literal(_) => match acc {
                        PatternSetType::Literal | PatternSetType::Regex | PatternSetType::Mixed => {
                            acc
                        }
                        PatternSetType::Unknown => PatternSetType::Literal,
                    },
                    CheckPattern::Regex(_) => match acc {
                        PatternSetType::Regex | PatternSetType::Mixed => acc,
                        PatternSetType::Literal | PatternSetType::Unknown => PatternSetType::Regex,
                    },
                    CheckPattern::Match(_) => match acc {
                        PatternSetType::Mixed => acc,
                        PatternSetType::Regex | PatternSetType::Literal => PatternSetType::Mixed,
                        PatternSetType::Unknown => PatternSetType::Mixed,
                    },
                    CheckPattern::Empty(_) => acc,
                });

        match set_type {
            PatternSetType::Literal => {
                let mut literals = unordered
                    .into_iter()
                    .map(|line| match line.pattern {
                        CheckPattern::Literal(s) => s.map(Cow::Borrowed),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                // Strip out duplicate substrings
                literals.sort();
                literals.dedup();
                SubstringSetBuilder::new_with_patterns(literals)
                    .build()
                    .map(MatchAll::Literal)
            }
            PatternSetType::Regex => {
                let mut patterns = unordered
                    .into_iter()
                    .map(|line| match line.pattern {
                        CheckPattern::Literal(s) => {
                            Span::new(s.span(), Cow::Owned(regex::escape(s.as_ref())))
                        }
                        CheckPattern::Regex(s) => s.map(Cow::Borrowed),
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                // Strip out duplicate patterns
                patterns.sort();
                patterns.dedup();
                RegexSetMatcher::new(patterns).map(MatchAll::Regex)
            }
            PatternSetType::Mixed => {
                let mut all_known = true;
                let mut all_literal = true;
                let mut prefixes = Vec::<PatternPrefix<'a>>::with_capacity(unordered.len());
                let mut patterns = Vec::<Vec<Pattern<'a>>>::with_capacity(unordered.len());
                for (id, mut pattern) in unordered.into_iter().enumerate() {
                    match pattern.pattern.pop_prefix() {
                        None => {
                            all_known = false;
                            all_literal = false;
                            prefixes.push(PatternPrefix::Dynamic { id });
                            patterns.push(vec![Pattern::compile(pattern.pattern, interner)?]);
                        }
                        Some((prefix, true)) => {
                            if let Some(canonical_prefix_id) = prefixes.iter().find_map(|p| {
                                if p.is_duplicate_prefix(&prefix) {
                                    Some(p.id())
                                } else {
                                    None
                                }
                            }) {
                                prefixes.push(PatternPrefix::Duplicate {
                                    id,
                                    canonical: canonical_prefix_id,
                                });
                                patterns[canonical_prefix_id]
                                    .push(Pattern::compile(pattern.pattern, interner)?);
                            } else {
                                prefixes.push(PatternPrefix::Literal { id, prefix });
                                patterns.push(vec![Pattern::compile(pattern.pattern, interner)?]);
                            }
                        }
                        Some((prefix, false)) => {
                            all_literal = false;
                            if let Some(canonical_prefix_id) = prefixes.iter().find_map(|p| {
                                if p.is_duplicate_prefix(&prefix) {
                                    Some(p.id())
                                } else {
                                    None
                                }
                            }) {
                                prefixes.push(PatternPrefix::Duplicate {
                                    id,
                                    canonical: canonical_prefix_id,
                                });
                                patterns[canonical_prefix_id]
                                    .push(Pattern::compile(pattern.pattern, interner)?);
                            } else {
                                prefixes.push(PatternPrefix::Regex { id, prefix });
                                patterns.push(vec![Pattern::compile(pattern.pattern, interner)?]);
                            }
                        }
                    }
                }

                if all_known {
                    let searcher = if all_literal {
                        let mut builder = SubstringSetBuilder::new_with_patterns(vec![]);
                        builder.with_patterns(
                            prefixes.iter().filter_map(|prefix| prefix.clone_prefix()),
                        );
                        MatchAny::Literal(builder.build()?)
                    } else {
                        let prefix_patterns = prefixes
                            .iter()
                            .filter_map(|prefix| match prefix {
                                PatternPrefix::Literal { ref prefix, .. } => Some(Span::new(
                                    prefix.span(),
                                    Cow::Owned(regex::escape(prefix)),
                                )),
                                PatternPrefix::Regex { ref prefix, .. } => Some(prefix.clone()),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        MatchAny::Regex(RegexSetMatcher::new(prefix_patterns)?)
                    };

                    // We now have a single vector that can be used to dispatch a search
                    Ok(Self::Smart {
                        searcher,
                        prefixes,
                        patterns,
                    })
                } else {
                    todo!()
                }
            }
            PatternSetType::Unknown => panic!("invalid empty pattern set"),
        }
    }
}
impl<'a> Spanned for MatchAll<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Literal(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            Self::Smart {
                ref prefixes,
                ref patterns,
                ..
            } => {
                let start = prefixes
                    .iter()
                    .zip(patterns.iter())
                    .map(|(prefix, patterns)| {
                        prefix
                            .span()
                            .map(|s| s.offset())
                            .or_else(|| patterns.iter().map(|p| p.start()).min())
                            .unwrap()
                    })
                    .min()
                    .unwrap();
                let end = prefixes
                    .iter()
                    .zip(patterns.iter())
                    .map(|(prefix, patterns)| {
                        prefix
                            .span()
                            .map(|s| s.offset())
                            .or_else(|| patterns.iter().map(|p| p.end()).max())
                            .unwrap()
                    })
                    .max()
                    .unwrap();
                SourceSpan::from(start..end)
            }
        }
    }
}
