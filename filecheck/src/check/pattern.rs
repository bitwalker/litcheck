use std::{borrow::Cow, fmt};

use litcheck::{
    diagnostics::{DiagResult, Report, SourceSpan, Span, Spanned},
    StringInterner,
};

use crate::check::*;

#[derive(Debug, thiserror::Error)]
#[error("invalid pattern identifier: out of range")]
pub struct InvalidPatternIdentifier;

pub trait PatternIdentifier: Sized + Copy + PartialEq + Eq + fmt::Debug {
    fn new(id: usize) -> Result<Self, InvalidPatternIdentifier>;
    fn new_unchecked(id: usize) -> Self;
    fn as_usize(&self) -> usize;
}
impl PatternIdentifier for regex_automata::PatternID {
    fn new(id: usize) -> Result<Self, InvalidPatternIdentifier> {
        regex_automata::PatternID::new(id).map_err(|_| InvalidPatternIdentifier)
    }
    fn new_unchecked(id: usize) -> Self {
        regex_automata::PatternID::new_unchecked(id)
    }
    fn as_usize(&self) -> usize {
        regex_automata::PatternID::as_usize(self)
    }
}
impl PatternIdentifier for aho_corasick::PatternID {
    fn new(id: usize) -> Result<Self, InvalidPatternIdentifier> {
        aho_corasick::PatternID::new(id).map_err(|_| InvalidPatternIdentifier)
    }
    fn new_unchecked(id: usize) -> Self {
        aho_corasick::PatternID::new_unchecked(id)
    }
    fn as_usize(&self) -> usize {
        aho_corasick::PatternID::as_usize(self)
    }
}

/// The compiled form of [CheckPattern]
#[derive(Debug)]
pub enum Pattern<'a> {
    /// This pattern always succeeds
    Empty(AlwaysMatch),
    /// A literal string that must occur somewhere in the input
    Substring(SubstringMatcher<'a>),
    /// A regular expression that must occur somewhere in the input
    Regex(RegexMatcher<'a>),
    /// A hybrid match expression that must occur somewhere in the input
    Smart(SmartMatcher<'a>),
    /// A matcher for pure ASCII whitespace patterns
    Whitespace(AsciiWhitespaceMatcher),
}
impl<'a> Pattern<'a> {
    pub fn into_matcher_mut(self) -> AnyMatcherMut<'a> {
        match self {
            Self::Empty(matcher) => Box::new(matcher),
            Self::Substring(matcher) => Box::new(matcher),
            Self::Regex(matcher) => Box::new(matcher),
            Self::Smart(matcher) => Box::new(matcher),
            Self::Whitespace(matcher) => Box::new(matcher),
        }
    }

    pub fn compile(pattern: CheckPattern<'a>, interner: &mut StringInterner) -> DiagResult<Self> {
        match pattern {
            CheckPattern::Literal(s) => {
                if s.is_empty() {
                    return Err(Report::from(InvalidCheckFileError::EmptyPattern(s.span())));
                }
                if s.trim().is_empty() {
                    if s.chars().all(|c| c.is_ascii_whitespace()) {
                        Ok(Pattern::Whitespace(AsciiWhitespaceMatcher::new(s.span())))
                    } else {
                        Ok(Pattern::Regex(RegexMatcher::new(Span::new(
                            s.span(),
                            Cow::Borrowed(r"\s+"),
                        ))?))
                    }
                } else {
                    Ok(Pattern::Substring(SubstringMatcher::new(
                        s.map(Cow::Borrowed),
                    )?))
                }
            }
            CheckPattern::Regex(s) => Ok(Pattern::Regex(RegexMatcher::new(s.map(Cow::Borrowed))?)),
            CheckPattern::Match(s) => {
                let (span, parts) = s.into_parts();
                let mut builder = SmartMatcher::build(span, interner);
                for part in parts.into_iter() {
                    match part {
                        CheckPatternPart::Literal(s) => {
                            builder.literal(s.map(Cow::Borrowed))?;
                        }
                        CheckPatternPart::Regex(s) => {
                            builder.regex(s.map(Cow::Borrowed))?;
                        }
                        CheckPatternPart::Match(Match::Substitution {
                            name,
                            pattern: None,
                            ..
                        }) => {
                            builder.substitution(Expr::Var(name));
                        }
                        CheckPatternPart::Match(Match::Substitution {
                            span,
                            name,
                            pattern: Some(pattern),
                        }) => {
                            builder.capture(
                                name.into_inner(),
                                Span::new(span, Cow::Borrowed(pattern.into_inner())),
                            )?;
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: None,
                            expr: None,
                            ..
                        }) => {
                            builder.numeric(span, format);
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            format,
                            capture: None,
                            expr: Some(expr),
                            ..
                        }) => {
                            builder.substitution_with_format(expr, ValueType::Number(format));
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: Some(name),
                            expr: None,
                            ..
                        }) => {
                            builder.capture_numeric(span, name.into_inner(), format);
                        }
                        CheckPatternPart::Match(Match::Numeric {
                            span,
                            format,
                            capture: Some(name),
                            constraint,
                            expr: Some(expr),
                        }) => {
                            builder.capture_numeric_with_constraint(
                                span,
                                name.into_inner(),
                                format,
                                constraint,
                                expr,
                            );
                        }
                    }
                }

                let pattern = builder.build();

                Ok(Pattern::Smart(pattern))
            }
            CheckPattern::Empty(span) => Ok(Pattern::Empty(AlwaysMatch::new(span))),
        }
    }

    pub fn compile_static(
        span: SourceSpan,
        pattern: CheckPattern<'a>,
    ) -> DiagResult<StaticMatcher<'a>> {
        match pattern {
            CheckPattern::Literal(lit) => Ok(StaticMatcher::Substring(SubstringMatcher::new(
                lit.map(Cow::Borrowed),
            )?)),
            CheckPattern::Regex(s) => Ok(StaticMatcher::Regex(RegexMatcher::new(
                s.map(Cow::Borrowed),
            )?)),
            CheckPattern::Match(parts) => {
                let var = parts
                    .iter()
                    .find(|p| matches!(p, CheckPatternPart::Match(_)))
                    .unwrap()
                    .span();
                Err(Report::new(InvalidCheckFileError::CheckLabelVariable {
                    line: span,
                    var: var.span(),
                }))
            }
            CheckPattern::Empty(_) => unreachable!(
                "{pattern:?} is only valid for CHECK-EMPTY, and is not an actual pattern"
            ),
        }
    }

    pub fn compile_literal(pattern: CheckPattern<'a>) -> DiagResult<Self> {
        match pattern {
            CheckPattern::Literal(lit) => Ok(Pattern::Substring(SubstringMatcher::new(
                lit.map(Cow::Borrowed),
            )?)),
            CheckPattern::Regex(_) | CheckPattern::Match(_) => {
                unreachable!("the lexer will never emit tokens for these non-terminals")
            }
            CheckPattern::Empty(_) => unreachable!(
                "{pattern:?} is only valid for CHECK-EMPTY, and is not an actual pattern"
            ),
        }
    }
}
impl<'a> MatcherMut for Pattern<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        match self {
            Self::Substring(ref matcher) => matcher.try_match(input, context),
            Self::Regex(ref matcher) => matcher.try_match(input, context),
            Self::Smart(ref matcher) => matcher.try_match_mut(input, context),
            Self::Whitespace(ref matcher) => matcher.try_match(input, context),
            Self::Empty(ref matcher) => matcher.try_match(input, context),
        }
    }
}
impl<'a> Spanned for Pattern<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Substring(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            Self::Smart(ref matcher) => matcher.span(),
            Self::Whitespace(ref matcher) => matcher.span(),
            Self::Empty(ref matcher) => matcher.span(),
        }
    }
}

#[derive(Debug)]
pub enum MatchAny<'a> {
    /// Match a set of literal strings
    Literal(SubstringSetMatcher<'a>),
    /// Match a set of static regex patterns
    Regex(RegexSetMatcher<'a>),
    ///// Match a set of hybrid patterns
    //Smart(SmartMatcherSet<'a>),
}
impl<'a> Spanned for MatchAny<'a> {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Literal(ref matcher) => matcher.span(),
            Self::Regex(ref matcher) => matcher.span(),
            //Self::Smart(ref matcher) => matcher.span(),
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
        match self {
            Self::Literal(ref matcher) => matcher.try_match_mut(input, context),
            Self::Regex(ref matcher) => matcher.try_match_mut(input, context),
        }
    }
}

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

#[derive(Debug)]
pub enum PatternPrefix<'a> {
    Literal {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    Regex {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    Dynamic {
        id: usize,
    },
    Duplicate {
        id: usize,
        canonical: usize,
    },
}
impl<'a> PatternPrefix<'a> {
    pub fn id(&self) -> usize {
        match self {
            Self::Literal { id, .. }
            | Self::Regex { id, .. }
            | Self::Dynamic { id, .. }
            | Self::Duplicate { id, .. } => *id,
        }
    }

    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.span()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn prefix(&self) -> Option<&str> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.as_ref()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn clone_prefix(&self) -> Option<Span<Cow<'a, str>>> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.clone()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn is_duplicate(&self) -> Option<usize> {
        match self {
            Self::Duplicate { canonical, .. } => Some(*canonical),
            _ => None,
        }
    }

    pub fn is_dynamic(&self) -> Option<usize> {
        match self {
            Self::Duplicate { canonical, .. } => Some(*canonical),
            _ => None,
        }
    }

    pub fn is_duplicate_prefix(&self, prefix: &str) -> bool {
        self.prefix().map(|p| p == prefix).unwrap_or(false)
    }
}
