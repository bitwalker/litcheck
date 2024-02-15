mod id;
mod iter;
pub mod matcher;
mod matches;
mod prefix;
pub mod search;
pub(crate) mod visitors;

pub use self::id::PatternIdentifier;
pub use self::matcher::*;
pub use self::matches::Matches;
pub use self::prefix::PatternPrefix;
pub use self::search::{DefaultSearcher, PatternSearcher, Searcher};

use crate::{
    ast::{CheckPattern, CheckPatternPart},
    common::*,
    errors::InvalidCheckFileError,
};

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

    pub fn from_prefix(
        prefix: PatternPrefix<'a>,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
        match prefix {
            PatternPrefix::Literal { prefix, .. } | PatternPrefix::Substring { prefix, .. } => {
                if prefix.is_empty() {
                    return Err(Report::from(InvalidCheckFileError::EmptyPattern(
                        prefix.span(),
                    )));
                }
                if prefix.trim().is_empty() {
                    if prefix.chars().all(|c| c.is_ascii_whitespace()) {
                        Ok(Pattern::Whitespace(AsciiWhitespaceMatcher::new(
                            prefix.span(),
                        )))
                    } else {
                        Ok(Pattern::Regex(RegexMatcher::new_nocapture(Span::new(
                            prefix.span(),
                            Cow::Borrowed(r"\s+"),
                        ))?))
                    }
                } else {
                    Ok(Pattern::Substring(SubstringMatcher::new(prefix)?))
                }
            }
            PatternPrefix::Regex { prefix, .. } if prefix.captures.is_empty() => {
                Ok(Pattern::Regex(RegexMatcher::new_nocapture(prefix.pattern)?))
            }
            PatternPrefix::Regex { prefix, .. } => {
                Ok(Pattern::Regex(RegexMatcher::new(prefix, interner)?))
            }
            PatternPrefix::Dynamic { prefix, .. } => {
                let mut builder = SmartMatcher::build(prefix.span(), interner);
                builder.lower_match(prefix.into_owned())?;
                Ok(Self::Smart(builder.build()))
            }
        }
    }

    pub fn compile(
        mut pattern: CheckPattern<'a>,
        interner: &mut StringInterner,
    ) -> DiagResult<Self> {
        pattern.compact(interner);
        match pattern {
            CheckPattern::Literal(s) => {
                Self::from_prefix(PatternPrefix::Literal { prefix: s, id: 0 }, interner)
            }
            CheckPattern::Regex(s) => Ok(Pattern::Regex(RegexMatcher::new(s, interner)?)),
            CheckPattern::Match(s) => {
                // At least one part requires expression evaluation
                let (span, parts) = s.into_parts();
                let mut builder = SmartMatcher::build(span, interner);
                for part in parts.into_iter() {
                    match part {
                        CheckPatternPart::Literal(s) => {
                            builder.literal(s)?;
                        }
                        CheckPatternPart::Regex(s) => {
                            builder.regex_pattern(s)?;
                        }
                        CheckPatternPart::Match(m) => {
                            builder.lower_match(m)?;
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
        mut pattern: CheckPattern<'a>,
        interner: &mut StringInterner,
    ) -> DiagResult<SimpleMatcher<'a>> {
        pattern.compact(interner);

        match pattern {
            CheckPattern::Literal(lit) => Ok(SimpleMatcher::Substring(SubstringMatcher::new(lit)?)),
            CheckPattern::Regex(regex) if regex.captures.is_empty() => {
                Ok(SimpleMatcher::Regex(RegexMatcher::new(regex, interner)?))
            }
            pattern @ (CheckPattern::Regex(_) | CheckPattern::Match(_)) => {
                let diag = Diag::new("invalid variable usage in pattern")
                    .with_label(Label::new(span, "occurs in this pattern"))
                    .and_labels(
                        pattern
                            .locate_variables()
                            .map(|span| Label::new(span, "occurs here").into()),
                    )
                    .with_help("CHECK-LABEL patterns must be literals or regular expressions");
                Err(Report::new(diag))
            }
            CheckPattern::Empty(_) => unreachable!(
                "{pattern:?} is only valid for CHECK-EMPTY, and is not an actual pattern"
            ),
        }
    }

    pub fn compile_literal(pattern: CheckPattern<'a>) -> DiagResult<Self> {
        match pattern {
            CheckPattern::Literal(lit) => Ok(Pattern::Substring(SubstringMatcher::new(lit)?)),
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
