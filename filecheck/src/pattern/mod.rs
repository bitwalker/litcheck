mod id;
mod iter;
pub mod matcher;
mod matches;
mod prefix;
pub mod search;

pub use self::id::PatternIdentifier;
pub use self::matcher::*;
pub use self::matches::Matches;
pub use self::prefix::PatternPrefix;
pub use self::search::{DefaultSearcher, PatternSetSearcher, Searcher};

use crate::{
    ast::{CheckPattern, CheckPatternPart, Match},
    common::*,
    errors::InvalidCheckFileError,
    expr::ValueType,
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
    ) -> DiagResult<SimpleMatcher<'a>> {
        match pattern {
            CheckPattern::Literal(lit) => Ok(SimpleMatcher::Substring(SubstringMatcher::new(
                lit.map(Cow::Borrowed),
            )?)),
            CheckPattern::Regex(s) => Ok(SimpleMatcher::Regex(RegexMatcher::new(
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
