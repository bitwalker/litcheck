mod always;
mod context;
mod cursor;
mod env;
mod regex;
pub mod searcher;
mod simple;
mod smart;
mod stack;
mod substring;
mod whitespace;

pub use self::always::AlwaysMatch;
pub use self::context::{Context, ContextExt, ContextGuard, MatchContext};
pub use self::cursor::{Cursor, CursorGuard, CursorPosition};
pub use self::env::{Env, LexicalScope, LexicalScopeExtend, LexicalScopeMut, ScopeGuard};
pub use self::regex::{RegexMatcher, RegexSetMatcher, RegexSetSearcher};
pub use self::simple::SimpleMatcher;
pub use self::smart::SmartMatcher;
pub use self::stack::Operand;
pub use self::substring::{
    SubstringMatcher, SubstringSetBuilder, SubstringSetMatcher, SubstringSetSearcher,
};
pub use self::whitespace::AsciiWhitespaceMatcher;

use std::fmt;

use litcheck::{
    diagnostics::{DiagResult, SourceSpan, Spanned},
    range::Range,
};
pub use regex_automata::util::look::LookMatcher;

use crate::{
    check::{CheckFailedError, Input},
    expr::Value,
};

pub type AnyMatcher<'a> = Box<dyn DynMatcher + 'a>;

pub type AnyMatcherMut<'a> = Box<dyn DynMatcherMut + 'a>;

/// This trait is used for match patterns which are pure, i.e. they have
/// no effect on the current [MatchContext].
pub trait Matcher: MatcherMut {
    /// Search for a match in the given input buffer
    ///
    /// The first match found is returned.
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized;
}
impl<'a, M> Matcher for &'a M
where
    M: ?Sized + Matcher,
{
    fn try_match<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        <M as Matcher>::try_match(self, input, context)
    }
}

pub trait DynMatcher: DynMatcherMut {
    fn try_match_dyn<'input>(
        &self,
        input: Input<'input>,
        context: &dyn Context<'input, '_>,
    ) -> DiagResult<MatchResult<'input>>;
}
impl<M> DynMatcher for M
where
    M: Matcher,
{
    fn try_match_dyn<'input>(
        &self,
        input: Input<'input>,
        context: &dyn Context<'input, '_>,
    ) -> DiagResult<MatchResult<'input>> {
        self.try_match(input, context)
    }
}

/// This trait is used for match patterns which have side effects on the
/// current [MatchContext] when successful.
///
/// These matchers may bind new variables, push operands or otherwise modify the match context.
/// Implementors should ensure that such effects are only applied if a match succeeds however,
/// to avoid polluting the match context.
pub trait MatcherMut: fmt::Debug + Spanned {
    /// Search for a match in the given input buffer
    ///
    /// The first match found is returned.
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized;
}
impl<'a, M> MatcherMut for &'a M
where
    M: ?Sized + MatcherMut,
{
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        <M as MatcherMut>::try_match_mut(self, input, context)
    }
}
impl<'a> MatcherMut for (dyn DynMatcher + 'a) {
    #[inline]
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let passthrough = context.protect();
        self.try_match_dyn(input, &passthrough)
    }
}
impl<'a> MatcherMut for Box<dyn DynMatcher + 'a> {
    #[inline]
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        (**self).try_match_mut(input, context)
    }
}

pub trait DynMatcherMut: fmt::Debug + Spanned {
    fn try_match_mut_dyn<'input>(
        &self,
        input: Input<'input>,
        context: &mut dyn Context<'input, '_>,
    ) -> DiagResult<MatchResult<'input>>;
}
impl<M> DynMatcherMut for M
where
    M: MatcherMut,
{
    fn try_match_mut_dyn<'input>(
        &self,
        input: Input<'input>,
        context: &mut dyn Context<'input, '_>,
    ) -> DiagResult<MatchResult<'input>> {
        self.try_match_mut(input, context)
    }
}
impl<'a> MatcherMut for (dyn DynMatcherMut + 'a) {
    #[inline]
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut passthrough = context.protect();
        let result = self.try_match_mut_dyn(input, &mut passthrough)?;
        if result.is_ok() {
            passthrough.save();
        }
        Ok(result)
    }
}
impl<'a> MatcherMut for Box<dyn DynMatcherMut + 'a> {
    #[inline]
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        (**self).try_match_mut(input, context)
    }
}

#[derive(Debug)]
pub struct MatchResult<'input> {
    pub ty: MatchType,
    pub info: Option<MatchInfo<'input>>,
}
impl<'input> MatchResult<'input> {
    pub fn new(ty: MatchType, info: Option<MatchInfo<'input>>) -> Self {
        Self { ty, info }
    }

    /// An expected pattern was succesfully found in the input
    pub fn ok(info: MatchInfo<'input>) -> Self {
        Self {
            ty: MatchType::MatchFoundAndExpected,
            info: Some(info),
        }
    }

    /// A negative match assertion, i.e. CHECK-NOT, successfully
    /// matched by not finding the pattern in the input. Such a
    /// "match" is empty.
    pub fn empty() -> Self {
        Self {
            ty: MatchType::MatchNoneAndExcluded,
            info: None,
        }
    }

    /// A match failed with the given error
    pub fn failed(error: CheckFailedError) -> Self {
        Self {
            ty: MatchType::Failed(error),
            info: None,
        }
    }

    /// A match was found, but failed with the given error,
    pub fn match_found_but_failed(error: CheckFailedError, info: MatchInfo<'input>) -> Self {
        Self {
            ty: MatchType::Failed(error),
            info: Some(info),
        }
    }

    /// Returns true if the match succeeded.
    ///
    /// This does not necessarily mean there is a corresponding [MatchInfo],
    /// such as in the case of negative matches like CHECK-NOT.
    pub fn is_ok(&self) -> bool {
        self.ty.is_ok()
    }

    /// Returns true if this result represents a successful CHECK-NOT match.
    pub fn is_empty(&self) -> bool {
        matches!(self.ty, MatchType::MatchNoneAndExcluded)
    }

    #[inline]
    pub fn pattern_id(&self) -> Option<usize> {
        self.info.as_ref().map(|info| info.pattern_id)
    }

    #[inline]
    pub fn matched_range(&self) -> Option<Range<usize>> {
        self.info.as_ref().map(|info| info.matched_range())
    }

    pub fn unwrap_err(self) -> CheckFailedError {
        match self.ty {
            MatchType::Failed(err) => err,
            ty => panic!("attempted to unwrap error from {ty}"),
        }
    }

    pub fn into_result(self) -> Result<Option<MatchInfo<'input>>, CheckFailedError> {
        match self {
            Self {
                ty: MatchType::Failed(err),
                ..
            } => Err(err),
            Self {
                info: Some(info), ..
            } => Ok(Some(info)),
            Self { info: None, .. } => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum MatchType {
    /// Indicates a good match for an expected pattern.
    MatchFoundAndExpected,
    /// Indicates no match for an excluded pattern.
    MatchNoneAndExcluded,
    /// The match failed for some reason
    Failed(CheckFailedError),
}
impl MatchType {
    pub fn is_ok(&self) -> bool {
        matches!(
            self,
            Self::MatchFoundAndExpected | Self::MatchNoneAndExcluded
        )
    }
}
impl fmt::Display for MatchType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MatchFoundAndExpected => f.write_str("match found for expected pattern"),
            Self::MatchNoneAndExcluded => f.write_str("excluded pattern was never matched"),
            Self::Failed(err) => write!(f, "{err}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MatchInfo<'input> {
    /// The span of the matched input
    pub span: SourceSpan,
    /// The span of the pattern that was matched
    pub pattern_span: SourceSpan,
    /// The index of the pattern that was matched, if applicable
    ///
    /// For single pattern matchers, this is always 0
    pub pattern_id: usize,
    /// For matchers which capture parts of the input, this
    /// contains the captured values and their information
    pub captures: Vec<CaptureInfo<'input>>,
}
impl<'input> MatchInfo<'input> {
    pub fn new(span: impl Into<SourceSpan>, pattern_span: SourceSpan) -> Self {
        Self::new_with_pattern(span, pattern_span, 0)
    }

    pub fn new_with_pattern(
        span: impl Into<SourceSpan>,
        pattern_span: SourceSpan,
        pattern_id: usize,
    ) -> Self {
        Self {
            span: span.into(),
            pattern_span,
            pattern_id,
            captures: Vec::new(),
        }
    }

    pub fn with_pattern(mut self, pattern_id: usize) -> Self {
        self.pattern_id = pattern_id;
        self
    }

    pub fn with_captures(mut self, captures: Vec<CaptureInfo<'input>>) -> Self {
        self.captures = captures;
        self
    }

    pub fn matched_range(&self) -> Range<usize> {
        let start = self.span.offset();
        Range::new(start, start + self.span.len())
    }

    pub fn into_static(self) -> MatchInfo<'static> {
        MatchInfo {
            captures: self
                .captures
                .into_iter()
                .map(CaptureInfo::into_static)
                .collect(),
            ..self
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CaptureInfo<'input> {
    /// The span of the capture in the input
    pub span: SourceSpan,
    /// The span of the pattern from which this capture originated
    pub pattern_span: SourceSpan,
    /// The index of the capture
    pub index: usize,
    /// The captured value
    pub value: Value<'input>,
}
impl CaptureInfo<'_> {
    pub fn into_static(self) -> CaptureInfo<'static> {
        CaptureInfo {
            value: match self.value {
                Value::Undef => Value::Undef,
                Value::Str(s) => Value::Str(s.into_owned().into()),
                Value::Num(expr) => Value::Num(expr),
            },
            ..self
        }
    }
}

#[derive(Debug, Default)]
pub struct Matches<'input>(smallvec::SmallVec<[MatchResult<'input>; 1]>);
impl<'input> Matches<'input> {
    pub fn with_capacity(cap: usize) -> Self {
        Self(smallvec::SmallVec::with_capacity(cap))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    pub fn as_slice(&self) -> &[MatchResult<'input>] {
        self.0.as_slice()
    }

    /// Returns true if all of the matches are successful
    pub fn is_ok(&self) -> bool {
        self.0.iter().all(|mr| mr.is_ok())
    }

    /// Returns true if at least one match failed
    pub fn has_errors(&self) -> bool {
        !self.is_ok()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &MatchResult<'input>> + '_ {
        self.0.iter()
    }
}
impl<'input> From<MatchResult<'input>> for Matches<'input> {
    fn from(result: MatchResult<'input>) -> Self {
        Self(smallvec::smallvec![result])
    }
}
impl<'input> FromIterator<MatchResult<'input>> for Matches<'input> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = MatchResult<'input>>,
    {
        Self(smallvec::SmallVec::from_iter(iter))
    }
}
impl<'input> IntoIterator for Matches<'input> {
    type Item = Result<Option<MatchInfo<'input>>, CheckFailedError>;
    type IntoIter = MatchIter<'input>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        MatchIter {
            matches: self.0.into_iter(),
        }
    }
}
impl<'input> Matches<'input> {
    pub fn push(&mut self, result: MatchResult<'input>) {
        self.0.push(result);
    }

    pub fn append(&mut self, matches: &mut Self) {
        self.0.append(&mut matches.0);
    }

    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = MatchResult<'input>>,
    {
        self.0.extend(iter);
    }
}

pub struct MatchIter<'input> {
    matches: smallvec::IntoIter<[MatchResult<'input>; 1]>,
}
impl<'input> MatchIter<'input> {
    #[inline]
    pub fn as_slice(&self) -> &[MatchResult<'input>] {
        self.matches.as_slice()
    }
}
impl<'input> ExactSizeIterator for MatchIter<'input> {
    fn len(&self) -> usize {
        self.matches.len()
    }
}
impl<'input> Iterator for MatchIter<'input> {
    type Item = Result<Option<MatchInfo<'input>>, CheckFailedError>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.matches.next().map(MatchResult::into_result)
    }
}
impl<'input> DoubleEndedIterator for MatchIter<'input> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.matches.next_back().map(MatchResult::into_result)
    }
}
