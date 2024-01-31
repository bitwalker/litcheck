mod matchers;
mod result;

pub use self::matchers::*;
pub use self::result::{CaptureInfo, MatchInfo, MatchResult, MatchType};

use crate::common::*;

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
