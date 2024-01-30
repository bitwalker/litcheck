mod dag;
mod empty;
mod next;
mod not;
mod plain;
mod same;

pub use self::dag::CheckDag;
pub use self::empty::CheckEmpty;
pub use self::next::CheckNext;
pub use self::not::CheckNot;
pub use self::plain::CheckPlain;
pub use self::same::CheckSame;

use std::fmt;

use litcheck::diagnostics::{DiagResult, SourceSpan};

use crate::check::{
    matchers::{Context, MatchResult},
    Check,
};

pub trait Rule: fmt::Debug {
    fn kind(&self) -> Check;
    fn span(&self) -> SourceSpan;
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized;
}
impl<'r, R> Rule for &'r R
where
    R: ?Sized + Rule,
{
    #[inline(always)]
    fn kind(&self) -> Check {
        <R as Rule>::kind(self)
    }
    #[inline(always)]
    fn span(&self) -> SourceSpan {
        <R as Rule>::span(self)
    }
    #[inline(always)]
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        <R as Rule>::apply(self, context)
    }
}
impl<R> Rule for Box<R>
where
    R: Rule,
{
    #[inline(always)]
    fn kind(&self) -> Check {
        (**self).kind()
    }
    #[inline(always)]
    fn span(&self) -> SourceSpan {
        (**self).span()
    }
    #[inline(always)]
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        (**self).apply(context)
    }
}

pub trait DynRule: fmt::Debug {
    fn kind(&self) -> Check;
    fn span(&self) -> SourceSpan;
    fn apply_dyn<'input, 'context>(
        &self,
        context: &mut dyn Context<'input, 'context>,
    ) -> DiagResult<MatchResult<'input>>;
}
impl<'context> Rule for (dyn DynRule + 'context) {
    #[inline(always)]
    fn kind(&self) -> Check {
        Self::kind(self)
    }
    #[inline(always)]
    fn span(&self) -> SourceSpan {
        Self::span(self)
    }
    #[inline(always)]
    fn apply<'input, 'ctx, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'ctx> + ?Sized,
    {
        let mut passthrough = context.protect();
        let result = self.apply_dyn(&mut passthrough)?;
        if result.is_ok() {
            passthrough.save();
        }
        Ok(result)
    }
}

impl<R> DynRule for R
where
    R: Rule,
{
    #[inline(always)]
    fn kind(&self) -> Check {
        <R as Rule>::kind(self)
    }
    #[inline(always)]
    fn span(&self) -> SourceSpan {
        <R as Rule>::span(self)
    }
    #[inline(always)]
    fn apply_dyn<'input, 'context>(
        &self,
        context: &mut dyn Context<'input, 'context>,
    ) -> DiagResult<MatchResult<'input>> {
        <R as Rule>::apply(self, context)
    }
}
