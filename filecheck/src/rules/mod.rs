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

use crate::common::{Check, Context, DiagResult, Matches, Spanned};

pub trait Rule: fmt::Debug + Spanned {
    fn kind(&self) -> Check;
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
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
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        <R as Rule>::apply(self, context)
    }
}
impl<R> Rule for Box<R>
where
    R: Rule + ?Sized,
{
    #[inline(always)]
    fn kind(&self) -> Check {
        (**self).kind()
    }
    #[inline(always)]
    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        (**self).apply(context)
    }
}

pub trait DynRule: fmt::Debug + Spanned {
    fn kind(&self) -> Check;
    fn apply_dyn<'input>(
        &self,
        context: &mut dyn Context<'input, '_>,
    ) -> DiagResult<Matches<'input>>;
}
impl<'context> Rule for (dyn DynRule + 'context) {
    #[inline(always)]
    fn kind(&self) -> Check {
        Self::kind(self)
    }
    #[inline(always)]
    fn apply<'input, 'ctx, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'ctx> + ?Sized,
    {
        let mut passthrough = context.protect();
        let result = self.apply_dyn(&mut passthrough)?;
        passthrough.save();
        Ok(result)
    }
}

impl<R> DynRule for R
where
    R: Rule + ?Sized,
{
    #[inline(always)]
    fn kind(&self) -> Check {
        <R as Rule>::kind(self)
    }
    #[inline(always)]
    fn apply_dyn<'input>(
        &self,
        context: &mut dyn Context<'input, '_>,
    ) -> DiagResult<Matches<'input>> {
        <R as Rule>::apply(self, context)
    }
}
