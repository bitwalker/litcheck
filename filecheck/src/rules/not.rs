#![allow(unused)]

use crate::{common::*, pattern::matcher::MatchAll};

use super::*;

#[derive(Debug)]
pub struct CheckNot<'a> {
    patterns: MatchAll<'a>,
}
impl<'a> CheckNot<'a> {
    pub fn new(patterns: MatchAll<'a>) -> Self {
        Self { patterns }
    }
}
impl<'check> Spanned for CheckNot<'check> {
    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }
}
impl<'check> Rule for CheckNot<'check> {
    fn kind(&self) -> Check {
        Check::Not
    }

    fn apply<'input, 'context, C>(&self, _context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        // TODO: Implement support for CHECK-NOT
        let diag = Diag::new("support for CHECK-NOT has not yet been implemented")
            .with_label(Label::new(self.span(), "not supported"));
        Err(Report::from(diag))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn check_not_test() {
        todo!()
    }
}
