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
impl<'check> Rule for CheckNot<'check> {
    fn kind(&self) -> Check {
        Check::Not
    }

    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }

    fn apply<'input, 'context, C>(&self, _context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        todo!()
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
