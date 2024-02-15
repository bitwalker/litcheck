use crate::common::*;

#[derive(Debug, thiserror::Error)]
#[error("invalid pattern identifier: out of range")]
pub struct InvalidPatternIdentifier;

pub trait PatternIdentifier: Sized + Copy + PartialEq + Eq + fmt::Debug {
    fn new(id: usize) -> Result<Self, InvalidPatternIdentifier>;
    fn new_unchecked(id: usize) -> Self;
    fn as_usize(&self) -> usize;
}

impl PatternIdentifier for usize {
    #[inline(always)]
    fn new(id: usize) -> Result<Self, InvalidPatternIdentifier> {
        Ok(id)
    }
    #[inline(always)]
    fn new_unchecked(id: usize) -> Self {
        id
    }
    #[inline(always)]
    fn as_usize(&self) -> usize {
        *self
    }
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
