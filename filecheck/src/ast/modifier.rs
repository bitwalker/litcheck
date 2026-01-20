use std::str::FromStr;

use crate::common::*;

#[derive(Debug)]
pub struct InvalidCheckModifierError;

bitflags::bitflags! {
    /// Represents modificatons to the behavior of a [Check]
    #[derive(Copy, Clone)]
    pub struct CheckModifier: u16 {
        const LITERAL = 1;
    }
}
impl Default for CheckModifier {
    fn default() -> Self {
        Self::empty()
    }
}
impl FromStr for CheckModifier {
    type Err = InvalidCheckModifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "LITERAL" | "literal" => Ok(Self::LITERAL),
            _ => Err(InvalidCheckModifierError),
        }
    }
}
impl fmt::Debug for CheckModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Write;
        f.write_str("CheckModifier(")?;
        if self.contains(Self::LITERAL) {
            f.write_str("LITERAL")?;
        }
        f.write_char(')')
    }
}
impl CheckModifier {
    pub fn is_literal(&self) -> bool {
        self.contains(Self::LITERAL)
    }
}
impl Eq for CheckModifier {}
impl PartialEq for CheckModifier {
    fn eq(&self, other: &Self) -> bool {
        self.is_literal() == other.is_literal()
    }
}
