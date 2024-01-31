use std::str::FromStr;

use crate::common::*;

#[derive(Debug)]
pub struct InvalidCheckModifierError;

bitflags::bitflags! {
    /// Represents modificatons to the behavior of a [Check]
    #[derive(Copy, Clone)]
    pub struct CheckModifier: u16 {
        const LITERAL = 1;
        const COUNT = 2;
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
        let mut mods = 0;
        if self.contains(Self::LITERAL) {
            mods += 1;
            f.write_str("LITERAL")?;
        }
        if self.contains(Self::COUNT) {
            if mods > 0 {
                f.write_str(" | ")?;
            }
            write!(f, "COUNT({})", self.count())?;
        }
        f.write_char(')')
    }
}
impl CheckModifier {
    pub fn is_literal(&self) -> bool {
        self.contains(Self::LITERAL)
    }

    pub fn from_count(count: u8) -> Self {
        let count = (count as u16) << 2;
        Self::COUNT | CheckModifier::from_bits_retain(count)
    }

    pub fn count(&self) -> usize {
        if self.contains(Self::COUNT) {
            (self.bits() >> 2) as usize
        } else {
            1
        }
    }
}
impl Eq for CheckModifier {}
impl PartialEq for CheckModifier {
    fn eq(&self, other: &Self) -> bool {
        self.count() == other.count() && self.is_literal() == other.is_literal()
    }
}
