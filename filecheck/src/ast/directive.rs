use std::str::FromStr;

use crate::common::*;

use super::CheckModifier;

#[derive(Debug)]
pub enum InvalidCheckTypeError {
    Unrecognized,
    InvalidCount(core::num::ParseIntError),
}

/// This enum represents the kind of directive that was parsed
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub enum Check {
    /// Not used after parsing, represents a line with no directives
    #[default]
    None,
    /// The base CHECK directive, i.e. match the pattern is somewhere in the file
    Plain,
    /// The CHECK-NEXT directive, i.e. the pattern must match on the next line
    /// from the previous pattern.
    Next,
    /// The CHECK-SAME directive, i.e. the pattern must match on the same line
    /// as the previous pattern.
    Same,
    /// The CHECK-NOT directive, i.e. the pattern must _not_ match between this
    /// directive and the next positive match directive, or the end of file. This
    /// is the only negative match assertion supported.
    Not,
    /// The CHECK-DAG directive, i.e. like CHECK, but may match in any order relative
    /// to other CHECK-DAG directives which are all part of a single consecutive
    /// grouping. A non-CHECK-DAG directive between two sets of CHECK-DAG directives
    /// cause the two sets to be split into two logical groups, where the ordering
    /// between the groups is strict, but within the group it is not.
    Dag,
    /// The CHECK-LABEL directive, i.e. a regular CHECK, with the additional restriction
    /// that the pattern may not contain references to (or bind) variables. This
    /// directive type divides the checks (and input) into logical "blocks". Checks other
    /// than CHECK-LABEL belong to the block defined by their preceding CHECK-LABEL. Checks
    /// before the first CHECK-LABEL are part of an implicit prologue block.
    ///
    /// CHECK-LABEL divides up the input into blocks as well, by first matching all of the
    /// CHECK-LABEL patterns, and then block-by-block, matching all of the checks "owned"
    /// by each CHECK-LABEL, rejecting any matches that would match outside the region
    /// of input assigned to the block.
    Label,
    /// The CHECK-EMPTY directive, i.e. the next line must be empty, containing nothing
    /// but a newline, no other whitespace.
    Empty,
    /// This is only used during parsing, but represents CHECK-COUNT-N, i.e. a CHECK
    /// that is repeated N times. N must be non-zero.
    Count(usize),
    /// The COM directive, i.e. a comment.
    ///
    /// This is only used during parsing.
    Comment,
}
impl Check {
    pub fn suffix(&self) -> Option<&'static str> {
        match self {
            Self::Plain => Some(""),
            Self::Next => Some("-NEXT"),
            Self::Same => Some("-SAME"),
            Self::Not => Some("-NOT"),
            Self::Dag => Some("-DAG"),
            Self::Label => Some("-LABEL"),
            Self::Empty => Some("-EMPTY"),
            Self::Count(_) => Some("-COUNT"),
            Self::Comment | Self::None => None,
        }
    }
}
impl fmt::Display for Check {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => f.write_str("CHECK-NONE"),
            Self::Plain => f.write_str("CHECK"),
            Self::Next => f.write_str("CHECK-NEXT"),
            Self::Same => f.write_str("CHECK-SAME"),
            Self::Not => f.write_str("CHECK-NOT"),
            Self::Dag => f.write_str("CHECK-DAG"),
            Self::Label => f.write_str("CHECK-LABEL"),
            Self::Empty => f.write_str("CHECK-EMPTY"),
            Self::Count(n) => write!(f, "CHECK-COUNT-{n}"),
            Self::Comment => f.write_str("COM"),
        }
    }
}
impl FromStr for Check {
    type Err = InvalidCheckTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "" => Ok(Self::Plain),
            "NEXT" | "next" => Ok(Self::Next),
            "SAME" | "same" => Ok(Self::Same),
            "NOT" | "not" => Ok(Self::Not),
            "DAG" | "dag" => Ok(Self::Dag),
            "LABEL" | "label" => Ok(Self::Label),
            "EMPTY" | "empty" => Ok(Self::Empty),
            _ => match s
                .strip_prefix("COUNT-")
                .or_else(|| s.strip_prefix("count-"))
            {
                None => Err(InvalidCheckTypeError::Unrecognized),
                Some(count) => count
                    .parse::<usize>()
                    .map_err(InvalidCheckTypeError::InvalidCount)
                    .map(Self::Count),
            },
        }
    }
}

/// This represents the complete type of a CHECK* directive
#[derive(Debug)]
pub struct CheckType {
    span: SourceSpan,
    /// The kind of directive represented
    pub kind: Check,
    /// Any modifiers applied to this directive
    pub modifiers: Span<CheckModifier>,
}
impl Default for CheckType {
    fn default() -> Self {
        Self::new(SourceSpan::from(0..0), Default::default())
    }
}
impl Spanned for CheckType {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl CheckType {
    pub fn new(span: SourceSpan, kind: Check) -> Self {
        Self {
            span,
            kind,
            modifiers: Span::new(span, Default::default()),
        }
    }

    pub fn with_modifiers(mut self, modifiers: Span<CheckModifier>) -> Self {
        self.modifiers = modifiers;
        self
    }

    pub fn is_literal_match(&self) -> bool {
        self.modifiers.contains(CheckModifier::LITERAL)
    }

    pub fn count(&self) -> usize {
        self.modifiers.count()
    }
}
impl Eq for CheckType {}
impl PartialEq for CheckType {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.modifiers == other.modifiers
    }
}
