use crate::{
    ast::{Match, RegexPattern},
    common::*,
};

#[derive(Debug)]
pub enum PatternPrefix<'a> {
    /// The full pattern is the prefix (a literal string)
    Literal {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    /// The prefix is a substring of a larger string or pattern
    Substring {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    /// The prefix is a regular expression
    Regex { id: usize, prefix: RegexPattern<'a> },
    /// The prefix is a match block or substitution
    /// The prefix is a match block or substitution
    Dynamic {
        id: usize,
        prefix: Cow<'a, Match<'a>>,
    },
}
impl<'a> PatternPrefix<'a> {
    pub fn id(&self) -> usize {
        match self {
            Self::Literal { id, .. }
            | Self::Substring { id, .. }
            | Self::Regex { id, .. }
            | Self::Dynamic { id, .. } => *id,
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Literal { prefix, .. } | Self::Substring { prefix, .. } => prefix.span(),
            Self::Regex { prefix, .. } => prefix.span(),
            Self::Dynamic { prefix, .. } => prefix.span(),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Literal { prefix, .. } | Self::Substring { prefix, .. } => {
                Some(prefix.inner().as_ref())
            }
            Self::Regex { prefix, .. } => Some(prefix.as_ref()),
            Self::Dynamic { .. } => None,
        }
    }

    pub fn to_str(&self) -> Option<Span<Cow<'a, str>>> {
        match self {
            Self::Literal { prefix, .. } | Self::Substring { prefix, .. } => Some(prefix.clone()),
            Self::Regex { prefix, .. } => Some(prefix.pattern.clone()),
            Self::Dynamic { .. } => None,
        }
    }

    pub fn into_str(self) -> Option<Span<Cow<'a, str>>> {
        match self {
            Self::Literal { prefix, .. } | Self::Substring { prefix, .. } => Some(prefix),
            Self::Regex { prefix, .. } => Some(prefix.pattern),
            Self::Dynamic { .. } => None,
        }
    }

    pub fn into_regex_pattern(self) -> Option<RegexPattern<'a>> {
        match self {
            Self::Literal { prefix, .. } | Self::Substring { prefix, .. } => Some(
                RegexPattern::new(prefix.map(|s| Cow::Owned(regex::escape(s.as_ref())))),
            ),
            Self::Regex { prefix, .. } => Some(prefix),
            Self::Dynamic { .. } => None,
        }
    }

    pub fn is_regex(&self) -> bool {
        matches!(self, Self::Regex { .. })
    }

    pub fn is_regex_compatible(&self) -> bool {
        matches!(
            self,
            Self::Regex { .. } | Self::Literal { .. } | Self::Substring { .. }
        )
    }

    pub fn is_dynamic(&self) -> bool {
        matches!(self, Self::Dynamic { .. })
    }

    pub fn is_capturing(&self) -> bool {
        match self {
            Self::Regex { prefix, .. } if !prefix.captures.is_empty() => true,
            Self::Dynamic { .. } => true,
            _ => false,
        }
    }

    /// Returns true if `self` starts with `prefix`
    pub fn overlaps(&self, prefix: &str) -> bool {
        self.as_str()
            .map(|p| p.starts_with(prefix))
            .unwrap_or(false)
    }

    /// Returns true if `prefix` starts with `self`
    pub fn is_overlapped_by(&self, prefix: &str) -> bool {
        self.as_str()
            .map(|p| prefix.starts_with(p))
            .unwrap_or(false)
    }

    /// Returns true if `prefix` is a duplicate of `self`
    pub fn is_duplicate_prefix(&self, prefix: &str) -> bool {
        self.as_str().map(|p| p == prefix).unwrap_or(false)
    }
}
