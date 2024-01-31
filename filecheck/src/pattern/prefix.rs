use crate::common::*;

#[derive(Debug)]
pub enum PatternPrefix<'a> {
    Literal {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    Regex {
        id: usize,
        prefix: Span<Cow<'a, str>>,
    },
    Dynamic {
        id: usize,
    },
    Duplicate {
        id: usize,
        canonical: usize,
    },
}
impl<'a> PatternPrefix<'a> {
    pub fn id(&self) -> usize {
        match self {
            Self::Literal { id, .. }
            | Self::Regex { id, .. }
            | Self::Dynamic { id, .. }
            | Self::Duplicate { id, .. } => *id,
        }
    }

    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.span()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn prefix(&self) -> Option<&str> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.as_ref()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn clone_prefix(&self) -> Option<Span<Cow<'a, str>>> {
        match self {
            Self::Literal { prefix, .. } | Self::Regex { prefix, .. } => Some(prefix.clone()),
            Self::Dynamic { .. } | Self::Duplicate { .. } => None,
        }
    }

    pub fn is_duplicate(&self) -> Option<usize> {
        match self {
            Self::Duplicate { canonical, .. } => Some(*canonical),
            _ => None,
        }
    }

    pub fn is_dynamic(&self) -> Option<usize> {
        match self {
            Self::Duplicate { canonical, .. } => Some(*canonical),
            _ => None,
        }
    }

    pub fn is_duplicate_prefix(&self, prefix: &str) -> bool {
        self.prefix().map(|p| p == prefix).unwrap_or(false)
    }
}
