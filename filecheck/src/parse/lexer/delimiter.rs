#[derive(Debug, Copy, Clone)]
pub enum Delimiter {
    NumericMatchStart,
    MatchStart,
    MatchEnd,
    RegexStart,
    RegexEnd,
}
impl Delimiter {
    pub const ALL: &'static [Self] = &[
        Self::NumericMatchStart,
        Self::MatchStart,
        Self::MatchEnd,
        Self::RegexStart,
        Self::RegexEnd,
    ];

    #[inline]
    pub fn from_pid(id: usize) -> Self {
        Self::ALL[id]
    }
}
impl AsRef<str> for Delimiter {
    fn as_ref(&self) -> &str {
        match self {
            Self::NumericMatchStart => "[[#",
            Self::MatchStart => "[[",
            Self::MatchEnd => "]]",
            Self::RegexStart => "{{",
            Self::RegexEnd => "}}",
        }
    }
}
impl AsRef<[u8]> for Delimiter {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::NumericMatchStart => b"[[#",
            Self::MatchStart => b"[[",
            Self::MatchEnd => b"]]",
            Self::RegexStart => b"{{",
            Self::RegexEnd => b"}}",
        }
    }
}
