use crate::common::*;

#[derive(Debug, Clone)]
pub struct Number {
    pub span: SourceSpan,
    pub format: NumberFormat,
    pub value: i64,
}
impl Number {
    pub fn new(span: SourceSpan, value: i64) -> Self {
        Self {
            span,
            format: NumberFormat::default(),
            value,
        }
    }

    pub fn new_with_format(span: SourceSpan, value: i64, format: NumberFormat) -> Self {
        Self {
            span,
            format,
            value,
        }
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.format == other.format
    }
}
impl Spanned for Number {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.value;
        match self.format {
            NumberFormat::Unsigned { precision: 0 } => write!(f, "{}", value as u64),
            NumberFormat::Unsigned { precision: n } => {
                write!(f, "{:0n$}", value as u64, n = n as usize)
            }
            NumberFormat::Signed { precision: 0 } => write!(f, "{value}"),
            NumberFormat::Signed { precision: n } => write!(f, "{value:0n$}", n = n as usize),
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
            } => write!(f, "{value:#x?}"),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
            } => write!(f, "{value:x?}"),
            NumberFormat::Hex {
                require_prefix: true,
                precision: n,
            } => write!(f, "{value:#0n$x?}", n = n as usize),
            NumberFormat::Hex {
                require_prefix: false,
                precision: n,
            } => write!(f, "{value:0n$x?}", n = n as usize),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumberFormat {
    Unsigned { precision: u8 },
    Signed { precision: u8 },
    Hex { precision: u8, require_prefix: bool },
}
impl NumberFormat {
    pub fn is_signed(&self) -> bool {
        matches!(self, Self::Signed { .. })
    }

    pub fn is_hex(&self) -> bool {
        matches!(self, Self::Hex { .. })
    }

    pub fn precision(&self) -> usize {
        match self {
            Self::Unsigned { precision }
            | Self::Signed { precision }
            | Self::Hex { precision, .. } => *precision as usize,
        }
    }

    pub fn pattern(&self) -> String {
        const I64_MAX: &str = "9223372036854775807";
        const MAX_DIGITS: usize = I64_MAX.len();

        match self {
            NumberFormat::Signed { precision: 0 } => {
                format!("[-+]?(?P<digits>[0-9]{{1,{MAX_DIGITS}}})")
            }
            NumberFormat::Signed { precision } => {
                format!("[-+]?(?P<digits>[0-9]{{{precision}}})")
            }
            NumberFormat::Unsigned { precision: 0 } => {
                format!("(?P<digits>[0-9]{{1,{MAX_DIGITS}}})")
            }
            NumberFormat::Unsigned { precision } => {
                format!("(?P<digits>[0-9]{{{precision}}})")
            }
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
            } => {
                format!("0x(?P<digits>[A-Fa-f0-9]{{1,{MAX_DIGITS}}})")
            }
            NumberFormat::Hex {
                require_prefix: true,
                precision,
            } => {
                format!("0x(?P<digits>[A-Fa-f0-9]{{{precision}}})")
            }
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
            } => {
                format!("(?P<digits>[A-Fa-f0-9]{{1,{MAX_DIGITS}}})")
            }
            NumberFormat::Hex {
                require_prefix: false,
                precision,
            } => {
                format!("(?P<digits>[A-Fa-f0-9]{{{precision}}})")
            }
        }
    }
}
impl Default for NumberFormat {
    fn default() -> Self {
        Self::Unsigned { precision: 0 }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum FormatSpecifier {
    #[default]
    Unsigned,
    Signed,
    Hex,
}
