#![expect(unused_assignments)]

use crate::common::*;

#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum ParseNumberError {
    #[error("expected unsigned number but found sign")]
    #[diagnostic()]
    UnexpectedSign {
        #[label("occurs here")]
        span: SourceSpan,
    },
    #[error("expected hexadecimal number with 0x prefix, but no prefix was present")]
    #[diagnostic()]
    MissingPrefix {
        #[label("occurs here")]
        span: SourceSpan,
    },
    #[error("input string has incorrect precision: expected {precision} got {actual}")]
    #[diagnostic()]
    PrecisionMismatch {
        #[label("occurs here")]
        span: SourceSpan,
        precision: u8,
        actual: usize,
    },
    #[error("input string has incorrect numeric format: {reason:?}")]
    #[diagnostic()]
    InvalidFormat {
        #[label("occurs here")]
        span: SourceSpan,
        reason: core::num::IntErrorKind,
    },
    #[error("input string has incorrect numeric format: invalid casing style, expected {expected}")]
    #[diagnostic()]
    InvalidCasing {
        #[label("occurs here")]
        span: SourceSpan,
        expected: CasingStyle,
    },
}

#[derive(Debug, Clone)]
pub struct Number {
    pub span: SourceSpan,
    pub format: Option<NumberFormat>,
    pub value: i128,
}
impl Number {
    pub fn new(span: SourceSpan, value: i128) -> Self {
        Self {
            span,
            format: None,
            value,
        }
    }

    pub fn new_with_format(span: SourceSpan, value: i128, format: NumberFormat) -> Self {
        Self {
            span,
            format: Some(format),
            value,
        }
    }

    /// Returns the format that should be applied to an expression involving `self` and `other`.
    ///
    /// Returns `None` if there is an implicit format conflict. Otherwise this returns the format
    /// to apply to the expression result.
    ///
    /// Two numbers are format-compatible if they:
    ///
    /// * Have the same format specifier (i.e. hex, signed, unsigned), or one of the operands
    ///   has no specified format (i.e. it is inferred from the other operand).
    /// * Have the same precision, if specified, or one of the operands is arbitrary precision
    /// * If hex, both require a prefix, or neither do
    /// * If hex, the casing style is compatible, i.e. they don't have conflicting casing
    ///   requirements
    ///
    /// The format returned from this function will take the most precise information from the
    /// given input formats, e.g. if the two numbers are both unsigned integers, but one has a
    /// precision of 6, and the other is arbitrary precision, then the output format will specify
    /// a precision of 6.
    pub fn infer_expression_format(&self, other: &Self) -> Option<NumberFormat> {
        match (self.format, other.format) {
            (format @ Some(_), None) | (None, format @ Some(_)) => format,
            (None, None) => Some(NumberFormat::default()),
            (
                Some(NumberFormat::Hex {
                    require_prefix: lprefixed,
                    casing: lcasing,
                    precision: lprecision,
                }),
                Some(NumberFormat::Hex {
                    require_prefix: rprefixed,
                    casing: rcasing,
                    precision: rprecision,
                }),
            ) if lprefixed == rprefixed
                && (lprecision == rprecision || lprecision == 0 || rprecision == 0)
                && (lcasing == rcasing
                    || lcasing == CasingStyle::Any
                    || rcasing == CasingStyle::Any) =>
            {
                let precision = core::cmp::max(lprecision, rprecision);
                Some(NumberFormat::Hex {
                    precision,
                    require_prefix: lprefixed,
                    casing: lcasing,
                })
            }
            (
                Some(NumberFormat::Signed {
                    precision: lprecision,
                }),
                Some(NumberFormat::Signed {
                    precision: rprecision,
                }),
            ) if lprecision == rprecision || lprecision == 0 || rprecision == 0 => {
                Some(NumberFormat::Signed {
                    precision: core::cmp::max(lprecision, rprecision),
                })
            }
            (
                Some(NumberFormat::Unsigned {
                    precision: lprecision,
                }),
                Some(NumberFormat::Unsigned {
                    precision: rprecision,
                }),
            ) if lprecision == rprecision || lprecision == 0 || rprecision == 0 => {
                Some(NumberFormat::Unsigned {
                    precision: core::cmp::max(lprecision, rprecision),
                })
            }
            _ => None,
        }
    }

    pub fn parse_with_format(
        input: Span<&str>,
        format: NumberFormat,
    ) -> Result<Self, ParseNumberError> {
        let (span, input) = input.into_parts();

        match format {
            NumberFormat::Unsigned { precision } => {
                if input.starts_with(['-', '+']) {
                    return Err(ParseNumberError::UnexpectedSign { span });
                }
                let value = input
                    .parse::<i128>()
                    .map(|value| Self {
                        span,
                        format: Some(format),
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: *error.kind(),
                    })?;
                if precision == 0 {
                    return Ok(value);
                }
                if input.len() < precision as usize {
                    Err(ParseNumberError::PrecisionMismatch {
                        span,
                        precision,
                        actual: input.len(),
                    })
                } else {
                    Ok(value)
                }
            }
            NumberFormat::Signed { precision } => {
                let value = input
                    .parse::<i128>()
                    .map(|value| Self {
                        span,
                        format: Some(format),
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: *error.kind(),
                    })?;
                if precision == 0 {
                    return Ok(value);
                }
                let actual = if let Some(input) = input.strip_prefix(['-', '+']) {
                    input.len()
                } else {
                    input.len()
                };
                if actual < precision as usize {
                    Err(ParseNumberError::PrecisionMismatch {
                        span,
                        precision,
                        actual,
                    })
                } else {
                    Ok(value)
                }
            }
            NumberFormat::Hex {
                require_prefix,
                precision,
                casing,
            } => {
                let input = match input.strip_prefix("0x") {
                    None if require_prefix => return Err(ParseNumberError::MissingPrefix { span }),
                    None => input,
                    Some(input) => input,
                };
                let is_valid_casing = match casing {
                    CasingStyle::Any => true,
                    CasingStyle::Lower => input.chars().all(|c| matches!(c, 'a'..='f' | '0'..='9')),
                    CasingStyle::Upper => input.chars().all(|c| matches!(c, 'A'..='F' | '0'..='9')),
                };
                let value = i128::from_str_radix(input, 16)
                    .map(|value| Self {
                        span,
                        format: Some(format),
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: *error.kind(),
                    })?;
                if !is_valid_casing {
                    Err(ParseNumberError::InvalidCasing {
                        span,
                        expected: casing,
                    })
                } else if precision > 0 && input.len() < precision as usize {
                    Err(ParseNumberError::PrecisionMismatch {
                        span,
                        precision,
                        actual: input.len(),
                    })
                } else {
                    Ok(value)
                }
            }
        }
    }
}
impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.format == other.format
    }
}
impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Number {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.value.cmp(&other.value)
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
        let mut padding = 0;
        let format = self.format.unwrap_or_default();
        match format {
            NumberFormat::Unsigned { precision } => {
                if precision > 0 {
                    padding += precision as usize;
                }
            }
            NumberFormat::Signed { precision } => {
                padding += value.is_negative() as usize;
                if precision > 0 {
                    padding += precision as usize;
                }
            }
            NumberFormat::Hex {
                precision,
                require_prefix,
                ..
            } => {
                padding += (require_prefix as usize) * 2;
                if precision > 0 {
                    padding += precision as usize;
                }
            }
        }
        match format {
            NumberFormat::Unsigned { precision: 0 } => write!(f, "{}", value as u64),
            NumberFormat::Unsigned { .. } => {
                write!(f, "{:0padding$}", value as u64)
            }
            NumberFormat::Signed { precision: 0 } => write!(f, "{value}"),
            NumberFormat::Signed { .. } => write!(f, "{value:0padding$}"),
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "{value:#x?}"),
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
                casing: CasingStyle::Upper,
            } => write!(f, "{value:#X?}"),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "{value:x?}"),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
                casing: CasingStyle::Upper,
            } => write!(f, "{value:X?}"),
            NumberFormat::Hex {
                require_prefix: true,
                casing: CasingStyle::Any | CasingStyle::Lower,
                ..
            } => write!(f, "{value:#0padding$x?}"),
            NumberFormat::Hex {
                require_prefix: true,
                casing: CasingStyle::Upper,
                ..
            } => write!(f, "{value:#0padding$X?}"),
            NumberFormat::Hex {
                require_prefix: false,
                casing: CasingStyle::Any | CasingStyle::Lower,
                ..
            } => write!(f, "{value:0padding$x?}"),
            NumberFormat::Hex {
                require_prefix: false,
                casing: CasingStyle::Upper,
                ..
            } => write!(f, "{value:0padding$X?}"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum NumberFormat {
    Unsigned {
        precision: u8,
    },
    Signed {
        precision: u8,
    },
    Hex {
        precision: u8,
        require_prefix: bool,
        casing: CasingStyle,
    },
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum CasingStyle {
    #[default]
    Any = 0,
    Upper,
    Lower,
}

impl CasingStyle {
    const HEX_ALPHABETS: &[&str] = &["[A-Fa-f0-9]", "[A-F0-9]", "[a-f0-9]"];

    pub const fn as_hex_class(self) -> &'static str {
        Self::HEX_ALPHABETS[self as u8 as usize]
    }
}

impl fmt::Display for CasingStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("either upper or lowercase"),
            Self::Upper => f.write_str("uppercase"),
            Self::Lower => f.write_str("lowercase"),
        }
    }
}

impl NumberFormat {
    pub fn describe(&self) -> Cow<'static, str> {
        match self {
            Self::Unsigned { precision: 0 } => Cow::Borrowed("any unsigned 128-bit integer"),
            Self::Unsigned { precision } => {
                Cow::Owned(format!("an unsigned {precision}-digit 128-bit integer"))
            }
            Self::Signed { precision: 0 } => Cow::Borrowed("any signed 128-bit integer"),
            Self::Signed { precision } => {
                Cow::Owned(format!("a signed {precision}-digit 128-bit integer"))
            }
            Self::Hex {
                require_prefix: true,
                precision: 0,
                casing,
            } => Cow::Owned(format!(
                "any 128-bit integer in {casing} hex format, prefixed with 0x"
            )),
            Self::Hex {
                require_prefix: false,
                precision: 0,
                casing,
            } => Cow::Owned(format!("any 128-bit integer in {casing} hex format")),
            Self::Hex {
                require_prefix: true,
                precision,
                casing,
            } => Cow::Owned(format!(
                "a {precision}-digit 12864-bit integer in {casing} hex format, prefixed with 0x"
            )),
            Self::Hex {
                precision, casing, ..
            } => Cow::Owned(format!(
                "a {precision}-digit 128-bit integer in {casing} hex format"
            )),
        }
    }

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

    pub fn discriminant(&self) -> u8 {
        // SAFETY: Because `Self` is marked `repr(u8)`, its layout is a `repr(C)` `union`
        // between `repr(C)` structs, each of which has the `u8` discriminant as its first
        // field, so we can read the discriminant without offsetting the pointer.
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    pub fn pattern_nocapture(&self) -> Cow<'static, str> {
        // NOTE: The patterns below with precision 0 have their range capped at 39, which is the
        // maximum number of digits in i128::MAX, or the largest possible number that could be
        // represented in decimal form
        match self {
            NumberFormat::Signed { precision: 0 } => Cow::Borrowed(r"(?:[-+]?[0-9]{1,39})"),
            NumberFormat::Signed { precision } => {
                Cow::Owned(format!("(?:[-+]?[0-9]{{{precision},39}})"))
            }
            NumberFormat::Unsigned { precision: 0 } => Cow::Borrowed(r"(?:[0-9]{1,39})"),
            NumberFormat::Unsigned { precision } => {
                Cow::Owned(format!("(?:[0-9]{{{precision},39}})"))
            }
            // The hex value for i128::MAX is 7fffffffffffffffffffffffffffffff or 32 digits
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
                casing,
            } => Cow::Owned(format!("(?:0x{}{{1,32}})", casing.as_hex_class())),
            NumberFormat::Hex {
                require_prefix: true,
                precision,
                casing,
            } => Cow::Owned(format!("(?:0x{}{{{precision},32}})", casing.as_hex_class())),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
                casing,
            } => Cow::Owned(format!(r"(?:{}{{1,32}})", casing.as_hex_class())),
            NumberFormat::Hex {
                require_prefix: false,
                precision,
                casing,
            } => Cow::Owned(format!("(?:{}{{{precision},32}})", casing.as_hex_class())),
        }
    }

    pub fn pattern(&self, group_name_override: Option<&str>) -> Cow<'static, str> {
        // NOTE: The patterns below with precision 0 have their range capped at 39, which is the
        // maximum number of digits in i128::MAX, or the largest possible number that could be
        // represented in decimal form
        let group_name = group_name_override.unwrap_or("digits");
        match self {
            NumberFormat::Signed { precision: 0 } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>[-+]?[0-9]{1,39})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>[-+]?[0-9]{{1,39}})")),
            },
            NumberFormat::Signed { precision } => {
                Cow::Owned(format!("(?P<{group_name}>[-+]?[0-9]{{{precision},39}})"))
            }
            NumberFormat::Unsigned { precision: 0 } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>[0-9]{1,39})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>[0-9]{{1,39}})")),
            },
            NumberFormat::Unsigned { precision } => {
                Cow::Owned(format!("(?P<{group_name}>[0-9]{{{precision},39}})"))
            }
            // The hex value for i128::MAX is 7fffffffffffffffffffffffffffffff or 32 digits
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
                casing,
            } => Cow::Owned(format!(
                "(?P<{group_name}>0x{}{{1,32}})",
                casing.as_hex_class()
            )),
            NumberFormat::Hex {
                require_prefix: true,
                precision,
                casing,
            } => Cow::Owned(format!(
                "(?P<{group_name}>0x{}{{{precision},32}})",
                casing.as_hex_class()
            )),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
                casing,
            } => Cow::Owned(format!(
                "(?P<{group_name}>{}{{1,32}})",
                casing.as_hex_class()
            )),
            NumberFormat::Hex {
                require_prefix: false,
                precision,
                casing,
            } => Cow::Owned(format!(
                "(?P<{group_name}>{}{{{precision},32}})",
                casing.as_hex_class()
            )),
        }
    }
}

impl fmt::Display for NumberFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unsigned { precision: 0 } => f.write_str("%u"),
            Self::Unsigned { precision } => write!(f, "%.{precision}u"),
            Self::Signed { precision: 0 } => f.write_str("d"),
            Self::Signed { precision } => write!(f, "%.{precision}d"),
            Self::Hex {
                precision: 0,
                require_prefix: false,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "%x"),
            Self::Hex {
                precision: 0,
                require_prefix: false,
                casing: CasingStyle::Upper,
            } => write!(f, "%X"),
            Self::Hex {
                precision: 0,
                require_prefix: true,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "%#x"),
            Self::Hex {
                precision: 0,
                require_prefix: true,
                casing: CasingStyle::Upper,
            } => write!(f, "%#X"),
            Self::Hex {
                precision,
                require_prefix: false,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "%.{precision}x"),
            Self::Hex {
                precision,
                require_prefix: false,
                casing: CasingStyle::Upper,
            } => write!(f, "%.{precision}X"),
            Self::Hex {
                precision,
                require_prefix: true,
                casing: CasingStyle::Any | CasingStyle::Lower,
            } => write!(f, "%#.{precision}x"),
            Self::Hex {
                precision,
                require_prefix: true,
                casing: CasingStyle::Upper,
            } => write!(f, "%#.{precision}X"),
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
    Hex(CasingStyle),
}
