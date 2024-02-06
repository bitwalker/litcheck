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
}

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
                    .parse::<i64>()
                    .map(|value| Self {
                        span,
                        format,
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: error.kind().clone(),
                    })?;
                if precision == 0 {
                    return Ok(value);
                }
                if input.len() != precision as usize {
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
                    .parse::<i64>()
                    .map(|value| Self {
                        span,
                        format,
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: error.kind().clone(),
                    })?;
                if precision == 0 {
                    return Ok(value);
                }
                let actual = if let Some(input) = input.strip_prefix(['-', '+']) {
                    input.len()
                } else {
                    input.len()
                };
                if actual != precision as usize {
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
            } => {
                let input = match input.strip_prefix("0x") {
                    None if require_prefix => return Err(ParseNumberError::MissingPrefix { span }),
                    None => input,
                    Some(input) => input,
                };
                let value = i64::from_str_radix(input, 16)
                    .map(|value| Self {
                        span,
                        format,
                        value,
                    })
                    .map_err(|error| ParseNumberError::InvalidFormat {
                        span,
                        reason: error.kind().clone(),
                    })?;
                if input.len() != precision as usize {
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
        Some(self.value.cmp(&other.value))
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
#[repr(u8)]
pub enum NumberFormat {
    Unsigned { precision: u8 },
    Signed { precision: u8 },
    Hex { precision: u8, require_prefix: bool },
}
impl NumberFormat {
    pub fn describe(&self) -> Cow<'static, str> {
        match self {
            Self::Unsigned { precision: 0 } => Cow::Borrowed("any unsigned 64-bit integer"),
            Self::Unsigned { precision } => {
                Cow::Owned(format!("an unsigned {precision}-digit 64-bit integer"))
            }
            Self::Signed { precision: 0 } => Cow::Borrowed("any signed 64-bit integer"),
            Self::Signed { precision } => {
                Cow::Owned(format!("a signed {precision}-digit 64-bit integer"))
            }
            Self::Hex {
                require_prefix: true,
                precision: 0,
            } => Cow::Borrowed("any 64-bit integer in hex format, prefixed with 0x"),
            Self::Hex {
                require_prefix: false,
                precision: 0,
            } => Cow::Borrowed("any 64-bit integer in hex format"),
            Self::Hex {
                require_prefix: true,
                precision,
            } => Cow::Owned(format!(
                "a {precision}-digit 64-bit integer in hex format, prefixed with 0x"
            )),
            Self::Hex { precision, .. } => {
                Cow::Owned(format!("a {precision}-digit 64-bit integer in hex format"))
            }
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
        // NOTE: The patterns below with precision 0 have their
        // range capped at 19, which is the maximum number of digits
        // in i64::MAX, or the largest possible number that could
        // be represented in decimal form
        match self {
            NumberFormat::Signed { precision: 0 } => Cow::Borrowed(r"(?:[-+]?[0-9]{1,19})"),
            NumberFormat::Signed { precision } => {
                Cow::Owned(format!("(?:[-+]?[0-9]{{{precision}}})"))
            }
            NumberFormat::Unsigned { precision: 0 } => Cow::Borrowed(r"(?:[0-9]{1,19})"),
            NumberFormat::Unsigned { precision } => Cow::Owned(format!("(?:[0-9]{{{precision}}})")),
            // The hex value for i64::MAX is 7fffffffffffffff,
            // or 16 digits
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
            } => Cow::Borrowed(r"(?:0x[A-Fa-f0-9]{1,16})"),
            NumberFormat::Hex {
                require_prefix: true,
                precision,
            } => Cow::Owned(format!("(?:0x[A-Fa-f0-9]{{{precision}}})")),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
            } => Cow::Borrowed(r"(?:[A-Fa-f0-9]{1,16})"),
            NumberFormat::Hex {
                require_prefix: false,
                precision,
            } => Cow::Owned(format!("(?:[A-Fa-f0-9]{{{precision}}})")),
        }
    }

    pub fn pattern(&self, group_name_override: Option<&str>) -> Cow<'static, str> {
        // NOTE: The patterns below with precision 0 have their
        // range capped at 19, which is the maximum number of digits
        // in i64::MAX, or the largest possible number that could
        // be represented in decimal form
        let group_name = group_name_override.unwrap_or("digits");
        match self {
            NumberFormat::Signed { precision: 0 } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>[-+]?[0-9]{1,19})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>[-+]?[0-9]{{1,19}})")),
            },
            NumberFormat::Signed { precision } => {
                Cow::Owned(format!("(?P<{group_name}>[-+]?[0-9]{{{precision}}})"))
            }
            NumberFormat::Unsigned { precision: 0 } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>[0-9]{1,19})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>[0-9]{{1,19}})")),
            },
            NumberFormat::Unsigned { precision } => {
                Cow::Owned(format!("(?P<{group_name}>[0-9]{{{precision}}})"))
            }
            // The hex value for i64::MAX is 7fffffffffffffff,
            // or 16 digits
            NumberFormat::Hex {
                require_prefix: true,
                precision: 0,
            } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>0x[A-Fa-f0-9]{1,16})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>0x[A-Fa-f0-9]{{1,16}})")),
            },
            NumberFormat::Hex {
                require_prefix: true,
                precision,
            } => Cow::Owned(format!("(?P<{group_name}>0x[A-Fa-f0-9]{{{precision}}})")),
            NumberFormat::Hex {
                require_prefix: false,
                precision: 0,
            } => match group_name_override {
                None => Cow::Borrowed(r"(?P<digits>[A-Fa-f0-9]{1,16})"),
                Some(group_name) => Cow::Owned(format!("(?P<{group_name}>[A-Fa-f0-9]{{1,16}})")),
            },
            NumberFormat::Hex {
                require_prefix: false,
                precision,
            } => Cow::Owned(format!("(?P<{group_name}>[A-Fa-f0-9]{{{precision}}})")),
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
