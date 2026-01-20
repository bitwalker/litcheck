use std::{borrow::Cow, convert::Infallible, fmt, str::FromStr};

use crate::expr::{Expr, NumberFormat};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    Undef,
    Str(Cow<'a, str>),
    Num(Expr),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undef => Ok(()),
            Self::Str(s) => f.write_str(s.as_ref()),
            Self::Num(expr) => write!(f, "{expr}"),
        }
    }
}

impl FromStr for Value<'_> {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Ok(Value::Str(Cow::Borrowed("")))
        } else {
            Ok(Value::Str(Cow::Owned(s.to_string())))
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    String,
    Number(Option<NumberFormat>),
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String => f.write_str("string"),
            Self::Number(None) => f.write_str("any number"),
            Self::Number(Some(format)) => f.write_str(&format.describe()),
        }
    }
}

impl PartialOrd for ValueType {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ValueType {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        use core::cmp::Ordering;
        match (self, other) {
            (Self::String, Self::String) => Ordering::Equal,
            (Self::String, _) => Ordering::Less,
            (_, Self::String) => Ordering::Greater,
            (Self::Number(a), Self::Number(b)) => {
                let a = a.unwrap_or_default();
                let b = b.unwrap_or_default();
                if a == b {
                    Ordering::Equal
                } else {
                    let ap = a.precision();
                    let bp = b.precision();
                    if ap == 0 && bp > 0 {
                        Ordering::Greater
                    } else if ap > 0 && bp == 0 {
                        Ordering::Less
                    } else {
                        ap.cmp(&bp)
                            .then_with(|| a.discriminant().cmp(&b.discriminant()))
                    }
                }
            }
        }
    }
}
