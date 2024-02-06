use std::borrow::Cow;

use crate::expr::{Expr, Number, NumberFormat};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    Undef,
    Str(Cow<'a, str>),
    Num(Expr),
}
impl<'a> Value<'a> {
    pub fn unwrap_string(&self) -> Cow<'a, str> {
        match self {
            Self::Undef => Cow::Borrowed(""),
            Self::Str(s) => s.clone(),
            Self::Num(Expr::Num(n)) => Cow::Owned(format!("{n}")),
            Self::Num(expr) => panic!("cannot unwrap expression as string: {expr:?}"),
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Value::Num(Expr::Num(num)) => Some(num.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    String,
    Number(NumberFormat),
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
