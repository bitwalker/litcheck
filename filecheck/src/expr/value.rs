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
