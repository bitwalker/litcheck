mod cli;
mod error;
pub mod num;
mod parser;
mod value;

pub use self::cli::CliVariable;
pub use self::error::*;
pub use self::num::{CasingStyle, FormatSpecifier, Number, NumberFormat, ParseNumberError};
pub use self::value::{Value, ValueType};
pub use litcheck::variables::VariableName;

use litcheck::{Symbol, variables};

use crate::common::*;

pub type Variable<'a> = variables::Variable<Value<'a>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedVariable {
    pub name: VariableName,
    pub ty: ValueType,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(Number),
    Var(VariableName),
    Binary {
        span: SourceSpan,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}
impl Spanned for Expr {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Num(spanned) => spanned.span(),
            Self::Var(spanned) => spanned.span(),
            Self::Binary { span, .. } => *span,
        }
    }
}
impl Expr {
    pub fn from_call(
        span: SourceSpan,
        callee: Span<Symbol>,
        mut args: Vec<Expr>,
    ) -> Result<Self, InvalidCallExprError> {
        match args.len() {
            2 => {
                let op = match callee.as_str() {
                    "add" => BinaryOp::Add,
                    "sub" => BinaryOp::Sub,
                    "mul" => BinaryOp::Mul,
                    "div" => BinaryOp::Div,
                    "min" => BinaryOp::Min,
                    "max" => BinaryOp::Max,
                    callee => {
                        return Err(InvalidCallExprError::Undefined {
                            span,
                            callee: callee.to_string(),
                        });
                    }
                };
                let rhs = Box::new(args.pop().unwrap());
                let lhs = Box::new(args.pop().unwrap());
                Ok(Self::Binary { span, op, lhs, rhs })
            }
            arity => match callee.as_str() {
                callee @ ("add" | "sub" | "mul" | "div" | "min" | "max") => {
                    Err(InvalidCallExprError::InvalidArity {
                        span,
                        callee: callee.to_string(),
                        expected: 2,
                        given: arity as u8,
                    })
                }
                callee => Err(InvalidCallExprError::Undefined {
                    span,
                    callee: callee.to_string(),
                }),
            },
        }
    }
}
impl Eq for Expr {}
impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a == b,
            (Self::Var(a), Self::Var(b)) => a == b,
            (
                Self::Binary {
                    op: aop,
                    lhs: al,
                    rhs: ar,
                    ..
                },
                Self::Binary {
                    op: bop,
                    lhs: bl,
                    rhs: br,
                    ..
                },
            ) => aop == bop && al == bl && ar == br,
            _ => false,
        }
    }
}
impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Expr {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        use core::cmp::Ordering;
        match (self, other) {
            (Self::Num(l), Self::Num(r)) => l.cmp(r),
            (Self::Num(_), _) => Ordering::Less,
            (_, Self::Num(_)) => Ordering::Greater,
            (Self::Var(l), Self::Var(r)) => l.cmp(r),
            (Self::Var(_), _) => Ordering::Less,
            (_, Self::Var(_)) => Ordering::Greater,
            (
                Self::Binary {
                    op: lop,
                    lhs: ll,
                    rhs: lr,
                    ..
                },
                Self::Binary {
                    op: rop,
                    lhs: rl,
                    rhs: rr,
                    ..
                },
            ) => lop
                .cmp(rop)
                .then_with(|| ll.cmp(rl))
                .then_with(|| lr.cmp(rr)),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(variables::VariableName::Global(v)) => write!(f, "${v}"),
            Self::Var(variables::VariableName::Pseudo(v)) => write!(f, "@{v}"),
            Self::Var(variables::VariableName::User(v)) => write!(f, "{v}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::Binary { op, lhs, rhs, .. } => {
                if matches!(op, BinaryOp::Min | BinaryOp::Max) {
                    write!(f, "{op}({lhs}, {rhs})")
                } else {
                    write!(f, "{lhs} {op} {rhs}")
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eq => f.write_str("=="),
            Self::Add => f.write_str("+"),
            Self::Sub => f.write_str("-"),
            Self::Mul => f.write_str("*"),
            Self::Div => f.write_str("/"),
            Self::Min => f.write_str("min"),
            Self::Max => f.write_str("max"),
        }
    }
}
