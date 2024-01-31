mod cli;
mod error;
pub mod num;
mod parser;
mod value;

pub use self::cli::CliVariable;
pub use self::error::*;
pub use self::num::{FormatSpecifier, Number, NumberFormat};
pub use self::value::{Value, ValueType};

use litcheck::variables;

use crate::common::*;

pub type VariableName = variables::VariableName<Symbol>;
pub type Variable<'a> = variables::Variable<Symbol, Value<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Var<'a> {
    pub name: VariableName,
    pub value: Value<'a>,
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
        interner: &mut StringInterner,
        span: SourceSpan,
        callee: Span<Symbol>,
        mut args: Vec<Expr>,
    ) -> Result<Self, InvalidCallExprError> {
        match args.len() {
            2 => {
                let op = match interner.resolve(callee.into_inner()) {
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
                        })
                    }
                };
                let rhs = Box::new(args.pop().unwrap());
                let lhs = Box::new(args.pop().unwrap());
                Ok(Self::Binary { span, op, lhs, rhs })
            }
            arity => match interner.resolve(*callee) {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Min,
    Max,
}
