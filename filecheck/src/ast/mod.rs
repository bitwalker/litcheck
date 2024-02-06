mod directive;
mod file;
mod modifier;
mod pattern;

pub use self::directive::{Check, CheckType};
pub use self::file::{CheckFile, CheckLine};
pub use self::modifier::CheckModifier;
pub use self::pattern::{
    Capture, CheckPattern, CheckPatternPart, Constraint, Match, Prefix, RegexPattern,
};

use crate::expr::{BinaryOp, Expr, VariableName};

pub(crate) struct ConstraintExpr {
    pub constraint: Constraint,
    pub expr: Expr,
}

pub(crate) struct BinaryOpAndOperand {
    pub op: BinaryOp,
    pub operand: Box<Expr>,
}

pub(crate) struct CaptureNumeric {
    pub name: VariableName,
    pub constraint: Constraint,
    pub expr: Option<Expr>,
}

pub(crate) enum CaptureNumericOrExpr {
    Capture(CaptureNumeric),
    Expr(Expr),
}
