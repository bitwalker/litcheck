use crate::{
    check::Constraint,
    expr::{BinaryOp, Expr, VariableName},
};

pub struct ConstraintExpr {
    pub constraint: Constraint,
    pub expr: Expr,
}

pub struct BinaryOpAndOperand {
    pub op: BinaryOp,
    pub operand: Box<Expr>,
}

pub struct CaptureNumeric {
    pub name: VariableName,
    pub constraint: Constraint,
    pub expr: Option<Expr>,
}

pub enum CaptureNumericOrExpr {
    Capture(CaptureNumeric),
    Expr(Expr),
}
