#[derive(Debug, Clone)]
pub enum PrimitiveValue<'s> {
    S32(i32),
    S64(i64),
    F32(f32),
    F64(f64),
    Str(&'s str),
}

#[derive(Debug, Clone)]
pub enum PrimitiveOperation<T> {
    Binary(BinOp, T, T),
    Unary(UnOp, T),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Apply,

    LogOr,
    LogAnd,

    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Recv,

    BitOr,
    BitXor,
    BitAnd,

    Shl,
    Shr,

    Add,
    Sub,

    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Neg,
}
