use crate::{ast::operators::{BinaryOp, UnaryOp}, diagnostics::Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    // Integers with explicit types
    Int8(i8),
    Uint8(u8),
    Int16(i16),
    Uint16(u16),
    Int32(i32),
    Uint32(u32),
    Int64(i64),
    Uint64(u64),
    Int128(i128),
    Uint128(u128),
    IntSize(isize),  // iz
    UintSize(usize), // uz

    // Plain int (no suffix) - default to i64
    Int(i64),

    // Floats
    Float(f64),
    F32(f32),
    F64(f64),

    // Booleans
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
