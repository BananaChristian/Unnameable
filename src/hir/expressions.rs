use crate::{
    diagnostics::Span,
    hir::{HirStmt, HirTypeNode},
    lowering::NodeId,
};

#[derive(Debug, Clone, PartialEq)]
pub enum HirLiteral {
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
    IntSize(isize),
    UintSize(usize),
    // Untyped int for the type checker to resolve later
    Int(isize),
    // Floats
    Float(f64),
    F32(f32),
    F64(f64),

    Str(String),
    Char8(u8),
    Char16(u16),
    Char32(char),

    // Boolean
    Bool(bool),
    // Array literal
    ArrayLiteral(Vec<HirExpr>),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirBinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    // Logical
    And,
    Or,
    Coalesce,
    // Bitwise
    Shr,
    Shl,
    Xor,
    BitAnd,
    BitOr,
    // Compound assignment
    AddAssign,
    SubAssign,
    DivAssign,
    ModAssign,
    MulAssign,
    // Field access
    Access, //x.y
    // Assignment
    Assign,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirUnaryOp {
    Neg,         // -
    Not,         // !
    BitNot,      // bitwise not
    Increment,   // ++
    Decrement,   // --
    AddressOf,   // @
    Dereference, // ^
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirPostfixOp {
    Increment, // ++
    Decrement, // --
    Propagate, // !?
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirInstParam {
    pub hir_id: NodeId,
    pub name: String,
    pub value: Box<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    // Literal value
    Literal(HirLiteral),

    // Resolved identifier, just a name
    Identifier(String),

    // Binary expression
    Binary(Box<HirExpr>, HirBinaryOp, Box<HirExpr>),

    // Unary expression
    Unary(HirUnaryOp, Box<HirExpr>),

    // Generic instantiation, name is now a resolved mangled string
    GenericInstantion {
        name: String,
        type_params: Vec<HirTypeNode>,
    },

    // Function call ,callee is resolved, no more Path expressions
    Call(Box<HirExpr>, Vec<HirExpr>),

    Unwrap(Box<HirExpr>),

    // Postfix operation
    Postfix(Box<HirExpr>, HirPostfixOp),

    // sizeof expression
    SizeOf(HirTypeNode),

    StaticCast(Box<HirTypeNode>, Box<HirExpr>),
    BitCast(Box<HirTypeNode>, Box<HirExpr>),

    // Struct instantiation, Point .{ .x : 1 }
    Instantiation {
        init_ty: Option<HirTypeNode>,
        body: Vec<HirInstParam>,
    },

    //Dollar scope
    DollarScope {
        params: Vec<HirExpr>,
        body: Vec<HirStmt>,
        result: Option<Box<HirExpr>>, //This is the final result
    },

    // Index access — arr[0]
    Index {
        target: Box<HirExpr>,
        index: Box<HirExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub hir_id: NodeId,
    pub kind: HirExprKind,
    pub span: Span,
}

impl HirExpr {
    pub fn new(id: NodeId, kind: HirExprKind, span: Span) -> Self {
        HirExpr {
            hir_id: id,
            kind,
            span,
        }
    }
}
