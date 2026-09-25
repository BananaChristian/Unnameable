use crate::{
    ast::{
        PostfixOp, Stmt, Type,
        operators::{BinaryOp, UnaryOp},
    },
    diagnostics::Span,
    lexer::{TType, token::Token},
};

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

    // Plain int (no suffix) - default to isize
    Int(isize),

    // Floats
    Float(f64),
    F32(f32),
    F64(f64),

    //String and char literal
    Str(String),
    Char8(u8),
    Char16(u16),
    Char32(char),

    // Booleans
    Bool(bool),

    //Array literal
    ArrayLiteral(Vec<Expr>),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstParam {
    pub name: Box<Expr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard, // _ matches anything, binds nothing
    Literal(Box<Expr>),
    Path {
        type_name: String,
        member: String,
        payloads: Vec<Pattern>,
        span: Span,
    },
    Binding {
        name: String,
        span: Span,
    },
    Tuple {
        elements: Vec<Pattern>,
        span: Span,
    },
    StructPattern {
        type_name: String,
        fields: Vec<StructPatternField>,
        rest: bool,
        span: Span,
    },
    Or(Vec<Pattern>), // a | b | c — same arm handles multiple patterns
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPatternField {
    pub name: String,
    pub pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>, // `Circle(r) if r > 0.0 => ...`
    pub body: Box<Expr>, // a bare expr, OR a Block(Vec<Stmt>) expr if that variant exists/gets added
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Identifier(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Path(Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Unwrap(Box<Expr>),
    GenericInstantion {
        name: Box<Expr>,
        type_params: Vec<Type>,
    },
    Call(Box<Expr>, Vec<Expr>),
    Postfix(Box<Expr>, PostfixOp),
    SizeOfExpr(Box<Type>),
    //bitcast<i32>(x);
    BitcastExpr(Box<Type>, Box<Expr>),
    //cast<i32>(x);
    StaticCast(Box<Type>, Box<Expr>),
    //Struct Instantiation .Struct{.a= val, .b=val};
    Instantiation {
        init_ty: Box<Type>,
        body: Vec<InstParam>,
    },
    //Tuple instantiation
    TupleInst {
        body: Vec<Expr>,
    },
    //The dollar scope $${}
    DollarScope {
        params: Vec<Expr>,
        body: Box<Expr>,
    },
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Block(Vec<Stmt>),
    Marked(Box<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
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

    pub fn is_literal(token: &Token) -> bool {
        match token.token_type {
            TType::Int
            | TType::Int8
            | TType::Uint8
            | TType::Int16
            | TType::Uint16
            | TType::Int32
            | TType::Uint32
            | TType::Int64
            | TType::Uint64
            | TType::Int128
            | TType::Uint128
            | TType::IntSize
            | TType::UintSize
            | TType::Float
            | TType::F32
            | TType::F64
            | TType::StringLiteral
            | TType::Char8Literal
            | TType::Char16Literal
            | TType::Char32Literal
            | TType::True
            | TType::False
            | TType::LBracket => true,
            _ => false,
        }
    }
}
