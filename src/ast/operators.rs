use crate::lexer::{TType, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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

    //Bitwise
    Shr,
    Shl,
    Xor,
    BitAnd,
    BitOr,

    AddAssign,
    SubAssign,
    DivAssign,
    ModAssign,
    MulAssign,

    Access,
    Scope,

    // Assignment
    Assign,
}

impl BinaryOp {
    pub fn new(token: &Token) -> Self {
        match token.token_type {
            TType::Plus => BinaryOp::Add,
            TType::Minus => BinaryOp::Sub,
            TType::Star => BinaryOp::Mul,
            TType::Slash => BinaryOp::Div,
            TType::Percentage => BinaryOp::Mod,
            TType::Eq => BinaryOp::Eq,
            TType::Neq => BinaryOp::Neq,
            TType::Lt => BinaryOp::Lt,
            TType::Gt => BinaryOp::Gt,
            TType::Lte => BinaryOp::Leq,
            TType::Gte => BinaryOp::Geq,
            TType::And => BinaryOp::And,
            TType::Or => BinaryOp::Or,
            TType::Scope => BinaryOp::Scope,
            TType::Rightshift => BinaryOp::Shr,
            TType::Leftshift => BinaryOp::Shl,
            TType::Xor => BinaryOp::Xor,
            TType::BitwiseAnd => BinaryOp::BitAnd,
            TType::BitwiseOr => BinaryOp::BitOr,
            TType::Assign => BinaryOp::Assign,
            TType::Coalesce => BinaryOp::Coalesce,
            TType::CompoundAdd => BinaryOp::AddAssign,
            TType::CompoundSub => BinaryOp::SubAssign,
            TType::CompoundModulo => BinaryOp::ModAssign,
            TType::CompoundMul => BinaryOp::MulAssign,
            TType::CompoundDiv => BinaryOp::DivAssign,
            TType::Dot => BinaryOp::Access,
            _ => panic!("Not a binary operator: {:?}", token.token_type),
        }
    }

    pub fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TType::Plus
                | TType::Minus
                | TType::Star
                | TType::Slash
                | TType::Percentage
                | TType::Eq
                | TType::Neq
                | TType::Lt
                | TType::Gt
                | TType::Lte
                | TType::Gte
                | TType::And
                | TType::Or
                | TType::Scope
                | TType::BitwiseAnd
                | TType::BitwiseOr
                | TType::Xor
                | TType::Rightshift
                | TType::Leftshift
                | TType::Assign
                | TType::CompoundAdd
                | TType::CompoundDiv
                | TType::CompoundSub
                | TType::CompoundMul
                | TType::CompoundModulo
                | TType::Coalesce
                | TType::Dot
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,         // -
    Not,         // !
    Increment,   // ++
    Decrement,   //--
    AddressOf,   //@
    Dereference, //^
}

impl UnaryOp {
    pub fn new(token: &Token) -> Self {
        match token.token_type {
            TType::Minus => UnaryOp::Neg,
            TType::Bang => UnaryOp::Not,
            TType::MinusMinus => UnaryOp::Decrement,
            TType::PlusPlus => UnaryOp::Increment,
            TType::At => UnaryOp::AddressOf,
            TType::Caret => UnaryOp::Dereference,
            _ => panic!("Not a unary operator: {:?}", token.token_type),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    Decrement, // --
    Increment, // ++
    Propagate, //'!?'
}

impl PostfixOp {
    pub fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TType::Lparen | TType::PlusPlus | TType::MinusMinus | TType::Propagate
        )
    }
}
