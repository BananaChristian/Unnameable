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

    //Bitwise
    Shr,
    Shl,
    Xor,
    BitAnd,
    BitOr,

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
            TType::Assign => BinaryOp::Assign,
            TType::Scope => BinaryOp::Scope,
            TType::Rightshift => BinaryOp::Shr,
            TType::Leftshift => BinaryOp::Shl,
            TType::Xor => BinaryOp::Xor,
            TType::BitwiseAnd => BinaryOp::BitAnd,
            TType::BitwiseOr => BinaryOp::BitOr,
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
                | TType::Assign
                | TType::Scope
                | TType::BitwiseAnd
                | TType::BitwiseOr
                | TType::Xor
                | TType::Rightshift
                | TType::Leftshift
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg, // -
    Not, // !
    Increment, // ++
    Decrement, //--
}

impl UnaryOp {
    pub fn new(token: &Token) -> Self {
        match token.token_type {
            TType::Minus => UnaryOp::Neg,
            TType::Bang => UnaryOp::Not,
            TType::MinusMinus => UnaryOp::Decrement,
            TType::PlusPlus=>UnaryOp::Increment,
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
