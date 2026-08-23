use crate::lexer::TType;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Assignment, // =
    Coalesce,   //"??"
    Or,         // ||
    And,        // &&
    Equality,   // == !=
    Comparison, // < > <= >=
    BitwiseOr,  //|
    BitwiseXor, //xor
    BitwiseAnd, //and
    Shift,      //shl,shr
    Term,       // + -
    Factor,     // "* /"
    Call,       // .
    Primary,    //::
}

impl Precedence {
    pub fn token_precedence(ttype: &TType) -> Self {
        match ttype {
            TType::Plus | TType::Minus => Precedence::Term,
            TType::Lt | TType::Gt | TType::Lte | TType::Gte => Precedence::Comparison,
            TType::Neq | TType::Eq => Precedence::Equality,
            TType::Leftshift | TType::Rightshift => Precedence::Shift,
            TType::Star | TType::Slash | TType::Percentage => Precedence::Factor,
            TType::Coalesce => Precedence::Coalesce,
            TType::Scope => Precedence::Primary,
            TType::And => Precedence::And,
            TType::Or => Precedence::Or,
            TType::BitwiseAnd => Precedence::BitwiseAnd,
            TType::BitwiseOr => Precedence::BitwiseOr,
            TType::Xor => Precedence::BitwiseXor,
            TType::Dot => Precedence::Call,
            TType::Assign
            | TType::CompoundAdd
            | TType::CompoundSub
            | TType::CompoundModulo
            | TType::CompoundMul
            | TType::CompoundDiv => Precedence::Assignment,
            _ => Precedence::Lowest,
        }
    }

    pub fn next(self) -> Self {
        match self {
            Precedence::Lowest => Precedence::Assignment,
            Precedence::Assignment => Precedence::Coalesce,
            Precedence::Coalesce => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::BitwiseOr,
            Precedence::BitwiseOr => Precedence::BitwiseXor,
            Precedence::BitwiseXor => Precedence::BitwiseAnd,
            Precedence::BitwiseAnd => Precedence::Shift,
            Precedence::Shift => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}
