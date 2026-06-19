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
    BitwiseXor, //^
    BitwiseAnd, //&
    Shift,      //<< >>
    Term,       // + -
    Factor,     // "* /"
    Unary,      // "! - ~"
    Postfix,
    Call, // . () @
    Primary,
}

impl Precedence {
    pub fn token_precedence(ttype: &TType) -> Self {
        match ttype {
            TType::Plus | TType::Minus => Precedence::Term,
            TType::Eq | TType::Neq | TType::Lt | TType::Gt | TType::Lte | TType::Gte => {
                Precedence::Comparison
            }
            TType::Bang | TType::Tilde => Precedence::Unary,
            TType::Leftshift | TType::Rightshift => Precedence::Shift,
            TType::Star | TType::Slash | TType::Percentage => Precedence::Factor,
            _ => Precedence::Lowest,
        }
    }
}
