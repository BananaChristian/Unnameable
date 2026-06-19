use crate::{
    lexer::{TType, token::Token},
    parser::{
        Stmt,
        ast::{BinaryOp, Expr, Literal, UnaryOp},
    },
};

pub struct Parser {
    tokens: Vec<Token>,
    current_pos: usize,
    next_pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens,
            current_pos: 0,
            next_pos: 1,
        }
    }

    pub fn advance(&mut self) {
        if self.current_pos < self.tokens.len() {
            self.current_pos += 1
        }
    }

    pub fn current_token(&self) -> Option<&Token> {
        if self.current_pos < self.tokens.len() {
            Some(&self.tokens[self.current_pos])
        } else {
            None
        }
    }

    pub fn match_token(&mut self, expected: TType) -> bool {
        if let Some(token) = self.current_token() {
            if token.token_type == expected {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();

        while self.current_pos < self.tokens.len() {
            if let Some(token) = self.current_token() {
                if token.token_type == TType::End {
                    break;
                }

                // Parse the statement
                match self.parse_stmt() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.advance();
                        return Err(e);
                    }
                }
            } else {
                break;
            }
        }

        Ok(stmts)
    }

    pub fn is_binary_operator(&self, tt: &TType) -> bool {
        matches!(
            tt,
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
        )
    }

    pub fn map_to_binary_op(&self, tt: &TType) -> BinaryOp {
        match tt {
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
            _ => panic!("Not a binary operator: {:?}", tt),
        }
    }

    pub fn map_to_unary_op(&self, tt: &TType) -> UnaryOp {
        match tt {
            TType::Minus => UnaryOp::Neg,
            TType::Bang => UnaryOp::Not,
            _ => panic!("Not a unary operator: {:?}", tt),
        }
    }
}
