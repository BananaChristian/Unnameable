use crate::{
    lexer::{TType, token::Token},
    parser::{
        Stmt,
        ast::{Expr, Literal},
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
}
