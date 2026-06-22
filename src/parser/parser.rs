use crate::{
    ast::{Qualifier, Stmt, Type},
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    lexer::{TType, token::Token},
};

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current_pos: usize,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, diagnostics: &'a mut Diagnostics) -> Self {
        Parser {
            tokens: tokens,
            current_pos: 0,
            diagnostics,
            corrupted: false,
        }
    }

    pub fn advance(&mut self) {
        if self.current_pos < self.tokens.len() {
            self.current_pos += 1
        }
    }

    pub fn current_token(&mut self) -> Option<&Token> {
        if self.current_pos < self.tokens.len() {
            Some(&self.tokens[self.current_pos])
        } else {
            None
        }
    }
    
    pub fn next_token(&mut self) -> Option<&Token> {
        if self.current_pos+1 < self.tokens.len() {
            Some(&self.tokens[self.current_pos+1])
        } else {
            None
        }
    }

    pub fn expect_token(&mut self, expected: TType) -> Option<()> {
        let token = self.current_token()?.clone();

        if token.token_type == expected {
            self.advance();
            Some(())
        } else {
            let span = Span::from_token(&token);
            self.report(
                format!("Expected {:?}, found {:?}", expected, token.token_type),
                Some(span),
            );
            None
        }
    }

    pub fn match_token(&mut self, expected: TType) -> bool {
        if let Some(token) = self.current_token() {
            if token.token_type == expected {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();

        while self.current_pos < self.tokens.len() {
            if let Some(token) = self.current_token() {
                if token.token_type == TType::End {
                    break;
                }

                if let Some(stmt) = self.parse_stmt() {
                    stmts.push(stmt);
                } else {
                    // Error already reported, advance to avoid infinite loop
                    self.advance();
                }
            } else {
                break;
            }
        }

        stmts
    }

    pub fn parse_type(&mut self) -> Option<Type> {
        let token = self.current_token()?.clone();
        match token.token_type {
            TType::Ptr => {
                self.advance(); //Consume the ptr token
                self.expect_token(TType::Lt);
                let inner_type = self.parse_type()?;
                self.expect_token(TType::Gt);
                Some(Type::complex(&token, inner_type))
            }
            TType::Ref => {
                self.advance(); //Consum the ref token
                self.expect_token(TType::Lt);
                let inner_type = self.parse_type()?;
                self.expect_token(TType::Gt);
                Some(Type::complex(&token, inner_type))
            }
            TType::ISIZEKey
            | TType::USIZEKey
            | TType::I128Key
            | TType::U128Key
            | TType::I64Key
            | TType::U64Key
            | TType::I32Key
            | TType::U32key
            | TType::I16Key
            | TType::U16Key
            | TType::I8Key
            | TType::U8Key
            | TType::BoolKey
            | TType::F32Key
            | TType::F64Key => {
                self.advance();
                Some(Type::basic(&token))
            }
            _ => None,
        }
    }

    pub fn collect_qualifiers(&mut self) -> Vec<Qualifier> {
        let mut qualifiers = Vec::new();

        // Loop while we have qualifiers
        while let Some(token) = self.current_token() {
            match token.token_type {
                TType::Mut | TType::Const | TType::Heap => {
                    // It's a qualifier! Create it and consume it
                    let qualifier = Qualifier::new(&token);
                    qualifiers.push(qualifier);
                    self.advance();
                }
                _ => {
                    // Not a qualifier, stop collecting
                    break;
                }
            }
        }

        qualifiers
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Parser, span));
    }
}
