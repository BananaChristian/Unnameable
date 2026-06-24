use crate::{
    ast::{Precedence, Qualifier, Stmt, Type},
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
            tokens,
            current_pos: 0,
            diagnostics,
            corrupted: false,
        }
    }

    pub fn advance(&mut self) {
        if self.current_pos < self.tokens.len() {
            self.current_pos += 1;
        }
    }

    pub fn current_token(&self) -> Option<&Token> {
        if self.current_pos < self.tokens.len() {
            Some(&self.tokens[self.current_pos])
        } else {
            None
        }
    }

    pub fn peek_token(&self) -> Option<&Token> {
        if self.current_pos + 1 < self.tokens.len() {
            Some(&self.tokens[self.current_pos + 1])
        } else {
            None
        }
    }

    pub fn replace_token(&mut self, replacement: Token) -> Option<()> {
        if self.current_pos < self.tokens.len() {
            self.tokens[self.current_pos] = replacement;
            Some(())
        } else {
            None
        }
    }

    pub fn insert_token(&mut self, token: Token) {
        self.tokens.insert(self.current_pos + 1, token);
    }

    pub fn split_rightshift(&mut self) {
        let tok = match self.current_token() {
            Some(t) => t.clone(),
            None => return,
        };

        if tok.token_type == TType::Rightshift {
            // Replace >> with >
            let replacement = Token::new(
                ">".to_string(),
                TType::Gt,
                Span {
                    start: tok.span.start,
                    end: tok.span.end,
                },
            );
            self.replace_token(replacement);

            // Insert another > after it
            let extra = Token::new(
                ">".to_string(),
                TType::Gt,
                Span {
                    start: tok.span.start,
                    end: tok.span.end,
                },
            );
            self.insert_token(extra);
        }
    }

    pub fn expect_token(&mut self, expected: TType) -> Option<()> {
        let token = self.current_token()?.clone();
        if token.token_type == expected {
            self.advance();
            Some(())
        } else {
            let span = token.span;
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
                self.advance();
                self.expect_token(TType::Lt)?;
                let inner_type = self.parse_type()?;

                self.split_rightshift();

                self.expect_token(TType::Gt)?;
                Some(Type::complex(&token, inner_type))
            }
            TType::Ref => {
                self.advance();
                self.expect_token(TType::Lt)?;
                let inner_type = self.parse_type()?;

                self.split_rightshift();

                self.expect_token(TType::Gt)?;
                Some(Type::complex(&token, inner_type))
            }
            TType::Identifier => {
                let expr = self.parse_generic_inst()?;
                Some(Type::custom(expr))
            }
            TType::Arr => {
                self.advance();
                self.expect_token(TType::LBracket)?;
                let inner_type = self.parse_type()?;
                let mut arr_size = None;
                if self.current_token()?.token_type == TType::Comma {
                    self.advance(); // Consume the ,
                    arr_size = self.parse_expression(Precedence::Lowest);
                }

                self.expect_token(TType::RBracket)?;
                Some(Type::array(inner_type, arr_size))
            }
            TType::Func => {
                let start = self.current_token()?.span.start;
                self.advance(); //Consume the func token
                let mut fields = Vec::new();
                if self.current_token()?.token_type == TType::Lparen {
                    self.advance();
                    while self.current_token()?.token_type != TType::Rparen
                        && self.current_token()?.token_type != TType::End
                    {
                        if let Some(ty) = self.parse_type() {
                            fields.push(ty);
                        } else {
                            self.report(
                                "Expected a type as a parameter".to_string(),
                                Some(self.current_token()?.clone().span),
                            );
                            self.advance();
                        }

                        if self.current_token()?.token_type == TType::Comma {
                            self.advance();
                        }
                    }
                    self.expect_token(TType::Rparen)?;
                }

                let mut return_type = None;
                if self.current_token()?.token_type == TType::Colon {
                    self.advance();
                    return_type = self.parse_type();
                }

                let end = self.current_token()?.span.end;
                let span = Span { start, end };

                Some(Type::funcptr(fields, return_type, span))
            }
            TType::Lparen => {
                let start = self.current_token()?.span.start;
                self.advance(); //(
                if self.current_token()?.token_type == TType::Rparen {
                    let end = self.current_token()?.span.end;
                    let span = Span { start, end };
                    Some(Type::unit(span))
                } else {
                    let inner = self.parse_type()?;
                    self.expect_token(TType::Rparen)?;
                    self.expect_token(TType::QuestionMark)?;
                    let end = self.current_token()?.span.end;
                    let span = Span { start, end };
                    Some(Type::nullable(inner, span))
                }
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

        while let Some(token) = self.current_token() {
            match token.token_type {
                TType::Mut | TType::Const | TType::Heap => {
                    let qualifier = Qualifier::new(&token);
                    qualifiers.push(qualifier);
                    self.advance();
                }
                _ => break,
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
