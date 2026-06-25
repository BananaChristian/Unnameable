use crate::{
    ast::{BinaryOp, Expr, ExprKind, Literal, Precedence, Stmt, UnaryOp},
    diagnostics::Span,
    lexer::{TType, token::Token},
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self, min_prec: Precedence) -> Option<Expr> {
        let mut left = self.parse_prefix()?;

        while let Some(token) = self.current_token() {
            let token_type = token.token_type;

            // Stop at statement boundaries
            if Stmt::is_valid(token) || token_type == TType::End {
                break;
            }

            // Stop at semicolon or comma
            if token_type == TType::Semicolon || token_type == TType::Comma {
                self.advance();
                break;
            }

            // Check if it's a binary operator
            if !BinaryOp::is_valid(token) {
                break;
            }

            let op_prec = Precedence::token_precedence(&token_type);

            if op_prec < min_prec {
                break;
            }

            left = self.parse_binary(left)?;
        }

        Some(left)
    }

    fn parse_binary(&mut self, left: Expr) -> Option<Expr> {
        let operator = self.current_token()?.clone();

        self.advance();
        let right = self.parse_expression(Precedence::Lowest)?;

        let span = Span {
            start: left.span.start,
            end: right.span.end,
        };
        let op = BinaryOp::new(&operator);

        Some(Expr::new(
            ExprKind::Binary(Box::new(left), op, Box::new(right)),
            span,
        ))
    }

    fn parse_prefix(&mut self) -> Option<Expr> {
        let token = self.current_token()?.clone();

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
            | TType::True
            | TType::False => self.parse_literal(),

            TType::Identifier => {
                if self.peek_token()?.token_type == TType::Lt {
                    self.parse_generic_inst()
                } else {
                    self.parse_identifier()
                }
            }

            TType::Lparen => self.parse_grouping(),

            TType::Minus | TType::Bang => {
                let op = UnaryOp::new(&token);
                let op_span = token.span;
                self.advance();

                let expr = self.parse_expression(Precedence::Unary)?;
                let span = Span {
                    start: op_span.start,
                    end: expr.span.end,
                };
                Some(Expr::new(ExprKind::Unary(op, Box::new(expr)), span))
            }

            _ => {
                let span = token.span;
                self.report(
                    format!("Unexpected token: {:?}", token.token_type),
                    Some(span),
                );
                self.advance();
                None
            }
        }
    }

    fn parse_grouping(&mut self) -> Option<Expr> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Lparen)?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rparen)?;

        let span = Span { start, end };
        Some(Expr::new(expr.kind.clone(), span))
    }

    pub fn parse_identifier(&mut self) -> Option<Expr> {
        let token = self.current_token()?.clone();
        let name = token.lexeme.clone();
        let span = token.span;
        self.expect_token(TType::Identifier)?;
        Some(Expr::new(ExprKind::Identifier(name), span))
    }

    pub fn parse_generic_inst(&mut self) -> Option<Expr> {
        let mut name = self.parse_identifier()?;
        let start = name.span.start;
        let mut end = name.span.end;

        while self.current_token()?.token_type == TType::Scope {
            self.advance(); // consume ::
            let next = self.parse_identifier()?;
            end = next.span.end;
            name = Expr::new(
                ExprKind::Path(Box::new(name), Box::new(next)),
                Span { start, end },
            );
        }

        let mut type_params = Vec::new();
        if self.current_token()?.token_type == TType::Lt {
            self.advance();
            while self.current_token()?.token_type != TType::Gt
                && self.current_token()?.token_type != TType::End
            {
                if self.current_token()?.token_type == TType::Comma {
                    self.advance();
                    continue;
                }
                let ty = self.parse_type()?;
                type_params.push(ty);
            }
            self.expect_token(TType::Gt)?;
        }

        let span = Span { start, end };
        Some(Expr::new(
            ExprKind::GenericInstantion {
                name: Box::new(name),
                type_params,
            },
            span,
        ))
    }
    fn parse_number(&self, num_str: &str) -> Option<i64> {
        if num_str.starts_with("0x") || num_str.starts_with("0X") {
            let hex_str = &num_str[2..];
            i64::from_str_radix(hex_str, 16).ok()
        } else if num_str.starts_with("0b") || num_str.starts_with("0B") {
            let bin_str = &num_str[2..];
            i64::from_str_radix(bin_str, 2).ok()
        } else if num_str.starts_with("0o") || num_str.starts_with("0O") {
            let oct_str = &num_str[2..];
            i64::from_str_radix(oct_str, 8).ok()
        } else {
            num_str.parse::<i64>().ok()
        }
    }

    fn parse_float(&self, num_str: &str) -> Option<f64> {
        num_str.parse::<f64>().ok()
    }

    pub fn get_value(&mut self, token: &Token) -> Option<Literal> {
        let lexeme = &token.lexeme;

        match token.token_type {
            TType::Int => {
                let value = self.parse_number(lexeme)?;
                Some(Literal::Int(value))
            }
            TType::Int8 => {
                let value = self.parse_number(lexeme)? as i8;
                Some(Literal::Int8(value))
            }
            TType::Uint8 => {
                let value = self.parse_number(lexeme)? as u8;
                Some(Literal::Uint8(value))
            }
            TType::Int16 => {
                let value = self.parse_number(lexeme)? as i16;
                Some(Literal::Int16(value))
            }
            TType::Uint16 => {
                let value = self.parse_number(lexeme)? as u16;
                Some(Literal::Uint16(value))
            }
            TType::Int32 => {
                let value = self.parse_number(lexeme)? as i32;
                Some(Literal::Int32(value))
            }
            TType::Uint32 => {
                let value = self.parse_number(lexeme)? as u32;
                Some(Literal::Uint32(value))
            }
            TType::Int64 => {
                let value = self.parse_number(lexeme)?;
                Some(Literal::Int64(value))
            }
            TType::Uint64 => {
                let value = self.parse_number(lexeme)? as u64;
                Some(Literal::Uint64(value))
            }
            TType::Int128 => {
                let value = self.parse_number(lexeme)? as i128;
                Some(Literal::Int128(value))
            }
            TType::Uint128 => {
                let value = self.parse_number(lexeme)? as u128;
                Some(Literal::Uint128(value))
            }
            TType::IntSize => {
                let value = self.parse_number(lexeme)? as isize;
                Some(Literal::IntSize(value))
            }
            TType::UintSize => {
                let value = self.parse_number(lexeme)? as usize;
                Some(Literal::UintSize(value))
            }
            TType::Float => {
                let value = self.parse_float(lexeme)?;
                Some(Literal::Float(value))
            }
            TType::F32 => {
                let value = self.parse_float(lexeme)? as f32;
                Some(Literal::F32(value))
            }
            TType::F64 => {
                let value = self.parse_float(lexeme)?;
                Some(Literal::F64(value))
            }
            _ => {
                let span = token.clone().span;
                self.report(
                    format!("Unexpected token type: {:?}", token.token_type),
                    Some(span),
                );
                None
            }
        }
    }

    pub fn parse_literal(&mut self) -> Option<Expr> {
        let token = self.current_token()?.clone();
        let span = token.clone().span;

        match token.token_type {
            TType::True => {
                self.advance();
                Some(Expr::new(ExprKind::Literal(Literal::Bool(true)), span))
            }
            TType::False => {
                self.advance();
                Some(Expr::new(ExprKind::Literal(Literal::Bool(false)), span))
            }
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
            | TType::F64 => {
                let literal = self.get_value(&token)?;
                self.advance();
                Some(Expr::new(ExprKind::Literal(literal), span))
            }
            _ => {
                self.report(
                    format!("Expected literal, found {:?}", token.token_type),
                    Some(span),
                );
                self.advance();
                None
            }
        }
    }
}
