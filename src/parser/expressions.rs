use crate::{
    ast::{BinaryOp, Expr, ExprKind, InstParam, Literal, PostfixOp, Precedence, Stmt, UnaryOp},
    diagnostics::Span,
    lexer::{TType, token::Token},
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self, min_prec: Precedence) -> Option<Expr> {
        let mut left = self.parse_prefix()?;

        while let Some(token) = self.current_token() {
            let token_type = token.token_type;

            if Stmt::is_valid(token) || token_type == TType::End {
                break;
            }

            if token_type == TType::Semicolon || token_type == TType::Comma {
                break;
            }

            // postfix check BEFORE binary gate
            if PostfixOp::is_valid(token) {
                left = self.parse_postfix(left)?;
                continue;
            }

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

    fn parse_postfix(&mut self, left: Expr) -> Option<Expr> {
        let token = self.current_token()?.clone();
        let span = Span {
            start: left.span.start,
            end: token.span.end,
        };
        match token.token_type {
            TType::Lparen => self.parse_call(left),
            TType::PlusPlus => {
                self.advance();
                Some(Expr::new(
                    ExprKind::Postfix(Box::new(left), PostfixOp::Increment),
                    span,
                ))
            }
            TType::MinusMinus => {
                self.advance();
                Some(Expr::new(
                    ExprKind::Postfix(Box::new(left), PostfixOp::Decrement),
                    span,
                ))
            }
            TType::Propagate => {
                self.advance();
                Some(Expr::new(
                    ExprKind::Postfix(Box::new(left), PostfixOp::Propagate),
                    span,
                ))
            }
            TType::LBracket => {
                let start = left.span.start;
                self.advance(); // consume [
                let index = self.parse_expression(Precedence::Lowest)?;
                let end = self.current_token()?.span.end;
                self.expect_token(TType::RBracket)?;
                let span = Span { start, end };
                Some(Expr::new(
                    ExprKind::Index {
                        target: Box::new(left),
                        index: Box::new(index),
                    },
                    span,
                ))
            }
            _ => {
                let span = token.span;
                self.report(
                    format!("Unexpected postfix token: {:?}", token.token_type),
                    Some(span),
                );
                self.advance();
                None
            }
        }
    }

    fn parse_binary(&mut self, left: Expr) -> Option<Expr> {
        let operator = self.current_token()?.clone();

        self.advance();
        let op_prec = Precedence::token_precedence(&operator.token_type);
        let right = self.parse_expression(op_prec)?;

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
            | TType::False
            | TType::LBracket => self.parse_literal(),

            TType::Identifier => {
                if self.peek_token()?.token_type == TType::Scope
                    && self.peek_offset_token(2)?.token_type == TType::Lt
                {
                    self.parse_turbofish_inst()
                } else {
                    self.parse_identifier()
                }
            }
            TType::Init => self.parse_init(),
            TType::SizeOf => self.parse_sizeof_expr(),
            TType::Lparen => self.parse_grouping(),

            TType::Minus
            | TType::Bang
            | TType::PlusPlus
            | TType::MinusMinus
            | TType::At
            | TType::Caret => {
                let op = UnaryOp::new(&token);
                let op_span = token.span;
                self.advance();

                let expr = self.parse_prefix()?;
                let span = Span {
                    start: op_span.start,
                    end: expr.span.end,
                };
                Some(Expr::new(ExprKind::Unary(op, Box::new(expr)), span))
            }

            _ => {
                let span = token.span;
                self.report(
                    format!("Unexpected prefix token: {:?}", token.token_type),
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

    pub fn parse_turbofish_inst(&mut self) -> Option<Expr> {
        let mut name = self.parse_identifier()?;
        let start = name.span.start;
        let mut end = name.span.end;

        while self.current_token()?.token_type == TType::Scope {
            if self.peek_token()?.token_type == TType::Lt {
                break;
            }

            self.advance(); // consume '::'
            let next = self.parse_identifier()?;
            end = next.span.end;
            name = Expr::new(
                ExprKind::Path(Box::new(name), Box::new(next)),
                Span { start, end },
            );
        }

        let mut type_params = Vec::new();

        if self.current_token()?.token_type == TType::Scope
            && self.peek_token()?.token_type == TType::Lt
        {
            self.advance(); // consume '::'
            self.advance(); // consume '<'

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

            // Update the final span end to include the closing '>'
            end = self.current_token()?.span.end;
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

    fn parse_call_arguments(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();
        self.expect_token(TType::Lparen)?;
        while self.current_token()?.token_type != TType::Rparen
            && self.current_token()?.token_type != TType::End
        {
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
            let expr = self.parse_expression(Precedence::Lowest)?;
            args.push(expr);
        }

        self.expect_token(TType::Rparen)?;
        Some(args)
    }

    fn parse_call(&mut self, left: Expr) -> Option<Expr> {
        let args = self.parse_call_arguments()?;
        let end = self.current_token()?.span.end;

        let span = Span {
            start: left.span.start,
            end: end,
        };
        Some(Expr {
            kind: ExprKind::Call(Box::new(left), args),
            span,
        })
    }

    fn parse_sizeof_expr(&mut self) -> Option<Expr> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::SizeOf)?;

        self.expect_token(TType::Lt)?;
        let ty = self.parse_type()?;
        self.expect_token(TType::Gt)?;
        let end = self.current_token()?.span.end;
        Some(Expr::new(
            ExprKind::SizeOfExpr(Box::new(ty)),
            Span { start, end },
        ))
    }

    fn parse_init_param(&mut self) -> Option<InstParam> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Dot)?;
        let name = self.parse_identifier()?;
        self.expect_token(TType::Colon)?;
        let value = self.parse_expression(Precedence::Lowest)?;
        let end = self.current_token()?.span.end;
        Some(InstParam {
            name: Box::new(name),
            value: Box::new(value),
            span: Span { start, end },
        })
    }

    fn parse_init(&mut self) -> Option<Expr> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Init)?;
        let ty = self.parse_type()?;
        self.expect_token(TType::LBrace)?;
        let mut params = Vec::new();
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            let param = self.parse_init_param()?;
            params.push(param);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
        }
        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rbrace)?;

        Some(Expr::new(
            ExprKind::Instantiation {
                init_ty: Box::new(ty),
                body: params,
            },
            Span { start, end },
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
            TType::LBracket => self.parse_array_literal(),
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

    fn parse_array_literal(&mut self) -> Option<Expr> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::LBracket)?;
        let mut elements = Vec::new();
        while self.current_token()?.token_type != TType::RBracket
            && self.current_token()?.token_type != TType::End
        {
            let element = self.parse_expression(Precedence::Lowest)?;
            elements.push(element);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
        }

        let end = self.current_token()?.span.end;
        self.expect_token(TType::RBracket)?;
        Some(Expr::new(
            ExprKind::Literal(Literal::ArrayLiteral(elements)),
            Span { start, end },
        ))
    }
}
