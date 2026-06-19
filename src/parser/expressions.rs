use crate::{
    lexer::{
        TType,
        token::{self, Token},
    },
    parser::{
        Expr, Literal, Parser,
        ast::{BinaryOp, UnaryOp},
        precedence::{self, Precedence},
    },
};

impl Parser {
    pub fn parse_expression(&mut self, min_prec: Precedence) -> Result<Expr, String> {
        // Parse the leftmost expression (prefix)
        let mut left = self.parse_prefix()?;

        // While we have a binary operator with higher precedence
        while let Some(token) = self.current_token() {
            if token.token_type == TType::End {
                break;
            }

            if !self.is_binary_operator(&token.token_type) {
                break;
            }

            let op_prec = Precedence::token_precedence(&token.token_type);

            // If the operator has lower precedence than our minimum, stop
            if op_prec < min_prec {
                break;
            }

            // Parse the binary operation
            left = self.parse_binary(left)?;
        }

        Ok(left)
    }

    fn parse_binary(&mut self, left: Expr) -> Result<Expr, String> {
        let token_type = match self.current_token() {
            Some(t) => t.token_type.clone(), // Clone just the token type
            None => return Err("Expected operator".to_string()),
        };

        // Get operator precedence
        let prec = Precedence::token_precedence(&token_type);

        // Advance past the operator
        self.advance();

        // Parse the right side with higher precedence
        let right = self.parse_expression(prec)?;

        // Create the binary expression
        let op = self.map_to_binary_op(&token_type);

        Ok(Expr::Binary(Box::new(left), op, Box::new(right)))
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        let token = match self.current_token() {
            Some(t) => t.clone(),
            None => return Err("Unexpected end of file in parse prefix".to_string()),
        };

        match token.token_type {
            // Literals
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

            TType::Identifier => self.parse_identifier(),

            // Unary
            TType::Minus | TType::Bang => {
                let op = self.map_to_unary_op(&token.token_type);
                self.advance(); // consume '-'
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expr::Unary(op, Box::new(expr)))
            }

            _ => Err(format!(
                "Unexpected token: {:?} at {}:{}",
                token.token_type, token.line, token.col
            )),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expr, String> {
        let token = match self.current_token() {
            Some(t) => t.clone(),
            None => return Err("Unexpected end of file".to_string()),
        };

        let name = token.lexeme.clone();
        self.advance();
        Ok(Expr::Identifier(name))
    }

    fn parse_number(&self, num_str: &str) -> Result<i64, String> {
        if num_str.starts_with("0x") || num_str.starts_with("0X") {
            let hex_str = &num_str[2..];
            i64::from_str_radix(hex_str, 16).map_err(|_| format!("Invalid hex number: {}", num_str))
        } else if num_str.starts_with("0b") || num_str.starts_with("0B") {
            let bin_str = &num_str[2..];
            i64::from_str_radix(bin_str, 2)
                .map_err(|_| format!("Invalid binary number: {}", num_str))
        } else if num_str.starts_with("0o") || num_str.starts_with("0O") {
            let oct_str = &num_str[2..];
            i64::from_str_radix(oct_str, 8)
                .map_err(|_| format!("Invalid octal number: {}", num_str))
        } else {
            num_str
                .parse::<i64>()
                .map_err(|_| format!("Invalid number: {}", num_str))
        }
    }

    fn parse_float(&self, num_str: &str) -> Result<f64, String> {
        num_str
            .parse::<f64>()
            .map_err(|_| format!("Invalid float: {}", num_str))
    }

    pub fn get_value(&self, token: &Token) -> Result<Literal, String> {
        let lexeme = &token.lexeme;

        // Parse the number based on token type
        match token.token_type {
            TType::Int => {
                // Default int -> i64
                let value = self.parse_number(lexeme)?;
                Ok(Literal::Int(value))
            }
            TType::Int8 => {
                let value = self.parse_number(lexeme)? as i8;
                Ok(Literal::Int8(value))
            }
            TType::Uint8 => {
                let value = self.parse_number(lexeme)? as u8;
                Ok(Literal::Uint8(value))
            }
            TType::Int16 => {
                let value = self.parse_number(lexeme)? as i16;
                Ok(Literal::Int16(value))
            }
            TType::Uint16 => {
                let value = self.parse_number(lexeme)? as u16;
                Ok(Literal::Uint16(value))
            }
            TType::Int32 => {
                let value = self.parse_number(lexeme)? as i32;
                Ok(Literal::Int32(value))
            }
            TType::Uint32 => {
                let value = self.parse_number(lexeme)? as u32;
                Ok(Literal::Uint32(value))
            }
            TType::Int64 => {
                let value = self.parse_number(lexeme)?;
                Ok(Literal::Int64(value))
            }
            TType::Uint64 => {
                let value = self.parse_number(lexeme)? as u64;
                Ok(Literal::Uint64(value))
            }
            TType::Int128 => {
                let value = self.parse_number(lexeme)? as i128;
                Ok(Literal::Int128(value))
            }
            TType::Uint128 => {
                let value = self.parse_number(lexeme)? as u128;
                Ok(Literal::Uint128(value))
            }
            TType::IntSize => {
                let value = self.parse_number(lexeme)? as isize;
                Ok(Literal::IntSize(value))
            }
            TType::UintSize => {
                let value = self.parse_number(lexeme)? as usize;
                Ok(Literal::UintSize(value))
            }
            TType::Float => {
                let value = self.parse_float(lexeme)?;
                Ok(Literal::Float(value))
            }
            TType::F32 => {
                let value = self.parse_float(lexeme)? as f32;
                Ok(Literal::F32(value))
            }
            TType::F64 => {
                let value = self.parse_float(lexeme)?;
                Ok(Literal::F64(value))
            }
            _ => Err(format!("Unexpected token type: {:?}", token.token_type)),
        }
    }

    pub fn parse_literal(&mut self) -> Result<Expr, String> {
        let token = match self.current_token() {
            Some(t) => t.clone(),
            None => return Err("Unexpected end of file in parse literal".to_string()),
        };

        match token.token_type {
            TType::True => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(true)))
            }
            TType::False => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(false)))
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
                Ok(Expr::Literal(literal))
            }
            _ => Err(format!(
                "Expected literal, found {:?} at {}:{}",
                token.token_type, token.line, token.col
            )),
        }
    }
}
