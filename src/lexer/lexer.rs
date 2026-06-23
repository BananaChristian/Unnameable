use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    lexer::{TType, token::Token},
};

pub struct Lexer<'a> {
    line: usize,
    col: usize,
    pos: usize,
    source: Vec<char>,
    keywords: HashMap<String, TType>,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &str, diagnostics: &'a mut Diagnostics) -> Self {
        let keywords = HashMap::from([
            ("mut".to_string(), TType::Mut),
            ("const".to_string(), TType::Const),
            ("var".to_string(), TType::Var),
            ("heap".to_string(), TType::Heap),
            ("func".to_string(), TType::Func),
            ("struct".to_string(), TType::Struct),
            ("return".to_string(), TType::Return),
            ("continue".to_string(), TType::Continue),
            ("break".to_string(), TType::Break),
            ("true".to_string(), TType::True),
            ("false".to_string(), TType::False),
            ("if".to_string(), TType::If),
            ("elif".to_string(), TType::Elif),
            ("else".to_string(), TType::Else),
            ("i8".to_string(), TType::I8Key),
            ("u8".to_string(), TType::U8Key),
            ("i16".to_string(), TType::I16Key),
            ("u16".to_string(), TType::U16Key),
            ("i32".to_string(), TType::I32Key),
            ("u32".to_string(), TType::U32key),
            ("i64".to_string(), TType::I64Key),
            ("u64".to_string(), TType::U64Key),
            ("i128".to_string(), TType::I128Key),
            ("u128".to_string(), TType::U128Key),
            ("isize".to_string(), TType::ISIZEKey),
            ("usize".to_string(), TType::USIZEKey),
            ("bool".to_string(), TType::BoolKey),
            ("f32".to_string(), TType::F32Key),
            ("f64".to_string(), TType::F64Key),
            ("ptr".to_string(), TType::Ptr),
            ("ref".to_string(), TType::Ref),
            ("arr".to_string(), TType::Arr),
            ("seal".to_string(),TType::Seal),
        ]);

        Lexer {
            line: 1,
            col: 1,
            pos: 0,
            source: src.chars().collect(),
            keywords,
            corrupted: false,
            diagnostics,
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.pos + 1 < self.source.len() {
            Some(self.source[self.pos + 1])
        } else {
            None
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.pos + 1 < self.source.len() {
            Some(self.source[self.pos])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if let Some(ch) = self.current_char() {
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == '#' {
                self.skip_comment();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        if let Some(ch) = self.current_char() {
            if ch == '#' {
                self.advance(); // Consume the initial #

                if let Some(nch) = self.current_char() {
                    if nch == '#' {
                        // Multi-line comment
                        self.advance(); // Consume the second #
                        let mut closed = false;

                        // Skip until closing ##
                        while let Some(c) = self.current_char() {
                            if c == '#' && self.peek_char() == Some('#') {
                                self.advance(); // Consume first #
                                self.advance(); // Consume second #
                                closed = true;
                                break;
                            }
                            self.advance();
                        }

                        if !closed {
                            let span = Span::simple(self.line, self.col, 1);
                            self.report("Unterminated multi-line comment".to_string(), Some(span));
                        }
                    } else {
                        // Single-line comment
                        while let Some(c) = self.current_char() {
                            if c == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    }
                }
            }
        }
    }

    fn is_binary_digit(&self, ch: char) -> bool {
        ch == '0' || ch == '1'
    }

    fn is_alpha(&self, ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn is_alpha_or_digit(&self, ch: char) -> bool {
        self.is_alpha(ch) || ch.is_ascii_digit()
    }

    fn is_ident_char(&self, ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn is_ident_continue(&self, ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch.is_ascii_digit() || ch == '_'
    }

    fn parse_suffix(&mut self, value: String, line: usize, col: usize) -> Token {
        let mut suffix = String::new();

        while let Some(ch) = self.current_char() {
            if self.is_alpha_or_digit(ch) {
                suffix.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        match suffix.as_str() {
            "i64" => Token::new(line, col, value, TType::Int64),
            "u64" => Token::new(line, col, value, TType::Uint64),
            "i16" => Token::new(line, col, value, TType::Int16),
            "u16" => Token::new(line, col, value, TType::Uint16),
            "i128" => Token::new(line, col, value, TType::Int128),
            "u128" => Token::new(line, col, value, TType::Uint128),
            "i32" => Token::new(line, col, value, TType::Int32),
            "u32" => Token::new(line, col, value, TType::Uint32),
            "i8" => Token::new(line, col, value, TType::Int8),
            "u8" => Token::new(line, col, value, TType::Uint8),
            "iz" => Token::new(line, col, value, TType::IntSize),
            "uz" => Token::new(line, col, value, TType::UintSize),
            _ => Token::new(line, col, value, TType::Int), // Default to Int
        }
    }

    fn parse_float_suffix(&mut self, value: String, line: usize, col: usize) -> Token {
        let mut suffix = String::new();
        while let Some(ch) = self.current_char() {
            if self.is_alpha_or_digit(ch) {
                suffix.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        match suffix.as_str() {
            "f64" => Token::new(line, col, value, TType::F64),
            "f32" => Token::new(line, col, value, TType::F32),
            _ => Token::new(line, col, value, TType::Float), // Default to Float
        }
    }

    fn read_binary(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut number = String::from("0b");

        self.advance(); //Skip 0
        self.advance(); //Skip 1

        let mut has_digit = false;
        while let Some(ch) = self.current_char() {
            if self.is_binary_digit(ch) {
                has_digit = true;
                number.push(ch);
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if !has_digit {
            let span = Span::simple(line, col, 2);
            self.report(
                "Invalid binary number: expected binary digit after '0b'".to_string(),
                Some(span),
            );
            return Token::new(line, col, number, TType::Illegal);
        }

        self.parse_suffix(number, line, col)
    }

    fn read_hex(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut number = String::from("0x");

        self.advance(); //Consume 0
        self.advance(); //Consume x

        let mut has_digit = false;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_hexdigit() {
                has_digit = true;
                number.push(ch);
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if !has_digit {
            let span = Span::simple(line, col, 2);
            self.report(
                "Invalid hex number: expected hex digit after '0x'".to_string(),
                Some(span),
            );
            return Token::new(line, col, number, TType::Illegal);
        }

        self.parse_suffix(number, line, col)
    }

    fn read_number(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut number = String::new();

        //Check for hex and binary guys
        if let Some('0') = self.current_char() {
            if let Some(next) = self.peek_char() {
                if next == 'x' || next == 'X' {
                    return self.read_hex();
                } else if next == 'b' || next == 'B' {
                    return self.read_binary();
                }
            }
        }

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                number.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if Some('.') == self.current_char() {
            number.push('.');
            self.advance();

            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    number.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
            return self.parse_float_suffix(number, line, col);
        }

        self.parse_suffix(number, line, col)
    }

    fn read_identifier(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut identifier = String::new();

        while let Some(ch) = self.current_char() {
            if self.is_ident_char(ch) {
                identifier.push(ch);
                self.advance();
            } else {
                if self.is_ident_continue(ch) {
                    identifier.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        match self.keywords.get(&identifier) {
            Some(ttype) => Token::new(line, col, identifier, *ttype),
            None => Token::new(line, col, identifier, TType::Identifier),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current_char() {
            Some(ch) if self.is_ident_char(ch) => self.read_identifier(),
            Some(ch) if ch.is_ascii_digit() || ch == '0' => self.read_number(),
            Some('+') => {
                self.advance(); //Consume the token 
                if let Some('+') = self.current_char() {
                    self.advance(); //Consume the second +
                    Token::new(self.line, self.col, "++".to_string(), TType::PlusPlus)
                } else {
                    Token::new(self.line, self.col, "+".to_string(), TType::Plus)
                }
            }
            Some('-') => {
                self.advance();
                if let Some('-') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "--".to_string(), TType::MinusMinus)
                } else {
                    Token::new(self.line, self.col, "-".to_string(), TType::Minus)
                }
            }
            Some(':') => {
                self.advance();
                if let Some(':') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "::".to_string(), TType::Scope)
                } else if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, ":=".to_string(), TType::Bind)
                } else {
                    Token::new(self.line, self.col, ":".to_string(), TType::Colon)
                }
            }
            Some('=') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "==".to_string(), TType::Eq)
                } else {
                    Token::new(self.line, self.col, "=".to_string(), TType::Assign)
                }
            }
            Some('!') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "!=".to_string(), TType::Neq)
                } else {
                    Token::new(self.line, self.col, "!".to_string(), TType::Bang)
                }
            }
            Some('>') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, ">=".to_string(), TType::Gte)
                } else if let Some('>') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, ">>".to_string(), TType::Rightshift)
                } else {
                    Token::new(self.line, self.col, ">".to_string(), TType::Gt)
                }
            }
            Some('<') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "<=".to_string(), TType::Lte)
                } else if let Some('>') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "<<".to_string(), TType::Leftshift)
                } else {
                    Token::new(self.line, self.col, "<".to_string(), TType::Lt)
                }
            }
            Some('|') => {
                self.advance();
                if let Some('|') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "||".to_string(), TType::Or)
                } else {
                    self.advance();
                    Token::new(self.line, self.col, "|".to_string(), TType::Stick)
                }
            }
            Some('&') => {
                self.advance();
                if let Some('&') = self.current_char() {
                    self.advance();
                    Token::new(self.line, self.col, "&&".to_string(), TType::And)
                } else {
                    Token::new(self.line, self.col, "&".to_string(), TType::Ampersand)
                }
            }
            Some('(') => {
                self.advance();
                Token::new(self.line, self.col, "(".to_string(), TType::Lparen)
            }
            Some(')') => {
                self.advance();
                Token::new(self.line, self.col, ")".to_string(), TType::Rparen)
            }
            Some('{') => {
                self.advance();
                Token::new(self.line, self.col, "{".to_string(), TType::LBrace)
            }
            Some('}') => {
                self.advance();
                Token::new(self.line, self.col, "}".to_string(), TType::Rbrace)
            }
            Some('*') => {
                self.advance();
                Token::new(self.line, self.col, "*".to_string(), TType::Star)
            }
            Some('/') => {
                self.advance();
                Token::new(self.line, self.col, "/".to_string(), TType::Slash)
            }
            Some('%') => {
                self.advance();
                Token::new(self.line, self.col, "%".to_string(), TType::Percentage)
            }
            Some('~') => {
                self.advance();
                Token::new(self.line, self.col, "~".to_string(), TType::Tilde)
            }
            Some(';') => {
                self.advance();
                Token::new(self.line, self.col, ";".to_string(), TType::Semicolon)
            }
            Some(',') => {
                self.advance();
                Token::new(self.line, self.col, ",".to_string(), TType::Comma)
            }
            None => Token::new(self.line, self.col, "".to_string(), TType::End),
            Some(ch) => {
                let line = self.line;
                let col = self.col;
                let span = Span::simple(line, col, 1);

                self.report(format!("Invalid character: '{}'", ch), Some(span));

                self.advance();
                Token::new(line, col, ch.to_string(), TType::Illegal)
            }
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.token_type == TType::End;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Lexer, span));
    }
}
