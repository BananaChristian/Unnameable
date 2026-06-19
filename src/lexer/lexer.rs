use std::collections::HashMap;

use crate::lexer::{TType, token::Token};


pub struct Lexer {
    line: usize,
    col: usize,
    pos: usize,
    source: Vec<char>,
    keywords: HashMap<String, TType>,
}

impl Lexer {
    pub fn new(src: &str) -> Self {
        let keywords = HashMap::from([
            ("mut".to_string(), TType::Mut),
            ("const".to_string(), TType::Const),
            ("var".to_string(), TType::Var),
            ("heap".to_string(), TType::Heap),
            ("func".to_string(), TType::Func),
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
        ]);

        Lexer {
            line: 1,
            col: 1,
            pos: 0,
            source: src.chars().collect(),
            keywords,
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

                        // Skip until closing ##
                        while let Some(c) = self.current_char() {
                            if c == '#' && self.peek_char() == Some('#') {
                                self.advance(); // Consume first #
                                self.advance(); // Consume second #
                                break;
                            }
                            self.advance();
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

        while let Some(ch) = self.current_char() {
            if self.is_binary_digit(ch) {
                number.push(ch);
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        self.parse_suffix(number, line, col)
    }

    fn read_hex(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut number = String::from("0x");

        self.advance(); //Consume 0
        self.advance(); //Consume x

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_hexdigit() {
                number.push(ch);
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
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
                    Token::new(self.line, self.col, "==".to_string(), TType::Equals)
                } else {
                    Token::new(self.line, self.col, "=".to_string(), TType::Assign)
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
            Some(';') => {
                self.advance();
                Token::new(self.line, self.col, ";".to_string(), TType::Semicolon)
            }
            None => Token::new(self.line, self.col, "".to_string(), TType::End),
            Some(ch) => {
                self.advance();
                Token::new(self.line, self.col, ch.to_string(), TType::Illegal)
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
}
