use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    lexer::{TType, token::Token},
};

pub struct Lexer<'a> {
    pos: usize,
    source: &'a str,
    keywords: HashMap<String, TType>,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, diagnostics: &'a mut Diagnostics) -> Self {
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
            ("seal".to_string(), TType::Seal),
        ]);

        Lexer {
            pos: 0,
            source: src,
            keywords,
            corrupted: false,
            diagnostics,
        }
    }

    fn current_char(&self) -> Option<char> {
        self.source.chars().nth(self.pos)
    }

    fn peek_char(&self) -> Option<char> {
        self.source.chars().nth(self.pos + 1)
    }

    fn advance(&mut self) {
        self.pos += 1;
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

                        while let Some(c) = self.current_char() {
                            if c == '#' && self.peek_char() == Some('#') {
                                self.advance();
                                self.advance();
                                closed = true;
                                break;
                            }
                            self.advance();
                        }

                        if !closed {
                            let span = Span {
                                start: self.pos,
                                end: self.pos + 1,
                            };
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

    fn read_number(&mut self) -> Token {
        let start = self.pos;

        // Check for hex and binary
        if let Some('0') = self.current_char() {
            if let Some(next) = self.peek_char() {
                if next == 'x' || next == 'X' {
                    return self.read_hex();
                } else if next == 'b' || next == 'B' {
                    return self.read_binary();
                }
            }
        }

        // Read digits
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Check for float
        if let Some('.') = self.current_char() {
            self.advance();
            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            let end = self.pos;
            let lexeme = self.source[start..end].to_string();
            return self.parse_float_suffix(lexeme, Span { start, end });
        }

        let end = self.pos;
        let lexeme = self.source[start..end].to_string();
        self.parse_suffix(lexeme, Span { start, end })
    }

    fn read_hex(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // 0
        self.advance(); // x

        let mut has_digit = false;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_hexdigit() {
                has_digit = true;
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let lexeme = self.source[start..end].to_string();

        if !has_digit {
            let span = Span { start, end };
            self.report(
                "Invalid hex number: expected hex digit after '0x'".to_string(),
                Some(span.clone()),
            );
            return Token::new(lexeme, TType::Illegal, span);
        }

        self.parse_suffix(lexeme, Span { start, end })
    }

    fn read_binary(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // 0
        self.advance(); // b

        let mut has_digit = false;
        while let Some(ch) = self.current_char() {
            if ch == '0' || ch == '1' {
                has_digit = true;
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let lexeme = self.source[start..end].to_string();

        if !has_digit {
            let span = Span { start, end };
            self.report(
                "Invalid binary number: expected binary digit after '0b'".to_string(),
                Some(span.clone()),
            );
            return Token::new(lexeme, TType::Illegal, span);
        }

        self.parse_suffix(lexeme, Span { start, end })
    }

    fn parse_suffix(&mut self, value: String, span: Span) -> Token {
        let start = self.pos;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphabetic() || ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let suffix = self.source[start..end].to_string();

        let token_type = match suffix.as_str() {
            "i64" => TType::Int64,
            "u64" => TType::Uint64,
            "i16" => TType::Int16,
            "u16" => TType::Uint16,
            "i128" => TType::Int128,
            "u128" => TType::Uint128,
            "i32" => TType::Int32,
            "u32" => TType::Uint32,
            "i8" => TType::Int8,
            "u8" => TType::Uint8,
            "iz" => TType::IntSize,
            "uz" => TType::UintSize,
            _ => TType::Int,
        };

        Token::new(
            value,
            token_type,
            Span {
                start: span.start,
                end,
            },
        )
    }

    fn parse_float_suffix(&mut self, value: String, span: Span) -> Token {
        let start = self.pos;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphabetic() || ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let suffix = self.source[start..end].to_string();

        let token_type = match suffix.as_str() {
            "f64" => TType::F64,
            "f32" => TType::F32,
            _ => TType::Float,
        };

        Token::new(
            value,
            token_type,
            Span {
                start: span.start,
                end,
            },
        )
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.pos;

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let lexeme = self.source[start..end].to_string();
        let span = Span { start, end };

        match self.keywords.get(&lexeme) {
            Some(ttype) => Token::new(lexeme, *ttype, span),
            None => Token::new(lexeme, TType::Identifier, span),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start = self.pos;

        match self.current_char() {
            Some(ch) if ch.is_ascii_alphabetic() || ch == '_' => self.read_identifier(),
            Some(ch) if ch.is_ascii_digit() || ch == '0' => self.read_number(),
            Some('+') => {
                self.advance();
                if let Some('+') = self.current_char() {
                    self.advance();
                    Token::new(
                        "++".to_string(),
                        TType::PlusPlus,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "+".to_string(),
                        TType::Plus,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('-') => {
                self.advance();
                if let Some('-') = self.current_char() {
                    self.advance();
                    Token::new(
                        "--".to_string(),
                        TType::MinusMinus,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "-".to_string(),
                        TType::Minus,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some(':') => {
                self.advance();
                if let Some(':') = self.current_char() {
                    self.advance();
                    Token::new(
                        "::".to_string(),
                        TType::Scope,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        ":=".to_string(),
                        TType::Bind,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        ":".to_string(),
                        TType::Colon,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('=') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "==".to_string(),
                        TType::Eq,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "=".to_string(),
                        TType::Assign,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('!') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "!=".to_string(),
                        TType::Neq,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "!".to_string(),
                        TType::Bang,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('>') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        ">=".to_string(),
                        TType::Gte,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else if let Some('>') = self.current_char() {
                    self.advance();
                    Token::new(
                        ">>".to_string(),
                        TType::Rightshift,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        ">".to_string(),
                        TType::Gt,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('<') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "<=".to_string(),
                        TType::Lte,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else if let Some('>') = self.current_char() {
                    self.advance();
                    Token::new(
                        "<<".to_string(),
                        TType::Leftshift,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "<".to_string(),
                        TType::Lt,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('|') => {
                self.advance();
                if let Some('|') = self.current_char() {
                    self.advance();
                    Token::new(
                        "||".to_string(),
                        TType::Or,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "|".to_string(),
                        TType::Stick,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('&') => {
                self.advance();
                if let Some('&') = self.current_char() {
                    self.advance();
                    Token::new(
                        "&&".to_string(),
                        TType::And,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "&".to_string(),
                        TType::Ampersand,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('(') => {
                self.advance();
                Token::new(
                    "(".to_string(),
                    TType::Lparen,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some(')') => {
                self.advance();
                Token::new(
                    ")".to_string(),
                    TType::Rparen,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('{') => {
                self.advance();
                Token::new(
                    "{".to_string(),
                    TType::LBrace,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('}') => {
                self.advance();
                Token::new(
                    "}".to_string(),
                    TType::Rbrace,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('[') => {
                self.advance();
                Token::new(
                    "[".to_string(),
                    TType::LBracket,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some(']') => {
                self.advance();
                Token::new(
                    "]".to_string(),
                    TType::RBracket,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('*') => {
                self.advance();
                Token::new(
                    "*".to_string(),
                    TType::Star,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('/') => {
                self.advance();
                Token::new(
                    "/".to_string(),
                    TType::Slash,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('%') => {
                self.advance();
                Token::new(
                    "%".to_string(),
                    TType::Percentage,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('~') => {
                self.advance();
                Token::new(
                    "~".to_string(),
                    TType::Tilde,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some(';') => {
                self.advance();
                Token::new(
                    ";".to_string(),
                    TType::Semicolon,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some(',') => {
                self.advance();
                Token::new(
                    ",".to_string(),
                    TType::Comma,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            None => Token::new(
                "".to_string(),
                TType::End,
                Span {
                    start,
                    end: self.pos,
                },
            ),
            Some(ch) => {
                let span = Span {
                    start,
                    end: self.pos + 1,
                };
                self.report(format!("Invalid character: '{}'", ch), Some(span.clone()));
                self.advance();
                Token::new(ch.to_string(), TType::Illegal, span)
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
