use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    lexer::{TType, token::Token},
};

pub struct Lexer<'a> {
    pos: usize,
    source: &'a str,
    keywords: HashMap<String, TType>,
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, diagnostics: SharedDiagnostics) -> Self {
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
            ("while".to_string(), TType::While),
            ("for".to_string(), TType::For),
            ("each".to_string(), TType::Each),
            ("in".to_string(), TType::In),
            ("else".to_string(), TType::Else),
            ("shr".to_string(), TType::Rightshift),
            ("shl".to_string(), TType::Leftshift),
            ("and".to_string(), TType::BitwiseAnd),
            ("or".to_string(), TType::BitwiseOr),
            ("xor".to_string(), TType::Xor),
            ("not".to_string(), TType::Not),
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
            ("str".to_string(), TType::StrKey),
            ("char8".to_string(), TType::Char8Key),
            ("char16".to_string(), TType::Char16Key),
            ("char32".to_string(), TType::Char32Key),
            ("ptr".to_string(), TType::Ptr),
            ("ref".to_string(), TType::Ref),
            ("arr".to_string(), TType::Arr),
            ("seal".to_string(), TType::Seal),
            ("methods".to_string(), TType::Methods),
            ("generics".to_string(), TType::Generics),
            ("contract".to_string(), TType::Contract),
            ("sizeof".to_string(), TType::SizeOf),
            ("enum".to_string(), TType::Enum),
            ("variant".to_string(), TType::Variant),
            ("expose".to_string(), TType::Expose),
            ("null".to_string(), TType::Null),
            ("unwrap".to_string(), TType::Unwrap),
            ("bitcast".to_string(), TType::Bitcast),
            ("cast".to_string(), TType::Cast),
            ("alias".to_string(), TType::Alias),
            ("as".to_string(), TType::As),
            ("import".to_string(), TType::Import),
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
            } else if ch == '_' {
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
                } else if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "+=".to_string(),
                        TType::CompoundAdd,
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
            Some('$') => {
                self.advance();
                if let Some('$') = self.current_char() {
                    self.advance();
                    Token::new(
                        "$$".to_string(),
                        TType::DoubleDollar,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "$".to_string(),
                        TType::Dollar,
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
                } else if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "-=".to_string(),
                        TType::CompoundSub,
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
                } else if let Some('!') = self.current_char() {
                    self.advance();
                    Token::new(
                        "!!".to_string(),
                        TType::DoubleExclaim,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else if let Some('?') = self.current_char() {
                    self.advance();
                    Token::new(
                        "!?".to_string(),
                        TType::Propagate,
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
            Some('?') => {
                self.advance();
                if let Some('?') = self.current_char() {
                    self.advance();
                    Token::new(
                        "??".to_string(),
                        TType::Coalesce,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "?".to_string(),
                        TType::QuestionMark,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('.') => {
                self.advance();
                Token::new(
                    ".".to_string(),
                    TType::Dot,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
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
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "*=".to_string(),
                        TType::CompoundMul,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "*".to_string(),
                        TType::Star,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('/') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "/=".to_string(),
                        TType::CompoundDiv,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "/".to_string(),
                        TType::Slash,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
            }
            Some('%') => {
                self.advance();
                if let Some('=') = self.current_char() {
                    self.advance();
                    Token::new(
                        "%=".to_string(),
                        TType::CompoundModulo,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                } else {
                    Token::new(
                        "%".to_string(),
                        TType::Percentage,
                        Span {
                            start,
                            end: self.pos,
                        },
                    )
                }
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
            Some('^') => {
                self.advance();
                Token::new(
                    "^".to_string(),
                    TType::Caret,
                    Span {
                        start,
                        end: self.pos,
                    },
                )
            }
            Some('@') => {
                self.advance();
                Token::new(
                    "@".to_string(),
                    TType::At,
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
            Some('"') => {
                self.advance(); // consume opening "
                let mut value = String::new();
                loop {
                    match self.current_char() {
                        Some('"') => {
                            self.advance();
                            break;
                        }
                        Some('\\') => {
                            self.advance();
                            match self.current_char() {
                                Some('n') => {
                                    self.advance();
                                    value.push('\n');
                                }
                                Some('t') => {
                                    self.advance();
                                    value.push('\t');
                                }
                                Some('\\') => {
                                    self.advance();
                                    value.push('\\');
                                }
                                Some('"') => {
                                    self.advance();
                                    value.push('"');
                                }
                                Some('0') => {
                                    self.advance();
                                    value.push('\0');
                                }
                                Some('u') => {
                                    // unicode escape \u{1F600}
                                    self.advance(); // consume u
                                    if self.current_char() == Some('{') {
                                        self.advance();
                                        let mut hex = String::new();
                                        while let Some(c) = self.current_char() {
                                            if c == '}' {
                                                self.advance();
                                                break;
                                            }
                                            hex.push(c);
                                            self.advance();
                                        }
                                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                                            if let Some(c) = char::from_u32(code) {
                                                value.push(c);
                                            } else {
                                                self.report(
                                                    format!("Invalid unicode codepoint: {}", hex),
                                                    None,
                                                );
                                            }
                                        }
                                    }
                                }
                                Some(c) => {
                                    self.report(format!("Unknown escape sequence: \\{}", c), None);
                                    self.advance();
                                }
                                None => {
                                    self.report(
                                        "Unexpected end of file in string literal".to_string(),
                                        None,
                                    );
                                    break;
                                }
                            }
                        }
                        Some(c) => {
                            value.push(c);
                            self.advance();
                        }
                        None => {
                            self.report("Unterminated string literal".to_string(), None);
                            break;
                        }
                    }
                }
                let end = self.pos;
                Token::new(value, TType::StringLiteral, Span { start, end })
            }
            Some('\'') => {
                self.advance(); // consume opening '
                let ch = match self.current_char() {
                    Some('\\') => {
                        self.advance();
                        match self.current_char() {
                            Some('n') => {
                                self.advance();
                                '\n'
                            }
                            Some('t') => {
                                self.advance();
                                '\t'
                            }
                            Some('\\') => {
                                self.advance();
                                '\\'
                            }
                            Some('\'') => {
                                self.advance();
                                '\''
                            }
                            Some('0') => {
                                self.advance();
                                '\0'
                            }
                            Some(c) => {
                                self.report(format!("Unknown escape sequence: \\{}", c), None);
                                self.advance();
                                c
                            }
                            None => {
                                self.report(
                                    "Unexpected end of file in char literal".to_string(),
                                    None,
                                );
                                return Token::new(
                                    "".to_string(),
                                    TType::Illegal,
                                    Span {
                                        start,
                                        end: self.pos,
                                    },
                                );
                            }
                        }
                    }
                    Some(c) => {
                        self.advance();
                        c
                    }
                    None => {
                        self.report("Unexpected end of file in char literal".to_string(), None);
                        return Token::new(
                            "".to_string(),
                            TType::Illegal,
                            Span {
                                start,
                                end: self.pos,
                            },
                        );
                    }
                };

                if self.current_char() != Some('\'') {
                    self.report("Expected closing ' for char literal".to_string(), None);
                } else {
                    self.advance(); // consume closing '
                }

                // read suffix
                let suffix_start = self.pos;
                while let Some(c) = self.current_char() {
                    if c.is_ascii_alphanumeric() {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let suffix = &self.source[suffix_start..self.pos];

                // validate size fits
                let char_len = ch.len_utf8();
                let token_type = match suffix {
                    "c8" => {
                        if char_len > 1 {
                            self.report(
                                format!("Character '{}' does not fit in char8 (1 byte)", ch),
                                Some(Span {
                                    start,
                                    end: self.pos,
                                }),
                            );
                        }
                        TType::Char8Literal
                    }
                    "c16" => {
                        if char_len > 2 {
                            self.report(
                                format!("Character '{}' does not fit in char16 (2 bytes)", ch),
                                Some(Span {
                                    start,
                                    end: self.pos,
                                }),
                            );
                        }
                        TType::Char16Literal
                    }
                    "c32" => TType::Char32Literal, // always fits
                    "" => {
                        // default — char8, validate fits
                        if char_len > 1 {
                            self.report(
                    format!("Character '{}' does not fit in default char8 — use 'c16' or 'c32' suffix", ch),
                    Some(Span { start, end: self.pos }),
                );
                        }
                        TType::Char8Literal
                    }
                    _ => {
                        self.report(
                            format!("Unknown char suffix '{}'", suffix),
                            Some(Span {
                                start,
                                end: self.pos,
                            }),
                        );
                        TType::Char8Literal
                    }
                };

                let end = self.pos;
                Token::new(ch.to_string(), token_type, Span { start, end })
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
            .borrow_mut()
            .report(CompilerError::error(message, Phase::Lexer, span));
    }
}
