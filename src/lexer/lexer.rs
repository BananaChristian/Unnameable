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
        let keywords = Self::load_keywords();

        Lexer {
            pos: 0,
            source: src,
            keywords,
            corrupted: false,
            diagnostics,
        }
    }

    fn load_keywords() -> HashMap<String, TType> {
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
            ("extern".to_string(), TType::Extern),
            ("null".to_string(), TType::Null),
            ("unwrap".to_string(), TType::Unwrap),
            ("bitcast".to_string(), TType::Bitcast),
            ("cast".to_string(), TType::Cast),
            ("alias".to_string(), TType::Alias),
            ("as".to_string(), TType::As),
            ("import".to_string(), TType::Import),
        ]);
        keywords
    }

    /// The character starting at byte offset `byte`, if any.
    fn char_at(&self, byte: usize) -> Option<char> {
        self.source.get(byte..)?.chars().next()
    }

    fn current_char(&self) -> Option<char> {
        self.char_at(self.pos)
    }

    /// The character immediately after the current one (not `pos + 1` as a
    /// byte — the current char may be multi-byte).
    fn peek_char(&self) -> Option<char> {
        if let Some(c) = self.current_char() {
            self.char_at(self.pos + c.len_utf8())
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char() {
            self.pos += c.len_utf8();
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
                let comment_start = self.pos;
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
                                start: comment_start,
                                end: self.pos,
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

        // Check for hex, binary, and octal
        if let Some('0') = self.current_char() {
            if let Some(next) = self.peek_char() {
                if next == 'x' || next == 'X' {
                    return self.read_hex();
                } else if next == 'b' || next == 'B' {
                    return self.read_binary();
                } else if next == 'o' || next == 'O' {
                    return self.read_octal();
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
        let mut is_float = false;
        if let Some('.') = self.current_char() {
            self.advance();
            is_float = true;
            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Check for exponent (1e5, 1.5e3, 1.5E-3, 2e+4)
        let mut has_exponent = false;
        if self.has_exponent_at(self.pos) {
            self.advance(); // 'e' or 'E'
            if let Some(sign) = self.current_char() {
                if sign == '+' || sign == '-' {
                    self.advance();
                }
            }
            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            has_exponent = true;
        }

        let end = self.pos;
        let lexeme = self.source[start..end].replace('_', "");

        if is_float || has_exponent {
            self.parse_float_suffix(lexeme, Span { start, end })
        } else {
            self.parse_suffix(lexeme, Span { start, end })
        }
    }

    /// True if a valid exponent part (`e`/`E` with optional sign and at least
    /// one digit) starts at byte offset `at`. Used as lookahead so that a
    /// bare `e` with no exponent digits is left alone (and can be reported
    /// as an invalid suffix instead).
    fn has_exponent_at(&self, at: usize) -> bool {
        let Some(ch) = self.char_at(at) else {
            return false;
        };
        if ch != 'e' && ch != 'E' {
            return false;
        }
        let mut i = at + ch.len_utf8();
        if let Some(sign) = self.char_at(i) {
            if sign == '+' || sign == '-' {
                i += sign.len_utf8();
            }
        }
        matches!(self.char_at(i), Some(d) if d.is_ascii_digit())
    }

    fn read_octal(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // 0
        self.advance(); // o / O

        let mut has_digit = false;
        while let Some(ch) = self.current_char() {
            if ('0'..='7').contains(&ch) {
                has_digit = true;
                self.advance();
            } else if ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos;
        let lexeme = self.source[start..end].replace('_', "");

        if !has_digit {
            let span = Span { start, end };
            self.report(
                "Invalid octal number: expected octal digit after '0o'".to_string(),
                Some(span.clone()),
            );
            return Token::new(lexeme, TType::Illegal, span);
        }

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
        let lexeme = self.source[start..end].replace('_', "");

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
        let lexeme = self.source[start..end].replace('_', "");

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
        let suffix_start = self.pos;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphabetic() || ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
        let suffix_end = self.pos;

        if suffix_end == suffix_start {
            return Token::new(value, TType::Int, span);
        }

        let suffix = &self.source[suffix_start..suffix_end];

        let token_type = match suffix {
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
            _ => {
                self.report(
                    format!("Invalid suffix '{}' for integer literal", suffix),
                    Some(Span {
                        start: suffix_start,
                        end: suffix_end,
                    }),
                );
                // Do not swallow the unknown suffix: rewind so it lexes as
                // its own token (e.g. an identifier), with the error above
                // failing the compilation.
                self.pos = suffix_start;
                return Token::new(value, TType::Int, span);
            }
        };

        Token::new(
            value,
            token_type,
            Span {
                start: span.start,
                end: suffix_end,
            },
        )
    }

    fn parse_float_suffix(&mut self, value: String, span: Span) -> Token {
        let suffix_start = self.pos;
        while let Some(ch) = self.current_char() {
            if ch.is_ascii_alphabetic() || ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
        let suffix_end = self.pos;

        if suffix_end == suffix_start {
            return Token::new(value, TType::Float, span);
        }

        let suffix = &self.source[suffix_start..suffix_end];

        let token_type = match suffix {
            "f64" => TType::F64,
            "f32" => TType::F32,
            _ => {
                self.report(
                    format!("Invalid suffix '{}' for float literal", suffix),
                    Some(Span {
                        start: suffix_start,
                        end: suffix_end,
                    }),
                );
                self.pos = suffix_start;
                return Token::new(value, TType::Float, span);
            }
        };

        Token::new(
            value,
            token_type,
            Span {
                start: span.start,
                end: suffix_end,
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
                            Some('u') => {
                                // unicode escape \u{1F600}
                                self.advance(); // consume u
                                if self.current_char() != Some('{') {
                                    self.report(
                                        "Invalid unicode escape in char literal: expected '\\u{...}'"
                                            .to_string(),
                                        None,
                                    );
                                    '\u{FFFD}'
                                } else {
                                    self.advance(); // consume {
                                    let mut hex = String::new();
                                    loop {
                                        match self.current_char() {
                                            Some('}') => {
                                                self.advance();
                                                break;
                                            }
                                            Some(h) if h.is_ascii_hexdigit() => {
                                                hex.push(h);
                                                self.advance();
                                            }
                                            _ => break,
                                        }
                                    }
                                    match u32::from_str_radix(&hex, 16) {
                                        Ok(code) => match char::from_u32(code) {
                                            Some(c) => c,
                                            None => {
                                                self.report(
                                                    format!("Invalid unicode codepoint: {}", hex),
                                                    None,
                                                );
                                                '\u{FFFD}'
                                            }
                                        },
                                        Err(_) => {
                                            self.report(
                                                format!(
                                                    "Invalid unicode escape in char literal: \\u{{{}}}",
                                                    hex
                                                ),
                                                None,
                                            );
                                            '\u{FFFD}'
                                        }
                                    }
                                }
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
                    end: start + ch.len_utf8(),
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
