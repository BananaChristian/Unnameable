use std::cell::RefCell;
use std::rc::Rc;

use unnc::diagnostics::{CompilerError, Diagnostics, Phase, Span};
use unnc::lexer::token::Token;
use unnc::lexer::{Lexer, TType};

/// A lexed token stream plus the diagnostics produced while lexing.
struct LexResult {
    tokens: Vec<Token>,
    errors: Vec<CompilerError>,
    corrupted: bool,
}

/// Run the lexer over `src` and return everything it produced.
fn lex(src: &str) -> LexResult {
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "test.unn".to_string(),
        src.to_string(),
    )));
    let mut lexer = Lexer::new(src, diagnostics.clone());
    let tokens = lexer.tokenize();
    let errors = diagnostics.borrow().errors.clone();
    LexResult {
        tokens,
        errors,
        corrupted: lexer.corrupted,
    }
}

/// The token stream as owned (type, lexeme) pairs — ergonomic for assertions.
fn token_pairs(src: &str) -> Vec<(TType, String)> {
    lex(src)
        .tokens
        .iter()
        .map(|t| (t.token_type, t.lexeme.clone()))
        .collect()
}

/// Assert the full (type, lexeme) stream, including the trailing `End`.
#[track_caller]
fn assert_tokens(src: &str, expected: &[(TType, &str)]) {
    let actual = token_pairs(src);
    let expected: Vec<(TType, &str)> = expected.to_vec();
    assert_eq!(
        actual.len(),
        expected.len(),
        "token count mismatch for {src:?}\nactual:   {actual:#?}\nexpected: {expected:#?}"
    );
    for (i, ((at, al), (et, el))) in actual.iter().zip(expected.iter()).enumerate() {
        assert_eq!(at, et, "token {i} type mismatch for {src:?}");
        assert_eq!(al, el, "token {i} lexeme mismatch for {src:?}");
    }
}

/// Assert the byte spans of the token stream (excluding lexemes), including `End`.
#[track_caller]
fn assert_spans(src: &str, expected: &[(usize, usize)]) {
    let tokens = lex(src).tokens;
    let expected: Vec<(usize, usize)> = expected.to_vec();
    assert_eq!(
        tokens.len(),
        expected.len(),
        "token count mismatch for {src:?}"
    );
    for (i, (tok, (s, e))) in tokens.iter().zip(expected.iter()).enumerate() {
        assert_eq!(tok.span.start, *s, "token {i} start for {src:?}");
        assert_eq!(tok.span.end, *e, "token {i} end for {src:?}");
    }
}

/// Assert the lexer reports an error: corrupted flag, `Phase::Lexer`, message match,
/// and a span attached (unless `expect_span` is false).
#[track_caller]
fn assert_lexer_error(src: &str, message_contains: &str, expect_span: bool) {
    let result = lex(src);
    assert!(
        result.corrupted,
        "expected corrupted=true for {src:?}\nerrors: {:#?}",
        result.errors
    );
    assert!(
        !result.errors.is_empty(),
        "expected at least one error for {src:?}"
    );
    let err = result.errors.first().unwrap();
    assert!(
        matches!(err.phase, Phase::Lexer),
        "unexpected phase for {src:?}"
    );
    assert!(
        err.message.contains(message_contains),
        "message {:?} does not contain {:?} for {src:?}",
        err.message,
        message_contains
    );
    assert_eq!(err.span.is_some(), expect_span, "span presence for {src:?}");
}

// Empty / EOF handling
#[test]
fn empty_input_yields_only_end() {
    assert_tokens("", &[(TType::End, "")]);
}

#[test]
fn whitespace_only_yields_only_end() {
    assert_tokens("   \n\t  ", &[(TType::End, "")]);
}

#[test]
fn every_stream_terminates_with_end() {
    for src in ["x", "123", "= :: == $$", "# c\n", "if while var"] {
        let tokens = lex(src).tokens;
        assert_eq!(
            tokens.last().unwrap().token_type,
            TType::End,
            "missing End for {src:?}"
        );
    }
}

// Comments
#[test]
fn single_line_comment_to_eol() {
    assert_tokens("# hello world", &[(TType::End, "")]);
}

#[test]
fn single_line_comment_at_eof_without_newline() {
    assert_tokens("# no trailing newline", &[(TType::End, "")]);
}

#[test]
fn single_line_comment_resumes_at_next_line() {
    assert_tokens(
        "# skip\nvar x;",
        &[
            (TType::Var, "var"),
            (TType::Identifier, "x"),
            (TType::Semicolon, ";"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn block_comment_is_consumed() {
    assert_tokens("## block ##", &[(TType::End, "")]);
}

#[test]
fn block_comment_skips_newlines_and_resumes() {
    assert_tokens(
        "## multi\nline ## end",
        &[(TType::Identifier, "end"), (TType::End, "")],
    );
}

#[test]
fn unterminated_block_comment_is_an_error() {
    let result = lex("## unterminated");
    assert!(result.corrupted);
    assert_tokens("## unterminated", &[(TType::End, "")]);
    assert_lexer_error("## unterminated", "Unterminated multi-line comment", true);
}

#[test]
fn bare_double_hash_is_an_unterminated_block() {
    assert_lexer_error("##", "Unterminated multi-line comment", true);
}

#[test]
fn hash_inside_string_is_not_a_comment() {
    assert_tokens(
        "\"# not a comment\"",
        &[(TType::StringLiteral, "# not a comment"), (TType::End, "")],
    );
}

#[test]
fn hash_inside_char_is_not_a_comment() {
    assert_tokens("'#'", &[(TType::Char8Literal, "#"), (TType::End, "")]);
}

// Identifiers and keywords
#[test]
fn simple_identifiers() {
    assert_tokens(
        "foo Foo_bar _ _foo a1b2",
        &[
            (TType::Identifier, "foo"),
            (TType::Identifier, "Foo_bar"),
            (TType::Identifier, "_"),
            (TType::Identifier, "_foo"),
            (TType::Identifier, "a1b2"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn keyword_like_identifiers_are_not_keywords() {
    assert_tokens(
        "ifx elseif notthis",
        &[
            (TType::Identifier, "ifx"),
            (TType::Identifier, "elseif"),
            (TType::Identifier, "notthis"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn all_keywords_map_to_keyword_tokens() {
    assert_tokens(
        "mut const var owned func return continue break true false if elif while for \
         each in else shr shl and or xor not i8 u8 i16 u16 i32 u32 i64 u64 i128 u128 \
         isize usize bool f32 f64 str char8 char16 char32 ptr ref  seal methods \
         generics contract sizeof enum variant expose null unwrap bitcast cast alias as import match",
        &[
            (TType::Mut, "mut"),
            (TType::Const, "const"),
            (TType::Var, "var"),
            (TType::Owned, "owned"),
            (TType::Func, "func"),
            (TType::Return, "return"),
            (TType::Continue, "continue"),
            (TType::Break, "break"),
            (TType::True, "true"),
            (TType::False, "false"),
            (TType::If, "if"),
            (TType::Elif, "elif"),
            (TType::While, "while"),
            (TType::For, "for"),
            (TType::Each, "each"),
            (TType::In, "in"),
            (TType::Else, "else"),
            (TType::Rightshift, "shr"),
            (TType::Leftshift, "shl"),
            (TType::BitwiseAnd, "and"),
            (TType::BitwiseOr, "or"),
            (TType::Xor, "xor"),
            (TType::Not, "not"),
            (TType::I8Key, "i8"),
            (TType::U8Key, "u8"),
            (TType::I16Key, "i16"),
            (TType::U16Key, "u16"),
            (TType::I32Key, "i32"),
            (TType::U32key, "u32"),
            (TType::I64Key, "i64"),
            (TType::U64Key, "u64"),
            (TType::I128Key, "i128"),
            (TType::U128Key, "u128"),
            (TType::ISIZEKey, "isize"),
            (TType::USIZEKey, "usize"),
            (TType::BoolKey, "bool"),
            (TType::F32Key, "f32"),
            (TType::F64Key, "f64"),
            (TType::StrKey, "str"),
            (TType::Char8Key, "char8"),
            (TType::Char16Key, "char16"),
            (TType::Char32Key, "char32"),
            (TType::Ptr, "ptr"),
            (TType::Ref, "ref"),
            (TType::Seal, "seal"),
            (TType::Identifier, "methods"),
            (TType::Generics, "generics"),
            (TType::Contract, "contract"),
            (TType::SizeOf, "sizeof"),
            (TType::Enum, "enum"),
            (TType::Variant, "variant"),
            (TType::Expose, "expose"),
            (TType::Null, "null"),
            (TType::Unwrap, "unwrap"),
            (TType::Bitcast, "bitcast"),
            (TType::Cast, "cast"),
            (TType::Alias, "alias"),
            (TType::As, "as"),
            (TType::Import, "import"),
            (TType::Match, "match"),
            (TType::End, ""),
        ],
    );
}

// Operators and punctuation
#[test]
fn arithmetic_operators() {
    assert_tokens(
        "3 + 4 - 2 * 8 / 2 % 3",
        &[
            (TType::Int, "3"),
            (TType::Plus, "+"),
            (TType::Int, "4"),
            (TType::Minus, "-"),
            (TType::Int, "2"),
            (TType::Star, "*"),
            (TType::Int, "8"),
            (TType::Slash, "/"),
            (TType::Int, "2"),
            (TType::Percentage, "%"),
            (TType::Int, "3"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn comparison_and_logical_operators() {
    assert_tokens(
        "< > <= >= == != && ||",
        &[
            (TType::Lt, "<"),
            (TType::Gt, ">"),
            (TType::Lte, "<="),
            (TType::Gte, ">="),
            (TType::Eq, "=="),
            (TType::Neq, "!="),
            (TType::And, "&&"),
            (TType::Or, "||"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn assignment_family_is_disambiguated() {
    assert_tokens(
        "= == : :: += -= *= /= %=",
        &[
            (TType::Assign, "="),
            (TType::Eq, "=="),
            (TType::Colon, ":"),
            (TType::Scope, "::"),
            (TType::CompoundAdd, "+="),
            (TType::CompoundSub, "-="),
            (TType::CompoundMul, "*="),
            (TType::CompoundDiv, "/="),
            (TType::CompoundModulo, "%="),
            (TType::End, ""),
        ],
    );
}

#[test]
fn increment_decrement_and_bang_family() {
    assert_tokens(
        "++ -- ! != !! !?",
        &[
            (TType::PlusPlus, "++"),
            (TType::MinusMinus, "--"),
            (TType::Bang, "!"),
            (TType::Neq, "!="),
            (TType::DoubleExclaim, "!!"),
            (TType::Propagate, "!?"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn unary_pointer_and_other_operator_tokens() {
    assert_tokens(
        "~ @ ^ & | $ $$ ? ??",
        &[
            (TType::Tilde, "~"),
            (TType::At, "@"),
            (TType::Caret, "^"),
            (TType::Ampersand, "&"),
            (TType::Stick, "|"),
            (TType::Dollar, "$"),
            (TType::DoubleDollar, "$$"),
            (TType::QuestionMark, "?"),
            (TType::Coalesce, "??"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn delimiters_and_separators() {
    assert_tokens(
        "( ) { } [ ] , ; .",
        &[
            (TType::Lparen, "("),
            (TType::Rparen, ")"),
            (TType::LBrace, "{"),
            (TType::Rbrace, "}"),
            (TType::LBracket, "["),
            (TType::RBracket, "]"),
            (TType::Comma, ","),
            (TType::Semicolon, ";"),
            (TType::Dot, "."),
            (TType::End, ""),
        ],
    );
}

#[test]
fn angle_brackets_do_not_form_shift_tokens() {
    assert_tokens(
        "<< >> < <",
        &[
            (TType::Lt, "<"),
            (TType::Lt, "<"),
            (TType::Gt, ">"),
            (TType::Gt, ">"),
            (TType::Lt, "<"),
            (TType::Lt, "<"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn bitwise_operator_symbols_vs_keywords() {
    assert_tokens(
        "a & b | c ^ d shr and or xor not",
        &[
            (TType::Identifier, "a"),
            (TType::Ampersand, "&"),
            (TType::Identifier, "b"),
            (TType::Stick, "|"),
            (TType::Identifier, "c"),
            (TType::Caret, "^"),
            (TType::Identifier, "d"),
            (TType::Rightshift, "shr"),
            (TType::BitwiseAnd, "and"),
            (TType::BitwiseOr, "or"),
            (TType::Xor, "xor"),
            (TType::Not, "not"),
            (TType::End, ""),
        ],
    );
}

// Integer literals
#[test]
fn plain_integer_defaults_to_int() {
    assert_tokens("0", &[(TType::Int, "0"), (TType::End, "")]);
    assert_tokens("42", &[(TType::Int, "42"), (TType::End, "")]);
}

#[test]
fn every_integer_suffix_produces_its_own_token_type() {
    assert_tokens(
        "5i8 5u8 5i16 5u16 5i32 5u32 5i64 5u64 5i128 5u128 5iz 5uz",
        &[
            (TType::Int8, "5"),
            (TType::Uint8, "5"),
            (TType::Int16, "5"),
            (TType::Uint16, "5"),
            (TType::Int32, "5"),
            (TType::Uint32, "5"),
            (TType::Int64, "5"),
            (TType::Uint64, "5"),
            (TType::Int128, "5"),
            (TType::Uint128, "5"),
            (TType::IntSize, "5"),
            (TType::UintSize, "5"),
            (TType::End, ""),
        ],
    );
}

#[test]
fn hex_and_binary_literals_with_and_without_suffix() {
    assert_tokens("0xFF", &[(TType::Int, "0xFF"), (TType::End, "")]);
    assert_tokens("0Xff", &[(TType::Int, "0Xff"), (TType::End, "")]);
    assert_tokens("0xFFu32", &[(TType::Uint32, "0xFF"), (TType::End, "")]);
    assert_tokens("0x1Fi8", &[(TType::Int8, "0x1F"), (TType::End, "")]);
    assert_tokens("0b1010", &[(TType::Int, "0b1010"), (TType::End, "")]);
    assert_tokens(
        "0b10101010u8",
        &[(TType::Uint8, "0b10101010"), (TType::End, "")],
    );
    assert_tokens("0b1010uz", &[(TType::UintSize, "0b1010"), (TType::End, "")]);
}

#[test]
fn underscores_are_stripped_from_numeric_lexemes() {
    assert_tokens("1_000_000", &[(TType::Int, "1000000"), (TType::End, "")]);
    assert_tokens(
        "1_000_000u64",
        &[(TType::Uint64, "1000000"), (TType::End, "")],
    );
    assert_tokens(
        "0xdead_beefu64",
        &[(TType::Uint64, "0xdeadbeef"), (TType::End, "")],
    );
    assert_tokens(
        "0b1111_0000u8",
        &[(TType::Uint8, "0b11110000"), (TType::End, "")],
    );
    assert_tokens("5_", &[(TType::Int, "5"), (TType::End, "")]);
}

#[test]
fn standalone_hex_without_digits_is_an_error() {
    assert_lexer_error("0x", "expected hex digit after '0x'", true);
}

#[test]
fn standalone_binary_without_digits_is_an_error() {
    assert_lexer_error("0b", "expected binary digit after '0b'", true);
}

#[test]
fn hex_prefix_followed_by_underscore_only_is_an_error() {
    assert_lexer_error("0x_z", "expected hex digit after '0x'", true);
}

// Float literals
#[test]
fn plain_floats_default_to_float() {
    assert_tokens("1.5", &[(TType::Float, "1.5"), (TType::End, "")]);
    assert_tokens("0.5", &[(TType::Float, "0.5"), (TType::End, "")]);
    assert_tokens("1.", &[(TType::Float, "1."), (TType::End, "")]);
    assert_tokens("1_000.5", &[(TType::Float, "1000.5"), (TType::End, "")]);
}

#[test]
fn float_suffixes_produce_f32_and_f64() {
    assert_tokens("1.5f32", &[(TType::F32, "1.5"), (TType::End, "")]);
    assert_tokens("1.5f64", &[(TType::F64, "1.5"), (TType::End, "")]);
    assert_tokens("0.5f32", &[(TType::F32, "0.5"), (TType::End, "")]);
    assert_tokens("0.0f64", &[(TType::F64, "0.0"), (TType::End, "")]);
}

#[test]
fn hex_then_dot_splits_into_int_and_dot() {
    assert_tokens(
        "0xF.",
        &[(TType::Int, "0xF"), (TType::Dot, "."), (TType::End, "")],
    );
}

// Char literals
#[test]
fn default_char_is_char8() {
    assert_tokens("'a'", &[(TType::Char8Literal, "a"), (TType::End, "")]);
}

#[test]
fn char_suffix_selects_width() {
    assert_tokens("'a'c8", &[(TType::Char8Literal, "a"), (TType::End, "")]);
    assert_tokens("'a'c16", &[(TType::Char16Literal, "a"), (TType::End, "")]);
    assert_tokens("'a'c32", &[(TType::Char32Literal, "a"), (TType::End, "")]);
}

#[test]
fn char_escapes() {
    assert_tokens("'\\n'", &[(TType::Char8Literal, "\n"), (TType::End, "")]);
    assert_tokens("'\\t'", &[(TType::Char8Literal, "\t"), (TType::End, "")]);
    assert_tokens("'\\\\'", &[(TType::Char8Literal, "\\"), (TType::End, "")]);
    assert_tokens("'\\''", &[(TType::Char8Literal, "'"), (TType::End, "")]);
    assert_tokens("'\\0'", &[(TType::Char8Literal, "\0"), (TType::End, "")]);
}

#[test]
fn char_not_fitting_in_char8_is_an_error() {
    assert_lexer_error("'Ω'", "does not fit in default char8", true);
}

#[test]
fn unknown_char_escape_is_an_error() {
    assert_lexer_error("'\\q'", "Unknown escape sequence: \\q", false);
}

#[test]
fn char_with_missing_closing_quote_is_an_error() {
    let result = lex("'ab'");
    assert!(result.corrupted);
    let first = &result.errors[0];
    assert!(
        matches!(first.phase, Phase::Lexer),
        "unexpected phase: {:?}",
        first.phase
    );
    assert!(
        first
            .message
            .contains("Expected closing ' for char literal"),
        "message was {:?}",
        first.message
    );
}

// String literals
#[test]
fn plain_string() {
    assert_tokens("\"hi\"", &[(TType::StringLiteral, "hi"), (TType::End, "")]);
}

#[test]
fn string_escapes_are_unwrapped() {
    assert_tokens(
        "\"a\\nb\\t\\\\c\\\"d\\0e\"",
        &[(TType::StringLiteral, "a\nb\t\\c\"d\0e"), (TType::End, "")],
    );
}

#[test]
fn unicode_escape_in_string() {
    assert_tokens(
        "\"\\u{1F600}\"",
        &[(TType::StringLiteral, "😀"), (TType::End, "")],
    );
}

#[test]
fn multibyte_character_inside_string_is_preserved() {
    assert_tokens("\"𝄞\"", &[(TType::StringLiteral, "𝄞"), (TType::End, "")]);
}

#[test]
fn unknown_string_escape_is_an_error() {
    assert_lexer_error("\"\\q\"", "Unknown escape sequence: \\q", false);
}

#[test]
fn unterminated_string_is_an_error() {
    assert_lexer_error("\"abc", "Unterminated string literal", false);
}

#[test]
fn trailing_backslash_at_end_of_string_is_an_error() {
    assert_lexer_error("\"abc\\", "Unexpected end of file in string literal", false);
}

// Illegal characters
#[test]
fn backtick_is_an_illegal_character() {
    let result = lex("`");
    assert!(result.corrupted);
    let tokens = &result.tokens;
    assert_eq!(tokens[0].token_type, TType::Illegal);
    assert_lexer_error("`", "Invalid character", true);
}

#[test]
fn non_ascii_character_outside_string_is_illegal() {
    let result = lex("𝄞");
    assert!(result.corrupted);
    assert_eq!(result.tokens[0].token_type, TType::Illegal);
    assert_lexer_error("𝄞", "Invalid character", true);
}

#[test]
fn octal_literals_are_read() {
    assert_tokens("0o17", &[(TType::Int, "0o17"), (TType::End, "")]);
    assert_tokens("0O17", &[(TType::Int, "0O17"), (TType::End, "")]);
    assert_tokens("0o1_7u8", &[(TType::Uint8, "0o17"), (TType::End, "")]);
    assert_tokens("0o17i64", &[(TType::Int64, "0o17"), (TType::End, "")]);
    assert_tokens("0o777", &[(TType::Int, "0o777"), (TType::End, "")]);
    assert_spans("0o17", &[(0, 4), (4, 4)]);
}

#[test]
fn standalone_octal_without_digits_is_an_error() {
    let result = lex("0o");
    assert!(result.corrupted);
    assert_eq!(result.tokens[0].token_type, TType::Illegal);
    assert_lexer_error("0o", "expected octal digit after '0o'", true);
}

#[test]
fn unknown_integer_suffix_is_reported() {
    let result = lex("123abc");
    assert!(result.corrupted);
    assert_eq!(result.tokens[0].token_type, TType::Int);
    assert_eq!(result.tokens[0].lexeme, "123");
    assert_eq!(result.tokens[1].token_type, TType::Identifier);
    assert_eq!(result.tokens[1].lexeme, "abc");
    assert!(result.errors[0].message.contains("Invalid suffix 'abc'"));
}

#[test]
fn single_letter_integer_suffix_is_reported() {
    let result = lex("5u");
    assert!(result.corrupted);
    assert_eq!(result.tokens[0].token_type, TType::Int);
    assert_eq!(result.tokens[0].lexeme, "5");
    assert_eq!(result.tokens[1].token_type, TType::Identifier);
    assert_eq!(result.tokens[1].lexeme, "u");
    assert_lexer_error("5u", "Invalid suffix 'u'", true);
}

#[test]
fn unknown_float_suffix_is_reported() {
    let result = lex("1.5x");
    assert!(result.corrupted);
    assert_eq!(result.tokens[0].token_type, TType::Float);
    assert_eq!(result.tokens[0].lexeme, "1.5");
    assert_eq!(result.tokens[1].token_type, TType::Identifier);
    assert_eq!(result.tokens[1].lexeme, "x");
    assert!(result.errors[0].message.contains("Invalid suffix 'x'"));
}

#[test]
fn float_exponents_are_supported() {
    assert_tokens("1e5", &[(TType::Float, "1e5"), (TType::End, "")]);
    assert_tokens("1.5e3", &[(TType::Float, "1.5e3"), (TType::End, "")]);
    assert_tokens("1.5E3", &[(TType::Float, "1.5E3"), (TType::End, "")]);
    assert_tokens("1.5e-3", &[(TType::Float, "1.5e-3"), (TType::End, "")]);
    assert_tokens("2e+4", &[(TType::Float, "2e+4"), (TType::End, "")]);
    assert_tokens("1.5e3f32", &[(TType::F32, "1.5e3"), (TType::End, "")]);
    assert_tokens("1e5f64", &[(TType::F64, "1e5"), (TType::End, "")]);
    assert_spans("1e5", &[(0, 3), (3, 3)]);
}

#[test]
fn multibyte_char_literals_suffix_correctly() {
    assert_tokens("'Ω'c16", &[(TType::Char16Literal, "Ω"), (TType::End, "")]);
    assert_tokens("'Ω'c32", &[(TType::Char32Literal, "Ω"), (TType::End, "")]);
    assert_tokens("'😀'c32", &[(TType::Char32Literal, "😀"), (TType::End, "")]);
    assert_spans("'Ω'c32", &[(0, 7), (7, 7)]);
    assert_spans("'😀'c32", &[(0, 9), (9, 9)]);
}

#[test]
fn multibyte_string_span_is_byte_exact() {
    assert_spans("\"𝄞\"", &[(0, 6), (6, 6)]);
}

#[test]
fn illegal_multibyte_character_span_covers_all_bytes() {
    let result = lex("𝄞");
    assert_eq!(result.tokens[0].span, Span { start: 0, end: 4 });
}

#[test]
fn unicode_escape_in_char() {
    assert_tokens("'\\u{62}'", &[(TType::Char8Literal, "b"), (TType::End, "")]);
    assert_tokens(
        "'\\u{3A9}'c16",
        &[(TType::Char16Literal, "Ω"), (TType::End, "")],
    );
    assert_tokens(
        "'\\u{1F600}'c32",
        &[(TType::Char32Literal, "😀"), (TType::End, "")],
    );
}

#[test]
fn unterminated_block_comment_span_does_not_exceed_eof() {
    let result = lex("## unterminated");
    assert!(result.corrupted);
    assert_eq!(result.errors[0].message, "Unterminated multi-line comment");
    let span = result.errors[0].span.as_ref().unwrap();
    assert_eq!(span.start, 0);
    assert_eq!(span.end, "## unterminated".len());
    let result = lex("##");
    let span = result.errors[0].span.as_ref().unwrap();
    assert_eq!(*span, Span { start: 0, end: 2 });
}

#[test]
fn tuple_member_chain_lexes_float_for_digit_access() {
    assert_tokens(
        "tup.0.1",
        &[
            (TType::Identifier, "tup"),
            (TType::Dot, "."),
            (TType::Float, "0.1"),
            (TType::End, ""),
        ],
    );
}

// ---------------------------------------------------------------------------
// Spans
// ---------------------------------------------------------------------------

#[test]
fn spans_cover_the_full_literal_including_suffix() {
    assert_spans("5u32", &[(0, 4), (4, 4)]);
    assert_spans("1_000u64", &[(0, 8), (8, 8)]);
    assert_spans("0xFF", &[(0, 4), (4, 4)]);
    assert_spans("1.5f32", &[(0, 6), (6, 6)]);
    assert_spans("'a'c32", &[(0, 6), (6, 6)]);
    assert_spans("\"str\"", &[(0, 5), (5, 5)]);
}

#[test]
fn spans_are_byte_exact_on_ascii_statements() {
    let src = "mut var x = 12;";
    assert_spans(
        src,
        &[
            (0, 3),   // mut
            (4, 7),   // var
            (8, 9),   // x
            (10, 12), // =
            (13, 15), // 12
            (15, 16), // ;
            (16, 16), // End
        ],
    );
}

#[test]
fn spans_delimit_each_operator() {
    let src = "<< >> $ $$ ??";
    assert_spans(
        src,
        &[
            (0, 1),   // <
            (1, 2),   // <
            (3, 4),   // >
            (4, 5),   // >
            (6, 7),   // $
            (8, 10),  // $$
            (11, 13), // ??
            (13, 13), // End
        ],
    );
}
