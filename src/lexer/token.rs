#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TType {
    //Signs
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
    Colon,      //:
    Comma,      //,
    Eq,         //==
    Bind,       //:=
    Assign,     //=
    Neq,        // '!='
    And,        //&&
    Or,         // ||
    Scope,      //::
    Semicolon,  //;
    Gt,         //>
    Rightshift, //>>
    Lt,         //<
    Leftshift,  //<<
    Gte,        //>=
    Lte,        //<=
    Star,       // *
    Slash,      // /
    Percentage, // %
    Bang,       // !
    Tilde,      //~
    Stick,      // |
    Ampersand,  //&

    // Number types
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uint128,
    IntSize,  // iz
    UintSize, // uz
    Float,
    F32,
    F64,

    Identifier,

    //Keywords
    Mut,
    Const,
    Var,
    Heap,
    Func,
    Return,
    Break,
    Continue,
    True,
    False,
    If,
    Elif,
    Else,
    Struct,

    I8Key,
    U8Key,
    I16Key,
    U16Key,
    I32Key,
    U32key,
    I64Key,
    U64Key,
    I128Key,
    U128Key,
    ISIZEKey,
    USIZEKey,
    BoolKey,
    F32Key,
    F64Key,
    Ptr,
    Ref,
    Arr,

    //Brackets
    Lparen, // (
    Rparen, //)
    LBrace, //{
    Rbrace, //}

    Illegal,
    End,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line: usize,
    pub col: usize,
    pub lexeme: String,
    pub token_type: TType,
}

impl Token {
    pub fn new(line: usize, col: usize, lexeme: String, token_type: TType) -> Self {
        Token {
            line: line,
            col: col,
            lexeme: lexeme,
            token_type: token_type,
        }
    }
}
