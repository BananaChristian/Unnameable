use crate::diagnostics::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TType {
    //Signs
    Plus,
    Minus,
    PlusPlus,
    MinusMinus,
    Colon,         //:
    Comma,         //,
    Eq,            //==
    Bind,          //:=
    Assign,        //=
    Neq,           // '!='
    And,           //&&
    Or,            // ||
    Scope,         //::
    Semicolon,     //;
    Gt,            //>
    Rightshift,    //>>
    Lt,            //<
    Leftshift,     //<<
    Gte,           //>=
    Lte,           //<=
    Star,          // *
    Slash,         // /
    Percentage,    // %
    Bang,          // !
    Tilde,         //~
    Stick,         // |
    Ampersand,     //&
    QuestionMark,  //?
    Coalesce,      //??
    DoubleExclaim, //'!!'
    Propagate,     //'!?'

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
    Seal,

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
    Lparen,   // (
    Rparen,   //)
    LBrace,   //{
    Rbrace,   //}
    LBracket, //[
    RBracket, //]

    Illegal,
    End,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: String,
    pub token_type: TType,
    pub span: Span,
}

impl Token {
    pub fn new(lexeme: String, token_type: TType, span: Span) -> Self {
        Token {
            lexeme,
            token_type,
            span,
        }
    }
}
