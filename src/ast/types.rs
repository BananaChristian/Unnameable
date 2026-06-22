use crate::{
    ast::{Expr, Qualifier},
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    // Integer types
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    ISIZE,
    USIZE,

    // Float types
    F32,
    F64,

    // Boolean
    Bool,

    // Pointer types
    Ptr(Box<Type>),
    Ref(Box<Type>),

    // Array
    Array(Box<Type>),

    // Custom/User types
    Custom(Expr),

    // Placeholder
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    kind: TypeKind,
    pub span: Span,
}

impl Type {
    pub fn basic(token: &Token) -> Self {
        match token.token_type {
            TType::I8Key => Type {
                kind: TypeKind::I8,
                span: Span::from_token(token),
            },
            TType::U8Key => Type {
                kind: TypeKind::U8,
                span: Span::from_token(token),
            },
            TType::I16Key => Type {
                kind: TypeKind::I16,
                span: Span::from_token(token),
            },
            TType::U16Key => Type {
                kind: TypeKind::U16,
                span: Span::from_token(token),
            },
            TType::I32Key => Type {
                kind: TypeKind::I32,
                span: Span::from_token(token),
            },
            TType::U32key => Type {
                kind: TypeKind::U32,
                span: Span::from_token(token),
            },
            TType::I64Key => Type {
                kind: TypeKind::I64,
                span: Span::from_token(token),
            },
            TType::U64Key => Type {
                kind: TypeKind::U64,
                span: Span::from_token(token),
            },
            TType::I128Key => Type {
                kind: TypeKind::I128,
                span: Span::from_token(token),
            },
            TType::U128Key => Type {
                kind: TypeKind::U128,
                span: Span::from_token(token),
            },
            _ => Type {
                kind: TypeKind::None,
                span: Span::from_token(token),
            },
        }
    }

    pub fn complex(token: &Token, inner: Type) -> Type {
        match token.token_type {
            TType::Ptr => Type {
                kind: TypeKind::Ptr(Box::new(inner.clone())),
                span: Span::merge(&Span::from_token(token), &inner.span),
            },
            TType::Ref => Type {
                kind: TypeKind::Ref(Box::new(inner.clone())),
                span: Span::merge(&Span::from_token(token), &inner.span),
            },
            _ => Type {
                kind: TypeKind::None,
                span: Span::from_token(token),
            },
        }
    }

    pub fn none() -> Type {
        Type {
            kind: TypeKind::None,
            span: Span::fresh(),
        }
    }
}
