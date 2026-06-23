use crate::{
    ast::Expr,
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
    Array(Box<Type>, Option<Expr>),

    // Custom/User types
    CustomType(Box<Expr>),

    // Placeholder
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl Type {
    pub fn basic(token: &Token) -> Self {
        let kind = match token.token_type {
            TType::I8Key => TypeKind::I8,
            TType::U8Key => TypeKind::U8,
            TType::I16Key => TypeKind::I16,
            TType::U16Key => TypeKind::U16,
            TType::I32Key => TypeKind::I32,
            TType::U32key => TypeKind::U32,
            TType::I64Key => TypeKind::I64,
            TType::U64Key => TypeKind::U64,
            TType::I128Key => TypeKind::I128,
            TType::U128Key => TypeKind::U128,
            TType::ISIZEKey => TypeKind::ISIZE,
            TType::USIZEKey => TypeKind::USIZE,
            TType::BoolKey => TypeKind::Bool,
            TType::F32Key => TypeKind::F32,
            TType::F64Key => TypeKind::F64,
            _ => TypeKind::None,
        };

        Type {
            kind,
            span: token.clone().span,
        }
    }

    pub fn complex(token: &Token, inner: Type) -> Self {
        let kind = match token.token_type {
            TType::Ptr => TypeKind::Ptr(Box::new(inner.clone())),
            TType::Ref => TypeKind::Ref(Box::new(inner.clone())),
            _ => TypeKind::None,
        };

        // ⭐ Just use start/end byte offsets
        let span = Span {
            start: token.span.start,
            end: inner.span.end,
        };

        Type { kind, span }
    }

    pub fn array(inner: Type, array_size: Option<Expr>) -> Self {
        let kind = TypeKind::Array(Box::new(inner.clone()), array_size);
        let span = inner.span.clone();

        Type { kind, span }
    }

    pub fn custom(expr: Expr) -> Self {
        Type {
            kind: TypeKind::CustomType(Box::new(expr.clone())),
            span: expr.span,
        }
    }
}

