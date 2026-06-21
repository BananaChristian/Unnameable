use crate::{
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug)]
pub enum QualifierKind {
    Mut,
    Const,
    Heap,
    None,
}

#[derive(Debug)]
pub struct Qualifier {
    kind: QualifierKind,
    pub span: Span,
}

impl Qualifier {
    pub fn new(token: &Token) -> Self {
        match token.token_type {
            TType::Mut => Qualifier {
                kind: QualifierKind::Mut,
                span: Span::from_token(token),
            },
            TType::Const => Qualifier {
                kind: QualifierKind::Const,
                span: Span::from_token(token),
            },
            TType::Heap => Qualifier {
                kind: QualifierKind::Heap,
                span: Span::from_token(token),
            },
            _ => Qualifier {
                kind: QualifierKind::None,
                span: Span::from_token(token),
            },
        }
    }

    pub fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TType::Mut
                | TType::Const
                | TType::Heap
        )
    }
}
