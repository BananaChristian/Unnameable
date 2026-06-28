use crate::{
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum QualifierKind {
    Mut,
    Const,
    Heap, //This will be revised
    Exposed,
    None,
}

#[derive(Debug, Clone)]
pub struct Qualifier {
    pub kind: QualifierKind,
    pub span: Span,
}

impl Qualifier {
    pub fn new(token: &Token) -> Self {
        let kind = match token.token_type {
            TType::Mut => QualifierKind::Mut,
            TType::Const => QualifierKind::Const,
            TType::Heap => QualifierKind::Heap,
            TType::Expose=> QualifierKind::Exposed,
            _ => QualifierKind::None,
        };

        Qualifier {
            kind,
            span: token.clone().span,
        }
    }

    pub fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TType::Mut | TType::Const | TType::Heap | TType::Expose
        )
    }
}
