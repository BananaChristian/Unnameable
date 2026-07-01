use crate::{diagnostics::Span, lowering::NodeId};

#[derive(Debug, Clone, PartialEq)]
pub enum HirType {
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
    ISize,
    USize,
    // Float types
    F32,
    F64,
    // Boolean
    Bool,
    // Unit — ()
    Unit,
    // Pointer types
    Ptr(Box<HirTypeNode>),
    Ref(Box<HirTypeNode>),
    // Array — size is a resolved constant, not an Expr
    Array(Box<HirTypeNode>, Option<u64>),
    // Function pointer
    Func(Vec<HirTypeNode>, Box<HirTypeNode>),
    // Custom type — path fully resolved to a string
    CustomType(String),
    // Generic type — name resolved, type params are HirTypes
    GenericType {
        name: String,
        type_params: Vec<HirTypeNode>,
    },
    // Nullable — (T)?
    Nullable(Box<HirTypeNode>),
    // Failable — !!(T, E)
    Failable(Box<HirTypeNode>, Box<HirTypeNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeNode {
    pub hir_id: NodeId,
    pub kind: HirType,
    pub span: Span,
}

impl HirTypeNode {
    pub fn new(id: NodeId, kind: HirType, span: Span) -> Self {
        HirTypeNode {
            hir_id: id,
            kind,
            span,
        }
    }

    pub fn primitive(id: NodeId, kind: HirType, span: Span) -> Self {
        HirTypeNode {
            hir_id: id,
            kind,
            span,
        }
    }

    pub fn ptr(hir_id: NodeId, inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Ptr(Box::new(inner)),
            span,
        }
    }

    pub fn ref_type(hir_id: NodeId, inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Ref(Box::new(inner)),
            span,
        }
    }

    pub fn array(hir_id: NodeId, inner: HirTypeNode, size: Option<u64>, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Array(Box::new(inner), size),
            span,
        }
    }

    pub fn func(
        hir_id: NodeId,
        params: Vec<HirTypeNode>,
        return_type: HirTypeNode,
        span: Span,
    ) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Func(params, Box::new(return_type)),
            span,
        }
    }

    pub fn custom(hir_id: NodeId, name: String, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::CustomType(name),
            span,
        }
    }

    pub fn generic(
        hir_id: NodeId,
        name: String,
        type_params: Vec<HirTypeNode>,
        span: Span,
    ) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::GenericType { name, type_params },
            span,
        }
    }

    pub fn nullable(hir_id: NodeId, inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Nullable(Box::new(inner)),
            span,
        }
    }

    pub fn failable(hir_id: NodeId, ok: HirTypeNode, err: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Failable(Box::new(ok), Box::new(err)),
            span,
        }
    }

    pub fn unit(hir_id: NodeId, span: Span) -> Self {
        HirTypeNode {
            hir_id,
            kind: HirType::Unit,
            span,
        }
    }
}
