use crate::diagnostics::Span;

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
    Ptr(Box<HirType>),
    Ref(Box<HirType>),
    // Array — size is a resolved constant, not an Expr
    Array(Box<HirType>, Option<u64>),
    // Function pointer
    Func(Vec<HirType>, Option<Box<HirType>>),
    // Custom type — path fully resolved to a string
    CustomType(String),
    // Generic type — name resolved, type params are HirTypes
    GenericType {
        name: String,
        type_params: Vec<HirType>,
    },
    // Nullable — (T)?
    Nullable(Box<HirType>),
    // Failable — !!(T, E)
    Failable(Box<HirType>, Box<HirType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeNode {
    pub kind: HirType,
    pub span: Span,
}

impl HirTypeNode {
    pub fn new(kind: HirType, span: Span) -> Self {
        HirTypeNode { kind, span }
    }

    pub fn primitive(kind: HirType, span: Span) -> Self {
        HirTypeNode { kind, span }
    }

    pub fn ptr(inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Ptr(Box::new(inner.kind)),
            span,
        }
    }

    pub fn ref_type(inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Ref(Box::new(inner.kind)),
            span,
        }
    }

    pub fn array(inner: HirTypeNode, size: Option<u64>, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Array(Box::new(inner.kind), size),
            span,
        }
    }

    pub fn func(params: Vec<HirType>, return_type: Option<HirTypeNode>, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Func(
                params,
                return_type.map(|r| Box::new(r.kind)),
            ),
            span,
        }
    }

    pub fn custom(name: String, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::CustomType(name),
            span,
        }
    }

    pub fn generic(name: String, type_params: Vec<HirType>, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::GenericType { name, type_params },
            span,
        }
    }

    pub fn nullable(inner: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Nullable(Box::new(inner.kind)),
            span,
        }
    }

    pub fn failable(ok: HirTypeNode, err: HirTypeNode, span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Failable(Box::new(ok.kind), Box::new(err.kind)),
            span,
        }
    }

    pub fn unit(span: Span) -> Self {
        HirTypeNode {
            kind: HirType::Unit,
            span,
        }
    }
}
