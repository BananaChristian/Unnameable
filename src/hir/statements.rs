use crate::{
    diagnostics::Span,
    hir::{expressions::HirExpr, types::HirTypeNode},
    lowering::NodeId,
};

pub struct QualifierMap {
    pub mutable: bool,
    pub expose: bool,
    pub constant: bool,
    pub dollar_read: bool,
}

impl QualifierMap {
    pub fn new() -> Self {
        QualifierMap {
            mutable: false,
            expose: false,
            constant: false,
            dollar_read: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStmt {
    pub hir_id: NodeId,
    pub kind: HirStmtKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub hir_id: NodeId,
    pub name: String,
    pub ty: HirTypeNode,
    pub mutable: bool,
    pub dollar_read: bool,
    pub default: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnumMember {
    pub hir_id: NodeId,
    pub name: String,
    pub value: isize, // always resolved, auto incremented if not specified
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirVariantMember {
    pub hir_id: NodeId,
    pub name: String,
    pub member_types: Vec<HirTypeNode>, // empty vec if no data
    pub tag: u32,                       // compiler assigned tag value
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStmtKind {
    HirReturn(Option<Box<HirExpr>>),
    HirBreak,
    HirContinue,
    HirExpr(Box<HirExpr>),
    HirVarDecl {
        name: String,
        mutable: bool,
        constant: bool,
        dollar_read: bool,
        exposed: bool,
        ty: Option<HirTypeNode>,
        init: Box<HirExpr>, //Must initialize
    },

    HirFunctionDef {
        name: String,             // mangled name like Food_add, Point_distance etc
        params: Vec<HirParam>,    // clean param structs, not Stmts
        return_type: HirTypeNode, // never optional, Unit if not specified
        generic_type_params: Vec<HirTypeNode>,
        exposed: bool, // was in qualifiers
        dollar_read: bool,
        body: Vec<HirStmt>, // flat list, no Block wrapper
    },

    HirFunctionDecl {
        name: String,
        params: Vec<HirParam>,
        return_type: HirTypeNode, // Unit if not specified
        generic_type_params: Vec<HirTypeNode>,
        exposed: bool,
    },
    HirStructDecl {
        name: String,
        contracts: Vec<HirTypeNode>,
        generic_type_params: Vec<HirTypeNode>,
        fields: Vec<HirParam>,
        exposed: bool,
    },
    HirIf {
        condition: Box<HirExpr>,
        body: Vec<HirStmt>,
        else_body: Option<Vec<HirStmt>>, // elif desugared into nested if here
    },
    HirContractDecl {
        name: String,
        functions: Vec<HirStmt>,
        generic_type_params: Vec<HirTypeNode>,
        exposed: bool,
    },
    HirAlias {
        original: Box<HirTypeNode>,
        alias: String,
    },
    HirImport {
        name: String,
        alias: Option<String>,
    },
    HirWhile {
        condition: Box<HirExpr>,
        body: Vec<HirStmt>, // Block wrapper gone, flat list
    },
    HirEnumDecl {
        name: String,
        underlying: HirTypeNode,     // always resolved
        members: Vec<HirEnumMember>, // values always resolved
        exposed: bool,
    },
    HirVariantDecl {
        name: String,
        contracts: Vec<HirTypeNode>,
        members: Vec<HirVariantMember>,
        generic_type_params: Vec<HirTypeNode>, // from generic block if any
        exposed: bool,
    },
}
