use crate::{
    diagnostics::Span,
    hir::{
        expressions::HirExpr,
        types::HirTypeNode,
    },
};

pub struct QualifierMap {
    pub mutable: bool,
    pub expose: bool,
    pub constant: bool,
    pub heap: bool,
}

impl QualifierMap {
    pub fn new() -> Self {
        QualifierMap {
            mutable: false,
            expose: false,
            constant: false,
            heap: false,
        }
    }

}

#[derive(Debug)]
pub struct HirStmt {
    pub kind: HirStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct HirParam {
    pub name: String,
    pub ty: HirTypeNode,
    pub mutable: bool,
    pub default: Option<HirExpr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct HirEnumMember {
    pub name: String,
    pub value: i64, // always resolved, auto incremented if not specified
    pub span: Span,
}

#[derive(Debug)]
pub struct HirVariantMember {
    pub name: String,
    pub member_types: Vec<HirTypeNode>, // empty vec if no data
    pub tag: u32,                       // compiler assigned tag value
    pub span: Span,
}

#[derive(Debug)]
pub enum HirStmtKind {
    HirReturn(Option<HirExpr>),
    HirBreak,
    HirContinue,
    HirExpr(HirExpr),
    HirVarDecl {
        name: String,
        mutable: bool,
        constant: bool,
        heap: bool,
        exposed: bool,
        ty: Option<HirTypeNode>,
        init: Option<HirExpr>,
    },
    HirFunctionDef {
        name: String,             // mangled name like Food_add, Point_distance etc
        params: Vec<HirParam>,    // clean param structs, not Stmts
        return_type: HirTypeNode, // never optional, Unit if not specified
        type_params: Vec<String>,
        exposed: bool,      // was in qualifiers
        body: Vec<HirStmt>, // flat list, no Block wrapper
    },

    HirFunctionDecl {
        name: String,
        params: Vec<HirParam>,
        return_type: HirTypeNode, // Unit if not specified
        type_params: Vec<String>,
        exposed: bool,
    },
    HirStructDecl {
        name: String,
        contracts: Vec<String>,
        type_params: Vec<String>,
        fields: Vec<HirParam>,
        exposed: bool,
        span: Span,
    },
    HirIf {
        condition: HirExpr,
        body: Vec<HirStmt>,
        else_body: Option<Vec<HirStmt>>, // elif desugared into nested if here
    },
    HirContractDecl {
        name: String,
        functions: Vec<HirStmt>,
        exposed: bool,
    },
    HirWhile {
        condition: HirExpr,
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
        contracts: Vec<String>,
        members: Vec<HirVariantMember>,
        type_params: Vec<String>, // from generic block if any
        exposed: bool,
        span: Span,
    },
}
