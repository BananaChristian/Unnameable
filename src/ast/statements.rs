use crate::{
    ast::{Expr, Qualifier, Type},
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Elif {
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMember {
    pub name: Expr,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantMember {
    pub name: Expr,
    pub member_types: Vec<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Expr(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
    VarDecl {
        qualifiers: Vec<Qualifier>,    //Things like mut
        type_annotation: Option<Type>, //The type like u8
        name: Box<Expr>,               //The name of the variable
        init: Box<Expr>,
    },
    ParamDecl {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        type_annotation: Type,
        def: Option<Box<Expr>>,
    },
    FunctionDef {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        params: Vec<Stmt>,
        type_annotation: Option<Type>,
        body: Box<Expr>,
    },
    FunctionDecl {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        params: Vec<Stmt>,
        type_annotation: Option<Type>,
    },
    StructDecl {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        contracts: Vec<Type>,
        contents: Box<Expr>,
    },
    SealStmt {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        contents: Vec<Stmt>,
    },
    AliasStmt {
        original: Box<Type>,
        new: Box<Expr>,
    },
    ImportStmt {
        name: Box<Expr>,
        alias: Option<Expr>,
    },
    IfStmt {
        condition: Box<Expr>,
        body: Box<Expr>,
        elifs: Vec<Elif>,
        else_body: Option<Box<Expr>>,
    },
    GenericBlock {
        params: Vec<Type>,
        body: Box<Expr>,
    },
    ContractBlock {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        body: Vec<Stmt>,
    },
    WhileStmt {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
    ForStmt {
        init: Box<Stmt>,
        condition: Box<Expr>,
        update: Box<Expr>,
        body: Box<Expr>,
    },
    EachStmt {
        item: Box<Expr>,
        collection: Box<Expr>,
        body: Box<Expr>,
    },
    EnumStmt {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        underlying: Option<Type>,
        content: Vec<EnumMember>,
    },
    VariantStmt {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        contracts: Vec<Type>,
        body: Vec<VariantMember>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt { kind, span }
    }

    pub fn is_valid(token: &Token) -> bool {
        matches!(
            token.token_type,
            TType::Var
                | TType::Func
                | TType::Struct
                | TType::Seal
                | TType::Alias
                | TType::Return
                | TType::Break
                | TType::Continue
        )
    }
}
