use crate::{
    ast::{Expr, Qualifier, Type},
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug)]
pub struct Elif {
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct EnumMember {
    pub name: Expr,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct VariantMember {
    pub name: Expr,
    pub member_types: Vec<Type>,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
    VarDecl {
        qualifiers: Vec<Qualifier>,    //Things like mut
        type_annotation: Option<Type>, //The type like u8
        name: Box<Expr>,               //The name of the variable
        init: Option<Box<Expr>>,
    },
    ParamDecl {
        qualifiers: Vec<Qualifier>,
        name: Box<Expr>,
        type_annotation: Type,
        def: Option<Box<Expr>>,
    },
    Block {
        content: Vec<Stmt>,
    },
    FunctionDef {
        name: Box<Expr>,
        params: Vec<Stmt>,
        type_annotation: Option<Type>,
        body: Box<Stmt>,
    },
    FunctionDecl {
        name: Box<Expr>,
        params: Vec<Stmt>,
        type_annotation: Option<Type>,
    },
    StructDecl {
        name: Box<Expr>,
        contracts: Vec<Expr>,
        contents: Box<Stmt>,
    },
    SealStmt {
        name: Box<Expr>,
        contents: Vec<Stmt>,
    },
    MethodsStmt {
        name: Box<Expr>,
        contents: Vec<Stmt>,
    },
    IfStmt {
        condition: Box<Expr>,
        body: Box<Stmt>,
        elifs: Vec<Elif>,
        else_body: Option<Box<Stmt>>,
    },
    GenericBlock {
        params: Vec<Type>,
        body: Box<Stmt>,
    },
    ContractBlock {
        name: Box<Expr>,
        body: Vec<Stmt>,
    },
    WhileStmt {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    ForStmt {
        init: Box<Stmt>,
        condition: Box<Expr>,
        update: Box<Expr>,
        body: Box<Stmt>,
    },
    EachStmt {
        item: Box<Expr>,
        collection: Box<Expr>,
        body: Box<Stmt>,
    },
    EnumStmt {
        name: Box<Expr>,
        underlying: Option<Type>,
        content: Vec<EnumMember>,
    },
    VariantStmt {
        name: Box<Expr>,
        contracts: Vec<Expr>,
        body: Vec<VariantMember>,
    },
}

#[derive(Debug)]
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
                | TType::Methods
                | TType::Return
                | TType::Break
                | TType::Continue
        )
    }
}
