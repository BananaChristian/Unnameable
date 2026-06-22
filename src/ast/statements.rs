use crate::{
    ast::{Expr, Qualifier, Type},
    diagnostics::Span,
    lexer::{TType, token::Token},
};

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
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
    Block{
        content: Vec<Stmt>
    },
    FunctionDecl {
        name: Box<Expr>,
        params: Vec<Stmt>,
        type_annotation: Option<Type>,
        body: Option<Box<Stmt>>,
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
        matches!(token.token_type, TType::Var|TType::Func)
    }
}
