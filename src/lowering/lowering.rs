use crate::{
    ast::{Stmt, StmtKind},
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::HirStmt,
};

#[derive(Debug, Clone, Copy, PartialEq,Eq,Hash)]
pub struct NodeId(pub usize);

pub struct Lowering<'a> {
    ast: Vec<Stmt>,
    diagnostics: &'a mut Diagnostics,
    pub iter_counter: u64,
    pub node_counter: usize,
    pub corrupted: bool,
}

impl<'a> Lowering<'a> {
    pub fn new(ast: Vec<Stmt>, diagnostics: &'a mut Diagnostics) -> Self {
        Lowering {
            ast,
            diagnostics,
            iter_counter: 0,
            node_counter: 0,
            corrupted: false,
        }
    }

    pub fn next_id(&mut self) -> NodeId {
        let id = NodeId(self.node_counter);
        self.node_counter += 1;
        id
    }

    pub fn lower(&mut self) -> Vec<HirStmt> {
        let mut hir = Vec::new();
        for stmt in self.ast.clone() {
            match &stmt.kind {
                StmtKind::SealStmt { .. }
                | StmtKind::MethodsStmt { .. }
                | StmtKind::GenericBlock { .. }
                | StmtKind::ForStmt { .. }
                | StmtKind::EachStmt { .. } => {
                    if let Some(hir_stmts) = self.lower_constructs(&stmt) {
                        hir.extend(hir_stmts);
                    }
                }
                _ => {
                    if let Some(hir_stmt) = self.lower_stmt(&stmt) {
                        hir.push(hir_stmt);
                    }
                }
            }
        }
        hir
    }

    pub fn mangle_name(&mut self, prefix: String, name: String) -> String {
        format!("{}_{}", prefix, name)
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Lowering, span));
    }
}
