use crate::{
    ast::{Stmt, StmtKind, Type},
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::HirStmt
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId {
    pub local: usize,    //This is for nodes born within this particular module
    pub external: usize, //This is to be used externally by the import engine
}

pub struct Lowering {
    ast: Vec<Stmt>,
    diagnostics: SharedDiagnostics,
    pub iter_counter: u64,
    pub node_counter: usize,
    pub current_generic_params: Vec<Type>,
    pub corrupted: bool,
}

impl Lowering {
    pub fn new(ast: Vec<Stmt>, diagnostics: SharedDiagnostics) -> Self {
        Lowering {
            ast,
            diagnostics,
            iter_counter: 0,
            node_counter: 0,
            current_generic_params: Vec::new(),
            corrupted: false,
        }
    }

    pub fn next_id(&mut self) -> NodeId {
        let id = NodeId {
            local: self.node_counter,
            external: 0,
        };
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
            .borrow_mut()
            .report(CompilerError::error(message, Phase::Lowering, span));
    }
}
