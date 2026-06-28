use crate::{
    ast::Stmt,
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::HirStmt,
};

pub struct Lowering<'a> {
    ast: Vec<Stmt>,
    diagnostics: &'a mut Diagnostics,
    pub current_type_params: Vec<String>,
    pub iter_counter: u64,
    pub corrupted: bool,
}

impl<'a> Lowering<'a> {
    pub fn new(ast: Vec<Stmt>, diagnostics: &'a mut Diagnostics) -> Self {
        Lowering {
            ast,
            diagnostics,
            current_type_params: Vec::new(),
            iter_counter: 0,
            corrupted: false,
        }
    }

    pub fn lower(&mut self) -> Vec<HirStmt> {
        let mut hir = Vec::new();
        for stmt in self.ast.clone() {
            if let Some(hir_stmt) = self.lower_stmt(&stmt) {
                hir.push(hir_stmt);
            }
        }
        hir
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Lowering, span));
    }
}
