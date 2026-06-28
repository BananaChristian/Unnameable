use crate::{
    ast::{Qualifier, QualifierKind, Stmt},
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirStmt, QualifierMap},
};

pub struct Lowering<'a> {
    ast: Vec<Stmt>,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> Lowering<'a> {
    pub fn new(ast: Vec<Stmt>, diagnostics: &'a mut Diagnostics) -> Self {
        Lowering {
            ast,
            diagnostics,
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

    pub fn convert_qualifier(&mut self,qualifier: &Qualifier) -> Option<QualifierMap>{
        let mut map =QualifierMap::new();
        let result=match qualifier.kind{
            QualifierKind::Mut => 

        }


    }

}
