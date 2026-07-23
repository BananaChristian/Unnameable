use crate::{
    hir::{HirStmt, HirStmtKind},
    mir::builder::MIRBuilder,
};

impl<'a> MIRBuilder<'a> {
    pub fn build_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirExpr(_) => self.build_expr_stmt(stmt),
            _ => todo!("Implememt all the different statement handlers"),
        }
    }

    fn build_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            let instruction = self.build_expr(inner);
            self.add_instruction(instruction);
        }
    }
}
