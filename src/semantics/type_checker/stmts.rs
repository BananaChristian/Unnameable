use crate::{
    hir::{HirStmt, HirStmtKind},
    semantics::{type_checker::checker::TypeChecker},
};

impl<'a> TypeChecker<'a> {
    pub fn check_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirExpr(..) => self.check_expr_stmt(stmt),
            _ => (),
        }
    }

    fn check_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(expr) = &stmt.kind {
            self.check_expr(expr);
        }
    }

    fn check_if(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &stmt.kind
        {
            let cond_ty = self.expr_type(condition);
            let bool_ty = self.boolean(condition.span.clone());
            if !self.types_match(&bool_ty, &cond_ty) {
                self.type_mismatch(&bool_ty, &cond_ty, condition.span.clone());
            }

            for st in body {
                self.check_stmt(st);
            }

            if let Some(el_parts) = else_body {
                for el in el_parts {
                    self.check_stmt(el);
                }
            }
        }
    }

    fn check_while(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirWhile { condition, body } = &stmt.kind {
            let cond_ty = self.expr_type(condition);
            let bool_ty = self.boolean(condition.span.clone());
            if !self.types_match(&bool_ty, &cond_ty) {
                self.type_mismatch(&bool_ty, &cond_ty, condition.span.clone());
            }

            for st in body {
                self.check_stmt(st);
            }
        }
    }
}
