use crate::{
    hir::{HirParam, HirStmt, HirStmtKind},
    semantics::{TypeInfo, type_checker::checker::TypeChecker},
};

impl<'a> TypeChecker<'a> {
    pub fn check_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirExpr(..) => self.check_expr_stmt(stmt),
            HirStmtKind::HirVariantDecl { .. }
            | HirStmtKind::HirStructDecl { .. }
            | HirStmtKind::HirEnumDecl { .. }
            | HirStmtKind::HirFunctionDecl { .. } => self.declare_custom_types(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func(stmt),
            HirStmtKind::HirVarDecl { .. } => self.check_var(stmt),
            HirStmtKind::HirReturn(_) => self.check_return(stmt),
            _ => (),
        }
    }

    fn check_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(expr) = &stmt.kind {
            self.check_expr(expr);
        }
    }

    fn check_return(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirReturn(val) = &stmt.kind {
            let ty = match val {
                Some(inner) => self.expr_type(inner),
                None => self.unit(stmt.span.clone()),
            };
            self.insert(stmt.hir_id, ty.clone());
        } else {
            let unknown_ty = self.unknown(stmt.span.clone());
            self.insert(stmt.hir_id, unknown_ty.clone());
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
            if !TypeInfo::types_match(&bool_ty, &cond_ty) {
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
            if !TypeInfo::types_match(&bool_ty, &cond_ty) {
                self.type_mismatch(&bool_ty, &cond_ty, condition.span.clone());
            }

            for st in body {
                self.check_stmt(st);
            }
        }
    }

    pub fn check_func_param_type(&mut self, param: &HirParam) {
        let param_ty = self.type_from_hir_type(&param.ty);
        if let Some(def) = &param.default {
            let def_ty = self.expr_type(def);
            if !TypeInfo::types_match(&param_ty, &def_ty) {
                self.type_mismatch(&param_ty, &def_ty, param.span.clone());
            }
        }
        self.insert(param.hir_id, param_ty);
    }

    fn check_func(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef { params, body, .. } = &stmt.kind {
            //Declare the function type
            self.declare_custom_types(stmt);
            for param in params {
                self.check_func_param_type(param);
            }

            for s in body {
                self.check_stmt(s);
            }
        }
    }

    fn check_var(&mut self, stmt: &HirStmt) {
        let mut _annotated_ty = self.unknown(stmt.span.clone());
        if let HirStmtKind::HirVarDecl { ty, init, .. } = &stmt.kind {
            if let Some(init) = init {
                let init_ty = self.expr_type(init);
                _annotated_ty = match ty {
                    Some(ty) => self.type_from_hir_type(ty),
                    None => init_ty.clone(),
                };

                if !TypeInfo::types_match(&_annotated_ty, &init_ty) {
                    self.type_mismatch(&_annotated_ty, &init_ty, stmt.span.clone());
                }
            } else {
                _annotated_ty = match ty {
                    Some(ty) => self.type_from_hir_type(ty),
                    None => {
                        self.report(
                            format!("Cannot infer variable type without an initializer"),
                            Some(stmt.span.clone()),
                        );
                        self.unknown(stmt.span.clone())
                    }
                };
            }
            self.insert(stmt.hir_id, _annotated_ty);
        }
    }
}
