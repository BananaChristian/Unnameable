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
            | HirStmtKind::HirFunctionDecl { .. }
            | HirStmtKind::HirAlias { .. } => self.declare_custom_types(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func(stmt),
            HirStmtKind::HirVarDecl { .. } => self.check_var(stmt),
            HirStmtKind::HirReturn(_) => self.check_return(stmt),
            // Contract requirement functions carry signatures the contract
            // verifier must compare against impls; record them in the type
            // table under their own hir_id (the shape declared_custom_types
            // produces for a FunctionDecl).
            HirStmtKind::HirContractDecl { functions, .. } => {
                for f in functions {
                    self.declare_custom_types(f);
                }
            }
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
            self.expr_type(def);
            self.coerce_ty(&param_ty, def);
            let def_ty = self.expr_type(def);
            if !TypeInfo::types_match(&param_ty, &def_ty) {
                self.type_mismatch(&param_ty, &def_ty, param.span.clone());
            }
        }
        self.insert(param.hir_id, param_ty);
    }

    fn check_func(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef {
            generic_type_params,
            return_type,
            body,
            ..
        } = &stmt.kind
        {
            //Declare the function type (this also validates and types the params)
            self.declare_custom_types(stmt);

            // Reuse the already-resolved return type (same identity the CF
            // checker reads) instead of re-resolving, so an unsuffixed numeric
            // literal can adopt it without allocating new type identity.
            let function_return_ty = self.ctxt.types.types.get(&return_type.hir_id).cloned();

            // Keep the function's generic params in scope while checking the
            // body: parameter/return/usages of `T` inside must resolve to the
            // generic param (matching the param/return types declared above),
            // otherwise `T` falls through to `unknown` and every use is a type
            // mismatch. Restore whatever was active before this function.
            let saved_active_generic_params = std::mem::take(&mut self.active_generic_params);
            self.active_generic_params = generic_type_params
                .iter()
                .map(|p| self.get_ty_node_name(p))
                .collect();

            for s in body {
                // An unsuffixed numeric literal returned from a function takes
                // the function's declared return type instead of the default.
                if let (Some(ret_ty), HirStmtKind::HirReturn(Some(expr))) = (&function_return_ty, &s.kind) {
                    self.coerce_ty(ret_ty, expr);
                }
                self.check_stmt(s);
            }

            self.active_generic_params = saved_active_generic_params;
        }
    }

    fn check_var(&mut self, stmt: &HirStmt) {
        let mut _annotated_ty = self.unknown(stmt.span.clone());
        if let HirStmtKind::HirVarDecl { ty, init, .. } = &stmt.kind {
            let init_ty = self.expr_type(init);
            _annotated_ty = match ty {
                Some(ty) => self.type_from_hir_type(ty),
                None => init_ty.clone(),
            };
            self.coerce_ty(&_annotated_ty, init);

            let coerced_init_ty = self.expr_type(init);
            if !TypeInfo::types_match(&_annotated_ty, &coerced_init_ty) {
                self.type_mismatch(&_annotated_ty, &coerced_init_ty, stmt.span.clone());
            }

            self.insert(stmt.hir_id, _annotated_ty);
        }
    }
}
