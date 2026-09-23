use crate::{
    hir::{HirParam, HirStmt, HirStmtKind},
    semantics::{TypeInfo, type_checker::checker::TypeChecker},
};

impl<'a> TypeChecker<'a> {
    pub fn check_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirExpr(..) | HirStmtKind::HirTailExpr(..) => self.check_expr_stmt(stmt),
            HirStmtKind::HirVariantDecl { .. }
            | HirStmtKind::HirStructDecl { .. }
            | HirStmtKind::HirEnumDecl { .. }
            | HirStmtKind::HirFunctionDecl { .. }
            | HirStmtKind::HirAlias { .. } => self.declare_custom_types(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func(stmt),
            HirStmtKind::HirVarDecl { .. } => self.check_var(stmt),
            HirStmtKind::HirReturn(_) => self.check_return(stmt),
            HirStmtKind::HirContractDecl { functions, .. } => {
                for f in functions {
                    self.declare_custom_types(f);
                }
            }
            _ => (),
        }
    }

    fn check_expr_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirExpr(expr) | HirStmtKind::HirTailExpr(expr) => {
                self.check_expr(expr);
            }
            _ => {}
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
            inferred_return: inferred_return_flag,
            body,
            ..
        } = &stmt.kind
        {
            //Declare the function type (this also validates and types the params)
            self.declare_custom_types(stmt);

            let function_return_ty = self.ctxt.types.types.get(&return_type.hir_id).cloned();

            let mut inferred_return = None;

            let saved_active_generic_params = std::mem::take(&mut self.active_generic_params);
            self.active_generic_params = generic_type_params
                .iter()
                .map(|p| self.get_ty_node_name(p))
                .collect();

            for s in body {
                // An unsuffixed numeric literal returned from a function takes
                // the function's declared return type instead of the default.
                match (&function_return_ty, &s.kind) {
                    (Some(ret_ty), HirStmtKind::HirReturn(Some(expr)))
                    | (Some(ret_ty), HirStmtKind::HirTailExpr(expr)) => {
                        if *inferred_return_flag {
                            // Tail-return inference: don't force the tail to
                            // the baked Unit — record the tail's own type and
                            // write it back after checking the body.
                            self.check_stmt(s);
                            inferred_return = Some(self.expr_type(&expr));
                            continue;
                        }
                        self.coerce_ty(ret_ty, expr);
                    }
                    _ => {}
                }
                self.check_stmt(s);
            }

            if let Some(inferred_ty) = &inferred_return {
                let mut ty_info = self.ctxt.types.types.get(&return_type.hir_id).cloned();
                if let Some(ref mut slot) = ty_info {
                    slot.kind = inferred_ty.kind.clone();
                }
                if let Some(ref mut slot) = ty_info.clone() {
                    self.declare_custom_types(stmt);
                    if let Some(ref mut slot) = ty_info {
                        slot.type_id = inferred_ty.type_id.clone();
                        slot.layout = inferred_ty.layout.clone();
                        slot.name = inferred_ty.name.clone();
                    }
                    self.ctxt
                        .types
                        .types
                        .insert(return_type.hir_id.clone(), slot.clone());
                }
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
