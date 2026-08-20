use crate::{
    hir::{HirStmt, HirStmtKind, HirTypeNode},
    monomorph::monomorph::Monomorphizer,
    semantics::TypeInfo,
};

impl<'a> Monomorphizer<'a> {
    pub fn monormophize_stmt(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        explicit_new_name: Option<String>,
    ) {
        let target_name = match explicit_new_name {
            Some(name) => name,
            None => {
                if self.get_decl(&stmt.hir_id).is_some() {
                    self.extract_stmt_name(&stmt.hir_id)
                } else {
                    match &stmt.kind {
                        HirStmtKind::HirFunctionDef { name, .. } => name.clone(),
                        HirStmtKind::HirFunctionDecl { name, .. } => name.clone(),
                        HirStmtKind::HirStructDecl { name, .. } => name.clone(),
                        HirStmtKind::HirVariantDecl { name, .. } => name.clone(),
                        _ => String::new(),
                    }
                }
            }
        };

        match &mut stmt.kind {
            HirStmtKind::HirFunctionDef { .. } => {
                self.monomorphize_func_def(stmt, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirFunctionDecl { .. } => {
                self.monomorphize_func_decl(stmt, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirStructDecl { .. } => {
                self.monormophize_struct(stmt, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirIf { .. } => {
                self.monomorphize_if(stmt, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirWhile { .. } => {
                self.monomorphize_while(stmt, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirExpr(expr) => {
                self.monomorphize_expr(expr, generic_params, concrete_args, target_name)
            }
            HirStmtKind::HirVarDecl { .. } => {
                self.monomorphize_var_decl(stmt, generic_params, concrete_args, target_name);
            }
            _ => (),
        }
    }
    fn monomorphize_func_def(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirFunctionDef {
            name,
            params,
            return_type,
            generic_type_params,
            body,
            ..
        } = &mut stmt.kind
        {
            *name = new_name.clone();
            generic_type_params.clear();
            for param in params {
                self.substitute_type(&mut param.ty, generic_params, concrete_args);
            }

            self.substitute_type(return_type, generic_params, concrete_args);

            for body_stmt in body {
                self.monormophize_stmt(body_stmt, generic_params, concrete_args, None);
            }
        }
    }

    fn monomorphize_func_decl(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirFunctionDecl {
            name,
            params,
            return_type,
            generic_type_params,
            ..
        } = &mut stmt.kind
        {
            *name = new_name.clone();
            generic_type_params.clear();
            for param in params {
                self.substitute_type(&mut param.ty, generic_params, concrete_args);
            }

            self.substitute_type(return_type, generic_params, concrete_args);
        }
    }

    fn monormophize_struct(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirStructDecl {
            name,
            generic_type_params,
            fields,
            ..
        } = &mut stmt.kind
        {
            *name = new_name;
            generic_type_params.clear();
            for field in fields {
                self.substitute_type(&mut field.ty, generic_params, concrete_args);
            }
        }
    }

    fn monomorphize_if(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &mut stmt.kind
        {
            self.monomorphize_expr(condition, generic_params, concrete_args, new_name.clone());
            for st in body {
                self.monormophize_stmt(st, generic_params, concrete_args, None);
            }

            if let Some(el) = else_body {
                for el_stmt in el {
                    self.monormophize_stmt(el_stmt, generic_params, concrete_args, None);
                }
            }
        }
    }

    fn monomorphize_while(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirWhile { condition, body } = &mut stmt.kind {
            self.monomorphize_expr(condition, generic_params, concrete_args, new_name.clone());

            for st in body {
                self.monormophize_stmt(st, generic_params, concrete_args, None);
            }
        }
    }

    fn monomorphize_var_decl(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        if let HirStmtKind::HirVarDecl { ty, init, .. } = &mut stmt.kind {
            if let Some(ty_n) = ty {
                self.monomorphize_type(ty_n)
            }

            self.monomorphize_expr(init, generic_params, concrete_args, new_name);
        }
    }
}
