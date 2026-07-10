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
        new_name: String,
    ) {
        match &mut stmt.kind {
            HirStmtKind::HirFunctionDef { .. } => {
                self.monomorphize_func_def(stmt, generic_params, concrete_args, new_name)
            }
            HirStmtKind::HirFunctionDecl { .. } => {
                self.monomorphize_func_decl(stmt, generic_params, concrete_args, new_name)
            }
            HirStmtKind::HirStructDecl { .. } => {
                self.monormophize_struct(stmt, generic_params, concrete_args, new_name)
            }
            HirStmtKind::HirIf { .. } => {
                self.monomorphize_if(stmt, generic_params, concrete_args, new_name)
            }
            HirStmtKind::HirWhile { .. } => {
                self.monomorphize_while(stmt, generic_params, concrete_args, new_name)
            }
            HirStmtKind::HirExpr(expr) => {
                self.monomorphize_expr(expr, generic_params, concrete_args)
            }
            HirStmtKind::HirVarDecl { .. } => {
                self.monomorphize_var_decl(stmt, generic_params, concrete_args);
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
                self.monormophize_stmt(body_stmt, generic_params, concrete_args, new_name.clone());
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
            self.monomorphize_expr(condition, generic_params, concrete_args);
            for st in body {
                self.monormophize_stmt(st, generic_params, concrete_args, new_name.clone());
            }

            if let Some(el) = else_body {
                for el_stmt in el {
                    self.monormophize_stmt(
                        el_stmt,
                        generic_params,
                        concrete_args,
                        new_name.clone(),
                    );
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
            self.monomorphize_expr(condition, generic_params, concrete_args);
            for st in body {
                self.monormophize_stmt(st, generic_params, concrete_args, new_name.clone());
            }
        }
    }

    fn monomorphize_var_decl(
        &mut self,
        stmt: &mut HirStmt,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
    ) {
        if let HirStmtKind::HirVarDecl { ty, init, .. } = &mut stmt.kind {
            if let Some(ty_n) = ty {
                self.monomorphize_type(ty_n)
            }

            if let Some(init_expr) = init {
                self.monomorphize_expr(init_expr, generic_params, concrete_args);
            }
        }
    }
}
