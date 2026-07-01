use crate::{
    hir::{HirStmt, HirStmtKind, HirType},
    semantics::{resolver::Resolver, semantics::NameTable},
};

impl<'a> Resolver<'a> {
    pub fn resolve_stmt(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        match &stmt.kind {
            HirStmtKind::HirVarDecl { .. } => self.resolve_var(stmt, table),
            HirStmtKind::HirFunctionDecl { .. } => self.resolve_func_decl(stmt, table),
            HirStmtKind::HirFunctionDef { .. } => self.resolve_func_def(stmt, table),
            HirStmtKind::HirStructDecl { .. } => self.resolve_struct(stmt, table),
            HirStmtKind::HirVariantDecl { .. } => self.resolve_variant(stmt, table),
            _ => (),
        }
    }

    fn resolve_var(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        if let HirStmtKind::HirVarDecl { name, init, .. } = &stmt.kind {
            //First resolve  the value
            if let Some(val) = init {
                self.resolve_expr(val, table);
            }

            //Now resolve the declaration normally
            self.declare(name.clone(), stmt.hir_id, stmt.span.clone());
        }
    }

    fn resolve_func_decl(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        if let HirStmtKind::HirFunctionDecl {
            name,
            params,
            return_type,
            generic_type_params,
            ..
        } = &stmt.kind
        {
            self.declare(name.clone(), stmt.hir_id, stmt.span.clone());
            self.push_scope();
            for gen_ty_param in generic_type_params {
                if let HirType::CustomType(ty_name) = &gen_ty_param.kind {
                    self.declare(
                        ty_name.clone(),
                        gen_ty_param.hir_id,
                        gen_ty_param.span.clone(),
                    );
                }
            }
            for param in params {
                self.resolve_type(&param.ty, table);
                self.declare(param.name.clone(), param.hir_id, param.span.clone());
                if let Some(def) = &param.default {
                    self.resolve_expr(def, table);
                }
            }
            self.resolve_type(return_type, table);
            self.pop_scope();
        }
    }

    fn resolve_func_def(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        if let HirStmtKind::HirFunctionDef {
            name,
            params,
            return_type,
            body,
            generic_type_params,
            ..
        } = &stmt.kind
        {
            self.declare(name.clone(), stmt.hir_id, stmt.span.clone());
            self.push_scope();
            for gen_ty_param in generic_type_params {
                if let HirType::CustomType(na) = &gen_ty_param.kind {
                    self.declare(na.clone(), gen_ty_param.hir_id, gen_ty_param.span.clone());
                }
            }
            for param in params {
                self.resolve_type(&param.ty, table);
                self.declare(param.name.clone(), param.hir_id, param.span.clone());
                if let Some(def) = &param.default {
                    self.resolve_expr(def, table);
                }
            }

            self.resolve_type(return_type, table);
            for st in body {
                self.resolve_stmt(st, table);
            }
            self.pop_scope();
        }
    }

    fn resolve_struct(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        if let HirStmtKind::HirStructDecl {
            name,
            contracts,
            generic_type_params,
            fields,
            ..
        } = &stmt.kind
        {
            self.declare(name.clone(), stmt.hir_id, stmt.span.clone());
            self.push_scope();
            for gen_ty_param in generic_type_params {
                if let HirType::CustomType(na) = &gen_ty_param.kind {
                    self.declare(na.clone(), gen_ty_param.hir_id, gen_ty_param.span.clone());
                }
            }

            for contract in contracts {
                self.resolve_type(contract, table);
            }

            for field in fields {
                self.resolve_type(&field.ty, table);
                if let Some(def) = &field.default {
                    self.resolve_expr(def, table);
                }
            }
            self.pop_scope();
        }
    }

    fn resolve_variant(&mut self, stmt: &HirStmt, table: &mut NameTable) {
        if let HirStmtKind::HirVariantDecl {
            name,
            contracts,
            members,
            generic_type_params,
            ..
        } = &stmt.kind
        {
            self.declare(name.clone(), stmt.hir_id, stmt.span.clone());
            self.push_scope();
            for gen_ty_param in generic_type_params {
                if let HirType::CustomType(na) = &gen_ty_param.kind {
                    self.declare(na.clone(), gen_ty_param.hir_id, gen_ty_param.span.clone());
                }
            }

            for contract in contracts {
                self.resolve_type(contract, table);
            }

            for field in members {
                self.declare(field.name.clone(), field.hir_id, field.span.clone());
                for ty in &field.member_types {
                    self.resolve_type(&ty, table);
                }
            }

            self.pop_scope();
        }
    }
}
