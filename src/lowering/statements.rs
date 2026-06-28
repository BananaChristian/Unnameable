use crate::{
    ast::{Stmt, StmtKind},
    hir::{HirParam, HirStmt, HirStmtKind, HirTypeNode},
    lowering::lowering::Lowering,
};

impl<'a> Lowering<'a> {
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match stmt.kind {
            StmtKind::VarDecl { .. } => self.lower_var(stmt),
            StmtKind::FunctionDef { .. } => self.lower_func_def(stmt),
            StmtKind::FunctionDecl{..} => self.lower_func_decl(stmt),
            _ => {
                self.report("Unknown statement".to_string(), Some(stmt.span.clone()));
                None
            }
        }
    }

    fn lower_var(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::VarDecl {
            qualifiers,
            type_annotation,
            name,
            init,
        } = &stmt.kind
        {
            // Extract name string from Expr::Identifier
            let name_str = self.extract_name_string(name)?;

            // Extract qualifiers into a booleans map
            let qualifier_map = self.map_qualifiers(qualifiers);

            // Lower the type annotation if present
            let ty = match type_annotation {
                Some(t) => Some(self.lower_type(t)?),
                None => None,
            };

            // Lower the init expression if present
            let init = match init {
                Some(e) => Some(self.lower_expr(e)?),
                None => None,
            };

            Some(HirStmt {
                kind: HirStmtKind::HirVarDecl {
                    name: name_str,
                    mutable: qualifier_map.mutable,
                    constant: qualifier_map.constant,
                    heap: qualifier_map.heap,
                    exposed: qualifier_map.expose,
                    ty,
                    init,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_func_def(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::FunctionDef {
            qualifiers,
            name,
            params,
            type_annotation,
            body,
        } = &stmt.kind
        {
            let map=self.map_qualifiers(qualifiers);

            // Extract and mangle name
            let name_str = self.extract_name_string(name)?;

            // Lower params
            let hir_params = params
                .iter()
                .map(|p| self.lower_param(p))
                .collect::<Option<Vec<_>>>()?;

            let return_type = match type_annotation {
                Some(t) => self.lower_type(t)?,
                None => HirTypeNode::unit(stmt.span.clone()),
            };

            let hir_body = self.lower_block(body)?;

            Some(HirStmt {
                kind: HirStmtKind::HirFunctionDef {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    type_params: self.current_type_params.clone(),
                    exposed: map.expose,
                    body: hir_body,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_func_decl(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::FunctionDecl {
            qualifiers,
            name,
            params,
            type_annotation,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);

            let name_str = self.extract_name_string(name)?;

            let hir_params = params
                .iter()
                .map(|p| self.lower_param(p))
                .collect::<Option<Vec<_>>>()?;

            let return_type = match type_annotation {
                Some(t) => self.lower_type(t)?,
                None => HirTypeNode::unit(stmt.span.clone()),
            };

            Some(HirStmt {
                kind: HirStmtKind::HirFunctionDecl {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    type_params: self.current_type_params.clone(),
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_param(&mut self, stmt: &Stmt) -> Option<HirParam> {
        if let StmtKind::ParamDecl {
            qualifiers,
            name,
            type_annotation,
            def,
        } = &stmt.kind
        {
            let name_str = self.extract_name_string(name)?;
            let qualifier_map = self.map_qualifiers(qualifiers);
            let ty = self.lower_type(type_annotation)?;
            let default = match def {
                Some(e) => Some(self.lower_expr(e)?),
                None => None,
            };
            Some(HirParam {
                name: name_str,
                ty,
                mutable: qualifier_map.mutable,
                default,
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_block(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::Block { content } = &stmt.kind {
            content
                .iter()
                .map(|s| self.lower_stmt(s))
                .collect::<Option<Vec<_>>>()
        } else {
            None
        }
    }
}
