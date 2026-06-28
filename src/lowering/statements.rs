use crate::{
    ast::{Expr, ExprKind, Stmt, StmtKind},
    hir::{HirStmt, HirStmtKind, QualifierMap},
    lowering::lowering::Lowering,
};

impl<'a> Lowering<'a> {
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match stmt.kind {
            StmtKind::VarDecl { .. } => self.lower_var(stmt),
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
            let qualifier_map= self.map_qualifiers(qualifiers);

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
}
