use std::collections::HashMap;

use crate::{
    hir::{HirStmt, HirStmtKind},
    mir::{
        builder::MIRBuilder,
        instructions::{MIRFn, MIRGlobal, MIRLinkage},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirVarDecl { .. } => self.build_var(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.build_fn(stmt),
            HirStmtKind::HirExpr(_) => self.build_expr_stmt(stmt),
            _ => todo!("Implement all the different statement handlers"),
        }
    }

    fn build_var(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirVarDecl {
            name,
            constant,
            exposed,
            init,
            ..
        } = &stmt.kind
        {
            let ty = self.get_type(&stmt.hir_id);

            match self.current_func {
                None => {
                    let global_id = self.alloc_global_id();

                    let linkage = |is_exposed| {
                        if is_exposed {
                            MIRLinkage::Public
                        } else {
                            MIRLinkage::Private
                        }
                    };

                    let mir_global = MIRGlobal {
                        global_id: global_id.clone(),
                        name: name.clone(),
                        is_const: *constant,
                        linkage: linkage(*exposed),
                        ty,
                        init: init.as_ref().map(|expr| self.expr_value(expr)),
                    };

                    self.module.globals.insert(global_id, mir_global);
                }
                Some(fn_id) => (),
            }
        }
    }

    fn build_fn(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef { name, body, .. } = &stmt.kind {
            let new_fn_id = self.alloc_fn_id();

            let entry_block = self.create_basic_block();
            let entry_block_id = entry_block.id.clone();

            let mir_fn = MIRFn {
                fn_id: new_fn_id,
                name: name.clone(),
                blocks: HashMap::new(),
                entry_block: entry_block_id,
            };

            self.module.functions.insert(new_fn_id, mir_fn);

            //  Save previous context
            let prev_fn = self.current_func;
            let prev_block = self.current_block_id;

            self.current_func = Some(new_fn_id);
            self.current_block_id = Some(entry_block_id);

            self.add_block(&entry_block);

            for body_stmt in body {
                self.build_stmt(body_stmt);
            }

            self.current_func = prev_fn;
            self.current_block_id = prev_block;
        }
    }

    fn build_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            let instruction = self.build_expr(inner);
            self.add_instruction(instruction);
        }
    }
}
