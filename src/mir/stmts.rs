use std::collections::HashMap;

use crate::{
    hir::{HirStmt, HirStmtKind},
    mir::{builder::MIRBuilder, instructions::MIRFn},
};

impl<'a> MIRBuilder<'a> {
    pub fn build_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirFunctionDef { .. } => self.build_fn(stmt),
            HirStmtKind::HirExpr(_) => self.build_expr_stmt(stmt),
            _ => todo!("Implement all the different statement handlers"),
        }
    }

    pub fn build_fn(&mut self, stmt: &HirStmt) {
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
