use std::collections::HashMap;

use crate::{
    hir::{HirStmt, HirStmtKind},
    mir::{
        builder::MIRBuilder,
        instructions::{MIRFn, MIRGlobal, MIRLinkage, MIRTy, MIRTykind, Terminator},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirVarDecl { .. } => self.build_var(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.build_fn(stmt),
            HirStmtKind::HirReturn(_) => self.build_return(stmt),
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
                        init: self.expr_value(init),
                    };

                    self.module.globals.insert(global_id, mir_global);
                }
                Some(_) => {
                    let dest = self.new_register(MIRTy {
                        kind: MIRTykind::Ptr,
                        align: 8,
                    });
                    let alloca = self.build_alloca(dest.clone(), ty.clone());
                    self.add_instruction(alloca);
                    let val = self.expr_value(init);

                    let store = self.build_store(dest, val);
                    self.add_instruction(store);
                }
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

    fn build_return(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirReturn(inner) = &stmt.kind {
            let ret_val = match inner {
                Some(expr) => Some(self.expr_value(expr)),
                None => None,
            };
            let terminator = Terminator::Return(ret_val);
            self.set_terminator(terminator);
        }
    }

    fn build_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            let instruction = self.build_expr(inner);
            self.add_instruction(instruction);
        }
    }
}
