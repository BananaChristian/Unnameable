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
            HirStmtKind::HirIf { .. } => self.build_if(stmt),
            HirStmtKind::HirWhile { .. } => self.build_while(stmt),
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
                    self.build_alloca(dest.clone(), ty.clone(), Some(stmt.span.clone()));
                    self.declare_var(name.clone(), dest.clone());
                    let val = self.expr_value(init);
                    self.build_store(dest, val, Some(stmt.span.clone()));
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

            self.add_block(&entry_block, Some(stmt.span.clone()));

            self.push_scope();
            for body_stmt in body {
                self.build_stmt(body_stmt);
            }
            self.pop_scope();

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
            self.set_terminator(terminator, Some(stmt.span.clone()));
        }
    }

    fn build_if(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &stmt.kind
        {
            self.build_expr(condition);
            let cond_val = self
                .last_value
                .as_ref()
                .cloned()
                .expect("Failed to get condition value");

            let then_block = self.create_basic_block();
            let else_block = self.create_basic_block();
            let merge_block = self.create_basic_block();

            self.set_terminator(
                Terminator::Branch {
                    cond: cond_val,
                    then: then_block.id,
                    else_block: else_block.id,
                },
                Some(stmt.span.clone()),
            );

            // build then block
            self.add_block(&then_block, Some(stmt.span.clone()));
            self.current_block_id = Some(then_block.id);
            self.push_scope();
            for st in body {
                self.build_stmt(st);
            }
            self.pop_scope();
            self.set_terminator(Terminator::Goto(merge_block.id), Some(stmt.span.clone()));

            // build else block
            self.add_block(&else_block, Some(stmt.span.clone()));
            self.current_block_id = Some(else_block.id);
            self.push_scope();
            if let Some(else_stmts) = else_body {
                for st in else_stmts {
                    self.build_stmt(st);
                }
            }
            self.pop_scope();
            self.set_terminator(Terminator::Goto(merge_block.id), Some(stmt.span.clone()));

            // switch to merge block, execution continues here
            self.add_block(&merge_block, Some(stmt.span.clone()));
            self.current_block_id = Some(merge_block.id);
        }
    }

    fn build_while(&mut self, _stmt: &HirStmt) {
        todo!("Add while")
    }

    fn build_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            self.build_expr(inner);
        }
    }
}
