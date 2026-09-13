use std::collections::HashMap;

use crate::{
    hir::{HirStmt, HirStmtKind},
    mir::{
        MIRTy, MIRValue, MIRVariant,
        builder::MIRBuilder,
        instructions::{
            ArmInfo, MIRBody, MIRDollarMode, MIREnum, MIRGlobal, MIRLinkage, MIRParam,
            MIRStructDecl, MIRTykind, MIRVariantArm, Terminator,
        },
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirStructDecl { .. }
            | HirStmtKind::HirVariantDecl { .. }
            | HirStmtKind::HirEnumDecl { .. }
            | HirStmtKind::HirFunctionDecl { .. }
            | HirStmtKind::HirContractDecl { .. } => (),
            HirStmtKind::HirVarDecl { .. } => self.build_var(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.build_fn(stmt),
            HirStmtKind::HirReturn(_) => self.build_return(stmt),
            HirStmtKind::HirIf { .. } => self.build_if(stmt),
            HirStmtKind::HirWhile { .. } => self.build_while(stmt),
            HirStmtKind::HirExpr(_) => self.build_expr_stmt(stmt),
            _ => self.report_ice(
                format!(
                    "Encountered {:?}, no statement handler implemented",
                    stmt.kind
                ),
                Some(stmt.span.clone()),
            ),
        }
    }

    pub fn build_struct(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirStructDecl { name, fields, .. } = &stmt.kind {
            let struct_id = self.alloc_struct_id();
            self.struct_name_to_id
                .insert(name.clone(), struct_id.clone());
            let fields: Vec<(String, MIRTy)> = fields
                .iter()
                .map(|f| {
                    let name = f.name.clone();
                    let ty = self.get_type(&f.hir_id);
                    (name, ty)
                })
                .collect();

            let mir_struct = MIRStructDecl {
                struct_id,
                name: name.clone(),
                fields,
            };
            self.module.structs.insert(struct_id, mir_struct);
        }
    }

    pub fn build_enum(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirEnumDecl { name, members, .. } = &stmt.kind {
            let enum_id = self.alloc_enum_id();
            self.enum_name_to_id.insert(name.clone(), enum_id.clone());
            let underlying_ty = self.get_type(&stmt.hir_id);

            let mir_members: Vec<(String, isize)> =
                members.iter().map(|m| (m.name.clone(), m.value)).collect();

            let mir_enum = MIREnum {
                enum_id: enum_id.clone(),
                name: name.clone(),
                underlying: underlying_ty,
                members: mir_members,
            };
            self.enums.insert(enum_id, mir_enum);
        }
    }

    pub fn build_variant(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirVariantDecl { name, members, .. } = &stmt.kind {
            let variant_id = self.alloc_variant_id();
            self.variant_name_to_id.insert(name.clone(), variant_id);
            let total_arms = members.len();

            let discriminant_ty = if total_arms <= 256 {
                MIRTy {
                    kind: MIRTykind::U8,
                    size: 1,
                    align: 1,
                }
            } else {
                MIRTy {
                    kind: MIRTykind::U32,
                    size: 4,
                    align: 4,
                }
            };

            let variant_arms: Vec<MIRVariantArm> = members
                .iter()
                .enumerate()
                .map(|(index, arm_hir)| {
                    let payload_tys = arm_hir
                        .member_types
                        .iter()
                        .map(|field| self.get_type(&field.hir_id))
                        .collect();

                    MIRVariantArm {
                        name: arm_hir.name.clone(),
                        tag: index,
                        payload_tys,
                    }
                })
                .collect();

            let mir_variant = MIRVariant {
                name: name.clone(),
                discriminant_ty,
                arms: variant_arms,
            };

            let ty_info = self.get_type_info(&stmt.hir_id, Some(stmt.span.clone()));
            let struct_decl = self.convert_variant_to_struct(&mir_variant, &ty_info);

            let mut arms_for_struct = HashMap::new();

            for (tag_idx, arm_hir) in members.iter().enumerate() {
                let payload_tys = arm_hir
                    .member_types
                    .iter()
                    .map(|f| self.get_type(&f.hir_id))
                    .collect();

                arms_for_struct.insert(
                    arm_hir.name.clone(),
                    ArmInfo {
                        tag: tag_idx as u32,
                        payload_tys,
                    },
                );
            }

            self.arm_map
                .insert(struct_decl.struct_id.clone(), arms_for_struct);
            self.module
                .structs
                .insert(struct_decl.struct_id.clone(), struct_decl);
        }
    }

    fn build_var(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirVarDecl {
            name,
            constant,
            exposed,
            dollar_read,
            init,
            ..
        } = &stmt.kind
        {
            let ty = self.get_type(&stmt.hir_id);
            let dollar_mode = if *dollar_read {
                MIRDollarMode::ReadOnly
            } else {
                MIRDollarMode::None
            };

            match self.current_func {
                None => {
                    let global_id = self.alloc_global_id();
                    self.global_name_to_id
                        .insert(name.clone(), global_id.clone());
                    self.declare_var(name.clone(), MIRValue::Global(global_id));
                    self.declare_var_dollar(name.clone(), dollar_mode);

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
                        dollar_mode,
                        linkage: linkage(*exposed),
                        ty,
                        init: self.expr_value(init),
                    };

                    self.module.globals.insert(global_id, mir_global);
                }
                Some(_) => {
                    if *constant {
                        let const_val = self.expr_value(init);
                        if !matches!(const_val, MIRValue::Constant(_)) {
                            self.report(
                                format!(
                                    "'const {}' requires a compile-time-constant initializer",
                                    name
                                ),
                                Some(stmt.span.clone()),
                            );
                        }
                        self.declare_var(name.clone(), const_val);
                        self.declare_var_dollar(name.clone(), dollar_mode);

                        return;
                    }
                    if ty.kind == MIRTykind::Unit {
                        self.expr_value(init);
                        self.declare_var(name.clone(), MIRValue::Poison);
                        return;
                    }
                    let dest = self.new_register(self.ptr_type(), Some(name));
                    self.build_alloca(dest.clone(), ty.clone(), Some(stmt.span.clone()));
                    self.declare_var(name.clone(), dest.clone());
                    self.declare_var_dollar(name.clone(), dollar_mode);

                    if !self.build_into(init, dest.clone()) {
                        let val = self.expr_value(init);
                        let init_ty = self.get_type(&init.hir_id);
                        self.build_store(dest, val, init_ty, Some(stmt.span.clone()));
                    }
                }
            }
        }
    }

    pub fn build_fn_decl(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDecl {
            name,
            params,
            return_type,
            exposed,
            ..
        } = &stmt.kind
        {
            let linkage = match *exposed {
                true => MIRLinkage::Public,
                false => MIRLinkage::Private,
            };

            let mir_params: Vec<MIRParam> = params
                .iter()
                .map(|p| MIRParam {
                    name: p.name.clone(),
                    dollar_mode: match p.dollar_read {
                        true => MIRDollarMode::ReadOnly,
                        false => MIRDollarMode::None, //For now
                    },
                    ty: self.get_type(&p.hir_id),
                })
                .collect();

            let ret_ty = self.get_type(&return_type.hir_id);
            let dollar_mode = self.current_dollar_mode; //To be watched carefully
            self.get_or_create_func(name, &mir_params, &ret_ty, dollar_mode, linkage, None);
        }
    }

    fn build_fn(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef {
            name,
            params,
            body,
            dollar_read,
            exposed,
            return_type,
            ..
        } = &stmt.kind
        {
            let span = Some(stmt.span.clone());

            let entry_block = self.create_basic_block();
            let entry_block_id = entry_block.id.clone();

            let linkage = match *exposed {
                true => MIRLinkage::Public,
                false => MIRLinkage::Private,
            };

            let dollar_mode = match *dollar_read {
                true => MIRDollarMode::ReadOnly,
                false => self.current_dollar_mode,
            };

            //Mangle the name if we are inside a dollar scope
            let mangled_name = if dollar_mode == MIRDollarMode::Full {
                match &self.current_dollar_name {
                    Some(d_name) => format!("{}_{}", d_name, name),
                    None => name.clone(),
                }
            } else {
                name.clone()
            };

            let mir_params: Vec<MIRParam> = params
                .iter()
                .map(|p| MIRParam {
                    name: p.name.clone(),
                    dollar_mode: match p.dollar_read {
                        true => MIRDollarMode::ReadOnly,
                        false => MIRDollarMode::None, //For now
                    },
                    ty: self.get_type(&p.hir_id),
                })
                .collect();

            let ret_ty = self.get_type(&return_type.hir_id);

            let mir_body = MIRBody {
                blocks: HashMap::new(),
                entry_block: entry_block_id,
            };

            let fn_id = self.get_or_create_func(
                mangled_name.as_str(),
                &mir_params,
                &ret_ty,
                dollar_mode,
                linkage,
                Some(mir_body),
            );

            //  Save previous context
            let prev_fn = self.current_func;
            let prev_block = self.current_block_id;

            self.current_func = Some(fn_id);
            self.current_block_id = Some(entry_block_id);

            self.add_block(&entry_block, span.clone());

            self.push_scope();
            for param in &mir_params {
                if param.name == "self" {
                    continue;
                }

                let slot =
                    self.new_register(self.ptr_type(), Some(&format!("{}.addr", param.name)));
                self.build_alloca(slot.clone(), param.ty.clone(), span.clone());

                let param_val = self.new_register(param.ty.clone(), Some(param.name.as_str()));

                self.build_store(slot.clone(), param_val, param.ty.clone(), span.clone());
                self.declare_var(param.name.clone(), slot);
            }

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
            let span = Some(stmt.span.clone());
            self.build_expr(condition);
            let Some(cond_val) = self.last_value.as_ref().cloned() else {
                self.report_ice("Failed to get last MIRValue".to_string(), span.clone());
            };

            let then_block = self.create_basic_block();
            let else_block = self.create_basic_block();
            let merge_block = self.create_basic_block();

            self.set_terminator(
                Terminator::Branch {
                    cond: cond_val,
                    then: then_block.id,
                    else_block: else_block.id,
                },
                span.clone(),
            );

            // build then block
            self.add_block(&then_block, span.clone());
            self.current_block_id = Some(then_block.id);
            self.push_scope();
            for st in body {
                self.build_stmt(st);
            }
            self.pop_scope();
            self.set_terminator(Terminator::Goto(merge_block.id), span.clone());

            // build else block
            self.add_block(&else_block, span.clone());
            self.current_block_id = Some(else_block.id);
            self.push_scope();
            if let Some(else_stmts) = else_body {
                for st in else_stmts {
                    self.build_stmt(st);
                }
            }
            self.pop_scope();
            self.set_terminator(Terminator::Goto(merge_block.id), span.clone());

            // switch to merge block, execution continues here
            self.add_block(&merge_block, span);
            self.current_block_id = Some(merge_block.id);
        }
    }

    fn build_while(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirWhile { condition, body } = &stmt.kind {
            let span = Some(stmt.span.clone());
            let cond_block = self.create_basic_block();
            let body_block = self.create_basic_block();
            let exit_block = self.create_basic_block();

            // current block jumps to condition
            self.set_terminator(Terminator::Goto(cond_block.id), span.clone());

            self.add_block(&cond_block, span.clone());
            self.current_block_id = Some(cond_block.id);
            self.build_expr(condition);

            let Some(cond_val) = self.last_value.as_ref().cloned() else {
                self.report_ice("Failed to get last MIRValue".to_string(), span.clone());
            };
            self.set_terminator(
                Terminator::Branch {
                    cond: cond_val,
                    then: body_block.id,
                    else_block: exit_block.id,
                },
                span.clone(),
            );

            //Add the body block
            self.add_block(&body_block, span.clone());
            self.current_block_id = Some(body_block.id);
            self.push_scope();
            for s in body {
                self.build_stmt(s);
            }
            self.pop_scope();
            self.set_terminator(Terminator::Goto(cond_block.id), span.clone());

            //Add the exit block
            self.add_block(&exit_block, span);
            self.current_block_id = Some(exit_block.id);
        }
    }

    fn build_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            self.build_expr(inner);
        }
    }
}
