use std::collections::HashMap;

use crate::{
    bc_builder::bytecode::{
        BytecodeFn, BytecodeModule, DollarMode, GlobalVar, RegisterMap, VMOpcode,
    },
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    mir::{
        BlockId, ConstantValue, MIRDollarMode, MIRFn, MIRInstruction, MIRModule, MIROps, MIRValue,
        Terminator, Vreg,
    },
};

#[derive(Debug, Clone)]
pub struct RootDollarEval {
    pub scope_fn: String,
    pub args: Vec<MIRValue>,
}

pub struct BytecodeBuilder<'a> {
    pub mir_module: &'a MIRModule,
    bytecode_module: BytecodeModule,
    diagnostics: SharedDiagnostics,
    corrupted: bool,
}

impl<'a> BytecodeBuilder<'a> {
    pub fn new(mir_module: &'a MIRModule, diagnostics: SharedDiagnostics) -> Self {
        BytecodeBuilder {
            mir_module,
            bytecode_module: BytecodeModule {
                functions: Vec::new(),
                fn_symbols: HashMap::new(),
                globals: Vec::new(),
                global_symbols: HashMap::new(),
            },
            diagnostics,
            corrupted: false,
        }
    }

    pub fn build(&mut self) -> BytecodeModule {
        self.translate_globals();

        // Sort functions by name
        let mut sorted_fns: Vec<_> = self.mir_module.functions.values().collect();
        sorted_fns.sort_by(|a, b| a.name.cmp(&b.name));

        for func in &sorted_fns {
            if func.dollar_mode != MIRDollarMode::None {
                let fn_idx = self.bytecode_module.functions.len() as u32;
                self.bytecode_module
                    .fn_symbols
                    .insert(func.name.clone(), fn_idx);

                // Push a placeholder BytecodeFn so indices match vector positions
                let mut placeholder = BytecodeFn::new();
                placeholder.name = func.name.clone();
                placeholder.mode = self.convert_dollar_mode(&func.dollar_mode);
                placeholder.param_count = func.params.len() as u16;
                self.bytecode_module.functions.push(placeholder);
            }
        }

        // Translate function bodies into pre-allocated slots
        for func in &sorted_fns {
            if func.dollar_mode != MIRDollarMode::None {
                let &fn_idx = self.bytecode_module.fn_symbols.get(&func.name).unwrap();

                // Translate function and write directly to its pre-allocated slot
                let bc_func = self.translate_fn_body(func);
                self.bytecode_module.functions[fn_idx as usize] = bc_func;
            }
        }

        // Discover root scopes and emit orchestrator
        let root_scopes = self.collect_root_dollar_scopes(&sorted_fns);
        self.emit_top_level_orchestrator(&root_scopes);

        self.bytecode_module.clone()
    }

    fn translate_globals(&mut self) {
        for (id, global) in &self.mir_module.globals {
            let global_id = id.0 as u32;
            let gvar = GlobalVar {
                id: global_id,
                name: global.name.clone(),
                size_in_bytes: 0,
                init_data: None,
            };
            self.bytecode_module.globals.push(gvar);
            self.bytecode_module
                .global_symbols
                .insert(global.name.clone(), global_id);
        }
    }

    fn collect_root_dollar_scopes(&self, sorted_fns: &[&MIRFn]) -> Vec<RootDollarEval> {
        let mut inner_scopes = std::collections::HashSet::new();
        let mut candidates: Vec<RootDollarEval> = Vec::new();

        // Collect all dollar scopes invoked inside any dollar scope
        for func in sorted_fns {
            if func.name.starts_with("$$scope") {
                for block in func.blocks.values() {
                    for inst in &block.instructions {
                        if let MIRInstruction::DollarEval { scope_fn, .. } = inst {
                            inner_scopes.insert(scope_fn.clone());
                        }
                    }
                }
            }
        }

        // Collect dollar scopes invoked outside dollar scopes
        for func in sorted_fns {
            if !func.name.starts_with("$$scope") {
                for block in func.blocks.values() {
                    for inst in &block.instructions {
                        if let MIRInstruction::DollarEval { scope_fn, args, .. } = inst {
                            if !candidates.iter().any(|r| &r.scope_fn == scope_fn) {
                                candidates.push(RootDollarEval {
                                    scope_fn: scope_fn.clone(),
                                    args: args.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }

        candidates.retain(|eval| !inner_scopes.contains(&eval.scope_fn));
        candidates
    }

    fn emit_top_level_orchestrator(&mut self, root_scopes: &[RootDollarEval]) {
        let mut top_func = BytecodeFn::new();
        top_func.name = "@$top_level".to_string();
        top_func.mode = DollarMode::Full;
        top_func.param_count = 0;

        let mut reg_map = RegisterMap::new();
        let mut instructions = Vec::new();

        for eval in root_scopes {
            if let Some(&fn_id) = self.bytecode_module.fn_symbols.get(&eval.scope_fn) {
                let mut vm_args = Vec::with_capacity(eval.args.len());
                for arg in &eval.args {
                    let reg = self.lower_mir_value(arg, &mut reg_map, &mut instructions);
                    vm_args.push(reg);
                }

                let dest_reg = reg_map.next_index;
                reg_map.next_index += 1;

                instructions.push(VMOpcode::DollarEval {
                    dest: Some(dest_reg),
                    fn_id,
                    args: vm_args,
                });
            } else {
                self.report_ice(format!(
                    "Root dollar scope '{}' not found in fn_symbols",
                    eval.scope_fn
                ));
            }
        }

        instructions.push(VMOpcode::Return { val: None });

        top_func.register_count = reg_map.total_registers();
        top_func.instructions = instructions;

        let top_idx = self.bytecode_module.functions.len() as u32;
        self.bytecode_module.functions.push(top_func);
        self.bytecode_module
            .fn_symbols
            .insert("@$top_level".to_string(), top_idx);
    }

    fn lower_const_val(
        &self,
        dest: u16,
        const_val: &ConstantValue,
        instructions: &mut Vec<VMOpcode>,
    ) {
        match const_val {
            ConstantValue::Bool(val) => instructions.push(VMOpcode::ConstBool { dest, val: *val }),
            ConstantValue::I8(v) => instructions.push(VMOpcode::ConstI8 { dest, val: *v }),
            ConstantValue::U8(v) => instructions.push(VMOpcode::ConstU8 { dest, val: *v }),
            ConstantValue::I16(v) => instructions.push(VMOpcode::ConstI16 { dest, val: *v }),
            ConstantValue::U16(v) => instructions.push(VMOpcode::ConstU16 { dest, val: *v }),
            ConstantValue::I32(v) => instructions.push(VMOpcode::ConstI32 { dest, val: *v }),
            ConstantValue::U32(v) => instructions.push(VMOpcode::ConstU32 { dest, val: *v }),
            ConstantValue::I64(v) => instructions.push(VMOpcode::ConstI64 { dest, val: *v }),
            ConstantValue::U64(v) => instructions.push(VMOpcode::ConstU64 { dest, val: *v }),
            ConstantValue::Int(v) => instructions.push(VMOpcode::ConstIsize { dest, val: *v }),
            ConstantValue::UInt(v) => instructions.push(VMOpcode::ConstUSize { dest, val: *v }),
            ConstantValue::I128(v) => instructions.push(VMOpcode::ConstI128 { dest, val: *v }),
            ConstantValue::U128(v) => instructions.push(VMOpcode::ConstU128 { dest, val: *v }),
            ConstantValue::F32(_) => todo!("float constants"),
            ConstantValue::F64(_) => todo!("float constants"),
        }
    }

    fn lower_mir_value(
        &self,
        val: &MIRValue,
        reg_map: &mut RegisterMap,
        instructions: &mut Vec<VMOpcode>,
    ) -> u16 {
        match val {
            MIRValue::Register { vreg, .. } => match vreg {
                Vreg::Numbered(n) => reg_map.get_or_insert(&format!("%{}", n)),
                Vreg::Named(name) => reg_map.get_or_insert(name),
            },
            MIRValue::Constant(const_val) => {
                let dest = reg_map.next_index;
                reg_map.next_index += 1;
                self.lower_const_val(dest, const_val, instructions);
                dest
            }
            MIRValue::Poison => panic!("Poison value in bytecode lowering"),
        }
    }

    fn lower_terminator(
        &self,
        term: &Terminator,
        reg_map: &mut RegisterMap,
        instructions: &mut Vec<VMOpcode>,
    ) {
        match term {
            Terminator::Return(val) => {
                let reg = val
                    .as_ref()
                    .map(|v| self.lower_mir_value(v, reg_map, instructions));
                instructions.push(VMOpcode::Return { val: reg });
            }
            Terminator::Goto(block_id) => {
                // placeholder, real offset resolved in patch pass
                instructions.push(VMOpcode::Jump {
                    target_pc: block_id.0,
                });
            }
            Terminator::Branch {
                cond,
                then,
                else_block,
            } => {
                let cond_reg = self.lower_mir_value(cond, reg_map, instructions);
                instructions.push(VMOpcode::BranchIf {
                    cond: cond_reg,
                    then_pc: then.0,       // placeholder
                    else_pc: else_block.0, // placeholder
                });
            }
            Terminator::Unreachable => {}
        }
    }

    fn translate_fn_body(&mut self, func: &MIRFn) -> BytecodeFn {
        let mut bc_func = BytecodeFn::new();
        bc_func.name = func.name.clone();
        bc_func.mode = self.convert_dollar_mode(&func.dollar_mode);
        bc_func.param_count = func.params.len() as u16;

        let mut reg_map = RegisterMap::new();
        let mut instructions: Vec<VMOpcode> = Vec::new();

        for param in &func.params {
            reg_map.get_or_insert(&param.name);
        }

        let mut block_order = vec![func.entry_block];
        let mut sorted_block_ids: Vec<_> = func.blocks.keys().copied().collect();
        sorted_block_ids.sort(); // Deterministic block order

        for id in sorted_block_ids {
            if id != func.entry_block {
                block_order.push(id);
            }
        }

        let mut block_offsets: HashMap<BlockId, usize> = HashMap::new();

        for block_id in &block_order {
            let block = &func.blocks[block_id];
            block_offsets.insert(*block_id, instructions.len());

            for inst in &block.instructions {
                self.lower_mir_instruction(
                    inst,
                    &mut reg_map,
                    &mut instructions,
                    &func.dollar_mode,
                );
            }

            self.lower_terminator(&block.terminator, &mut reg_map, &mut instructions);
        }

        Self::patch_jumps(&mut instructions, &block_offsets);

        bc_func.register_count = reg_map.total_registers();
        bc_func.instructions = instructions;

        bc_func
    }

    fn lower_mir_instruction(
        &mut self,
        inst: &MIRInstruction,
        reg_map: &mut RegisterMap,
        instructions: &mut Vec<VMOpcode>,
        fn_dollar_mode: &MIRDollarMode,
    ) {
        let current_mode = self.convert_dollar_mode(fn_dollar_mode);

        match inst {
            MIRInstruction::Alloca {
                dest, ty, align, ..
            } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let size = ty.align as u32; // size from type alignment for now
                instructions.push(VMOpcode::Alloca {
                    dest: dest_reg,
                    size,
                    align: *align as u32,
                });
            }

            MIRInstruction::Load { dest, ptr, .. } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let ptr_reg = self.lower_mir_value(ptr, reg_map, instructions);
                instructions.push(VMOpcode::Load {
                    dest: dest_reg,
                    ptr: ptr_reg,
                    mode: current_mode,
                });
            }

            MIRInstruction::Store { ptr, val, .. } => {
                let ptr_reg = self.lower_mir_value(ptr, reg_map, instructions);
                let val_reg = self.lower_mir_value(val, reg_map, instructions);
                instructions.push(VMOpcode::Store {
                    ptr: ptr_reg,
                    val: val_reg,
                    mode: current_mode,
                });
            }

            MIRInstruction::Assign { dest, src } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let src_reg = self.lower_mir_value(src, reg_map, instructions);
                instructions.push(VMOpcode::Move {
                    dest: dest_reg,
                    src: src_reg,
                });
            }

            MIRInstruction::BinaryOperation { dest, op, lhs, rhs } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let lhs_reg = self.lower_mir_value(lhs, reg_map, instructions);
                let rhs_reg = self.lower_mir_value(rhs, reg_map, instructions);
                let vm_op = match op {
                    MIROps::Add => VMOpcode::Add {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Sub => VMOpcode::Sub {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Mul => VMOpcode::Mul {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Sdiv | MIROps::Udiv => VMOpcode::Div {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Mod => VMOpcode::Mod {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Xor => VMOpcode::Xor {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::And => VMOpcode::And {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Or => VMOpcode::Or {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Shl => VMOpcode::Shl {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Shr => VMOpcode::Shr {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                    MIROps::Ashr => VMOpcode::AShr {
                        dest: dest_reg,
                        src1: lhs_reg,
                        src2: rhs_reg,
                    },
                };
                instructions.push(vm_op);
            }

            MIRInstruction::Compare { dest, op, lhs, rhs } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let lhs_reg = self.lower_mir_value(lhs, reg_map, instructions);
                let rhs_reg = self.lower_mir_value(rhs, reg_map, instructions);
                instructions.push(VMOpcode::Compare {
                    dest: dest_reg,
                    op: op.clone(),
                    src1: lhs_reg,
                    src2: rhs_reg,
                });
            }

            MIRInstruction::Call { dest, callee, args } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let fn_id = self.bytecode_module.fn_symbols[callee];
                let arg_regs = args
                    .iter()
                    .map(|a| self.lower_mir_value(a, reg_map, instructions))
                    .collect();
                instructions.push(VMOpcode::Call {
                    dest: Some(dest_reg),
                    fn_id,
                    args: arg_regs,
                });
            }

            MIRInstruction::DollarEval {
                dest,
                scope_fn,
                args,
            } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let Some(fn_id) = self.bytecode_module.fn_symbols.get(scope_fn) else {
                    self.report_ice(format!("Failed to get MIR function '{}'", scope_fn));
                };
                let arg_regs = args
                    .iter()
                    .map(|a| self.lower_mir_value(a, reg_map, instructions))
                    .collect();
                instructions.push(VMOpcode::DollarEval {
                    dest: Some(dest_reg),
                    fn_id: *fn_id,
                    args: arg_regs,
                });
            }

            MIRInstruction::GetElementPtr { dest, ptr, offset } => {
                todo!("GEP in bytecode")
            }

            MIRInstruction::Cast {
                dest, src, to_ty, ..
            } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let src_reg = self.lower_mir_value(src, reg_map, instructions);
                instructions.push(VMOpcode::Cast {
                    dest: dest_reg,
                    src: src_reg,
                    to_ty: to_ty.clone(),
                });
            }

            MIRInstruction::BitCast { dest, src, to_ty } => {
                let dest_reg = self.lower_mir_value(dest, reg_map, instructions);
                let src_reg = self.lower_mir_value(src, reg_map, instructions);
                instructions.push(VMOpcode::BitCast {
                    dest: dest_reg,
                    src: src_reg,
                    to_ty: to_ty.clone(),
                });
            }

            MIRInstruction::Phi { .. } => {
                // Phi nodes shouldn't exist after block linearization
                // If they do it's a bug in MIR construction
                panic!(
                    "Phi node encountered during bytecode lowering — MIR not properly linearized"
                );
            }
        }
    }

    fn patch_jumps(instructions: &mut Vec<VMOpcode>, block_offsets: &HashMap<BlockId, usize>) {
        for instr in instructions.iter_mut() {
            match instr {
                VMOpcode::Jump { target_pc } => {
                    let block_id = BlockId(*target_pc); // was stored as block_id.0
                    *target_pc = block_offsets[&block_id];
                }
                VMOpcode::BranchIf {
                    then_pc, else_pc, ..
                } => {
                    let then_block = BlockId(*then_pc);
                    let else_block = BlockId(*else_pc);
                    *then_pc = block_offsets[&then_block];
                    *else_pc = block_offsets[&else_block];
                }
                _ => {}
            }
        }
    }

    fn convert_dollar_mode(&self, mir_dollar: &MIRDollarMode) -> DollarMode {
        match mir_dollar {
            MIRDollarMode::None => DollarMode::None,
            MIRDollarMode::ReadOnly => DollarMode::Read,
            MIRDollarMode::Full => DollarMode::Full,
        }
    }

    pub fn report_ice(&mut self, message: String) -> ! {
        self.corrupted = true;
        let err = CompilerError::ice(message, Phase::BytecodeBuilder, None);
        self.diagnostics.borrow_mut().report_ice_and_panic(err);
    }
}
