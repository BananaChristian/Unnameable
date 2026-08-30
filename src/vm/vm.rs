use std::collections::HashMap;

use crate::{
    bc_builder::{BytecodeModule, VMOpcode},
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    impl_cmp_op, impl_int_op, impl_numeric_op,
    mir::{CmpOp, MIRTykind},
    vm::{
        Allocation,
        structures::{AllocId, EvalResultTable, MemoryKind, VMFrame, VMMemory, VMValue},
    },
};

pub struct VM<'a> {
    pub module: &'a BytecodeModule,
    pub eval_table: EvalResultTable,
    pub memory: VMMemory,
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl<'a> VM<'a> {
    pub fn new(module: &'a BytecodeModule, diagnostics: SharedDiagnostics) -> Self {
        let mut vm = VM {
            module,
            eval_table: EvalResultTable {
                results: HashMap::new(),
            },
            memory: VMMemory {
                allocations: HashMap::new(),
                next_alloc: 1,
            },
            diagnostics,
            corrupted: false,
        };
        vm.init_globals();
        vm
    }

    fn init_globals(&mut self) {
        for global in &self.module.globals {
            let kind = match global.is_const {
                true => MemoryKind::ROData,
                false => MemoryKind::Data,
            };

            let alloc_id = AllocId {
                kind,
                id: global.id,
            };

            let data = vec![0u8; global.size_in_bytes as usize]; // zero-init the backing bytes
            self.memory.allocations.insert(
                alloc_id.clone(),
                Allocation {
                    data,
                    relocations: HashMap::new(),
                },
            );

            if let Some(init_val) = &global.init_data {
                self.write_typed(&alloc_id, 0, init_val, &global.ty);
            }
        }
    }

    pub fn execute(&mut self) -> EvalResultTable {
        self.execute_fn("@$top_level", vec![]);
        self.eval_table.clone()
    }

    fn execute_fn(&mut self, fn_name: &str, args: Vec<VMValue>) -> VMValue {
        let Some(func) = self
            .module
            .functions
            .iter()
            .find(|f| f.name == fn_name)
            .clone()
        else {
            self.report_ice(format!("Cannot find function '{}' ", fn_name));
            return VMValue::Poison;
        };

        //Create a new frame
        let mut frame = VMFrame {
            fn_name: fn_name.to_string(),
            mode: func.mode.clone(),
            ip: 0,
            registers: vec![None; func.register_count as usize],
        };

        //Load the arguments into the registers
        for (i, arg) in args.into_iter().enumerate() {
            frame.registers[i] = Some(arg)
        }

        self.run_frame(&mut frame, &func.instructions)
    }

    fn run_frame(&mut self, frame: &mut VMFrame, instructions: &[VMOpcode]) -> VMValue {
        loop {
            if frame.ip >= instructions.len() {
                return VMValue::Unit;
            }

            let instr = instructions[frame.ip].clone();
            frame.ip += 1;

            match instr {
                VMOpcode::ConstUndef { dest } => {
                    self.write_reg(frame, dest, VMValue::Poison);
                }
                VMOpcode::ConstI8 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::I8(val));
                }
                VMOpcode::ConstU8 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::U8(val));
                }
                VMOpcode::ConstI16 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::I16(val));
                }
                VMOpcode::ConstU16 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::U16(val));
                }
                VMOpcode::ConstI32 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::I32(val));
                }
                VMOpcode::ConstU32 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::U32(val));
                }
                VMOpcode::ConstI64 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::I64(val));
                }
                VMOpcode::ConstU64 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::U64(val));
                }
                VMOpcode::ConstIsize { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Int(val));
                }
                VMOpcode::ConstUSize { dest, val } => {
                    self.write_reg(frame, dest, VMValue::UInt(val));
                }
                VMOpcode::ConstI128 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::I128(val));
                }
                VMOpcode::ConstU128 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::U128(val));
                }
                VMOpcode::ConstChar8 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Char8(val));
                }
                VMOpcode::ConstChar16 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Char16(val));
                }
                VMOpcode::ConstChar32 { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Char32(val));
                }
                VMOpcode::ConstBool { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Bool(val));
                }
                VMOpcode::ConstArray { dest, elements } => {
                    let elems = elements.iter().map(|&r| self.read_reg(r, frame)).collect();
                    self.write_reg(frame, dest, VMValue::Array(elems));
                }
                VMOpcode::ConstTuple { dest, fields } => {
                    let vals: Vec<VMValue> =
                        fields.iter().map(|r| self.read_reg(*r, frame)).collect();
                    self.write_reg(frame, dest, VMValue::Tuple(vals));
                }
                VMOpcode::ConstPtr {
                    dest,
                    alloc_id,
                    addr,
                } => {
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, addr));
                }
                VMOpcode::ConstStruct {
                    dest,
                    name,
                    fields,
                    struct_id,
                } => {
                    let field_values = fields.iter().map(|&r| self.read_reg(r, frame)).collect();
                    self.write_reg(
                        frame,
                        dest,
                        VMValue::Struct {
                            struct_id,
                            name: name.clone(),
                            fields: field_values,
                        },
                    );
                }

                VMOpcode::LoadGlobal {
                    dest,
                    global_id,
                    is_const,
                } => {
                    let kind = if is_const {
                        MemoryKind::ROData
                    } else {
                        MemoryKind::Data
                    };
                    let alloc_id = AllocId {
                        kind,
                        id: global_id,
                    };

                    // Load a base pointer referencing the global's allocation slot
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, 0));
                }
                VMOpcode::LoadFunc { dest, fn_id } => {
                    let kind = MemoryKind::Code;
                    let alloc_id = AllocId { kind, id: fn_id };
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, 0));
                }
                VMOpcode::Jump { target_pc } => {
                    frame.ip = target_pc;
                }
                VMOpcode::Move { dest, src } => {
                    let val = self.read_reg(src, frame);
                    self.write_reg(frame, dest, val);
                }
                VMOpcode::Alloca { dest, size, .. } => {
                    let alloc_id = self.memory.allocate_stack(size as usize);
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, 0));
                }
                VMOpcode::Load { dest, ptr, ty, .. } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    if let VMValue::Ptr(alloc_id, offset) = ptr_val {
                        let val = self.read_typed(&alloc_id, offset, &ty);
                        self.write_reg(frame, dest, val);
                    } else {
                        self.report_ice("Load from non pointer".to_string());
                    }
                }
                VMOpcode::Store { ptr, val, ty, .. } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    let src_val = self.read_reg(val, frame);
                    if let VMValue::Ptr(alloc_id, offset) = ptr_val {
                        self.write_typed(&alloc_id, offset, &src_val, &ty);
                    } else {
                        self.report_ice("Store to a non pointer".to_string());
                    }
                }
                VMOpcode::AddrOf { dest, src } => {
                    let ptr_val = self.read_reg(src, frame);
                    debug_assert!(matches!(ptr_val, VMValue::Ptr(_, _)));
                    self.write_reg(frame, dest, ptr_val);
                }
                VMOpcode::Return { val } => {
                    return match val {
                        Some(v) => self.read_reg(v, frame),
                        None => VMValue::Unit,
                    };
                }
                VMOpcode::CallDirect { dest, fn_id, args } => {
                    let fn_name = self.module.functions[fn_id as usize].name.clone();
                    let arg_vals: Vec<VMValue> =
                        args.iter().map(|r| self.read_reg(*r, frame)).collect();
                    let result = self.execute_fn(fn_name.as_str(), arg_vals);
                    if let Some(dest_reg) = dest {
                        self.write_reg(frame, dest_reg, result);
                    }
                }
                VMOpcode::CallIndirect { dest, callee, args } => {
                    let callee_val = self.read_reg(callee, frame);
                    let fn_id = match callee_val {
                        VMValue::Ptr(alloc_id, 0) if alloc_id.kind == MemoryKind::Code => {
                            alloc_id.id
                        }
                        other => panic!("CallIndirect expected function pointer, got {}", other),
                    };

                    let fn_name = self.module.functions[fn_id as usize].name.clone();
                    let arg_vals: Vec<VMValue> =
                        args.iter().map(|r| self.read_reg(*r, frame)).collect();
                    let result = self.execute_fn(fn_name.as_str(), arg_vals);

                    if let Some(dest_reg) = dest {
                        self.write_reg(frame, dest_reg, result);
                    }
                }
                VMOpcode::DollarEval { dest, fn_id, args } => {
                    let scope_name = self.module.functions[fn_id as usize].name.clone();
                    let arg_vals: Vec<VMValue> =
                        args.iter().map(|r| self.read_reg(*r, frame)).collect();
                    let result = self.execute_fn(scope_name.as_str(), arg_vals);
                    if let Some(dest_reg) = dest {
                        self.write_reg(frame, dest_reg, result.clone());
                    }
                    //Write into the eval table
                    self.eval_table.results.insert(scope_name, result);
                }
                VMOpcode::Compare {
                    dest,
                    op,
                    src1,
                    src2,
                } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);

                    let res = match op {
                        // Equality
                        CmpOp::Eq => impl_cmp_op!(a, b, ==),
                        CmpOp::Neq => impl_cmp_op!(a, b, !=),

                        // Signed comparison
                        CmpOp::Slt => impl_cmp_op!(a, b, <),
                        CmpOp::Sgt => impl_cmp_op!(a, b, >),
                        CmpOp::Sle => impl_cmp_op!(a, b, <=),
                        CmpOp::Sge => impl_cmp_op!(a, b, >=),

                        // Unsigned comparison
                        CmpOp::Ult => impl_cmp_op!(a, b, <),
                        CmpOp::Ugt => impl_cmp_op!(a, b, >),
                        CmpOp::Ule => impl_cmp_op!(a, b, <=),
                        CmpOp::Uge => impl_cmp_op!(a, b, >=),

                        // Float comparison
                        CmpOp::Flt => impl_cmp_op!(a, b, <),
                        CmpOp::Fgt => impl_cmp_op!(a, b, >),
                        CmpOp::Fle => impl_cmp_op!(a, b, <=),
                        CmpOp::Fge => impl_cmp_op!(a, b, >=),
                    };

                    self.write_reg(frame, dest, res);
                }
                VMOpcode::Add { dest, src1, src2 } => {
                    let val1 = self.read_reg(src1, frame);
                    let val2 = self.read_reg(src2, frame);
                    let result = impl_numeric_op!(self,val1,val2,+);
                    self.write_reg(frame, dest, result);
                }
                VMOpcode::Sub { dest, src1, src2 } => {
                    let val1 = self.read_reg(src1, frame);
                    let val2 = self.read_reg(src2, frame);
                    let res = impl_numeric_op!(self, val1,val2,-);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::Mul { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_numeric_op!(self, a, b, *);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::Div { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_numeric_op!(self, a, b, /);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::Mod { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_numeric_op!(self, a, b, %);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::And { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, &);
                    self.write_reg(frame, dest, res);
                }

                VMOpcode::Or { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, |);
                    self.write_reg(frame, dest, res);
                }

                VMOpcode::Xor { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, ^);
                    self.write_reg(frame, dest, res);
                }

                VMOpcode::Shl { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, <<);
                    self.write_reg(frame, dest, res);
                }

                VMOpcode::Shr { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, >>);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::AShr { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_int_op!(a, b, >>);
                    self.write_reg(frame, dest, res);
                }

                VMOpcode::Cast { dest, src, to_ty } => {
                    let val = self.read_reg(src, frame);

                    let res = match (val.clone(), &to_ty.kind) {
                        // Integer/Numeric -> Pointer (inttoptr)
                        (v, MIRTykind::Ptr) if v.is_integer() => {
                            let offset = v.as_u128() as usize;
                            let null_alloc_id = AllocId {
                                kind: MemoryKind::Data,
                                id: u32::MAX,
                            };
                            VMValue::Ptr(null_alloc_id, offset)
                        }

                        // Pointer -> Integer (ptrtoint)
                        (VMValue::Ptr(_, offset), _) if to_ty.is_integer() => {
                            let raw_addr = offset as u128;
                            match &to_ty.kind {
                                MIRTykind::I8 => VMValue::I8(raw_addr as i8),
                                MIRTykind::U8 => VMValue::U8(raw_addr as u8),
                                MIRTykind::I16 => VMValue::I16(raw_addr as i16),
                                MIRTykind::U16 => VMValue::U16(raw_addr as u16),
                                MIRTykind::I32 => VMValue::I32(raw_addr as i32),
                                MIRTykind::U32 => VMValue::U32(raw_addr as u32),
                                MIRTykind::I64 => VMValue::I64(raw_addr as i64),
                                MIRTykind::U64 => VMValue::U64(raw_addr as u64),
                                MIRTykind::ISIZE => VMValue::Int(raw_addr as isize),
                                MIRTykind::USIZE => VMValue::UInt(raw_addr as usize),
                                MIRTykind::I128 => VMValue::I128(raw_addr as i128),
                                MIRTykind::U128 => VMValue::U128(raw_addr),
                                _ => panic!(
                                    "Invalid pointer-to-integer cast destination: {:?}",
                                    to_ty
                                ),
                            }
                        }

                        // General Primitives -> Target Integer Types
                        (v, target_kind) if to_ty.is_integer() => match target_kind {
                            MIRTykind::I8 => VMValue::I8(v.as_i128() as i8),
                            MIRTykind::U8 => VMValue::U8(v.as_u128() as u8),
                            MIRTykind::I16 => VMValue::I16(v.as_i128() as i16),
                            MIRTykind::U16 => VMValue::U16(v.as_u128() as u16),
                            MIRTykind::I32 => VMValue::I32(v.as_i128() as i32),
                            MIRTykind::U32 => VMValue::U32(v.as_u128() as u32),
                            MIRTykind::I64 => VMValue::I64(v.as_i128() as i64),
                            MIRTykind::U64 => VMValue::U64(v.as_u128() as u64),
                            MIRTykind::ISIZE => VMValue::Int(v.as_i128() as isize),
                            MIRTykind::USIZE => VMValue::UInt(v.as_u128() as usize),
                            MIRTykind::I128 => VMValue::I128(v.as_i128()),
                            MIRTykind::U128 => VMValue::U128(v.as_u128()),
                            MIRTykind::Bool => VMValue::Bool(v.as_u128() != 0),
                            _ => unreachable!(),
                        },

                        // General Primitives -> Target Float Types
                        (v, MIRTykind::F32) => VMValue::F32(v.as_f64() as f32),
                        (v, MIRTykind::F64) => VMValue::F64(v.as_f64()),

                        // Pointer -> Pointer (No-op cast)
                        (ptr @ VMValue::Ptr(_, _), MIRTykind::Ptr) => ptr,

                        (v, _) => panic!("Unsupported VM cast from {:?} to {:?}", v, to_ty),
                    };

                    self.write_reg(frame, dest, res);
                }
                VMOpcode::BitCast { dest, src, to_ty } => {
                    let val = self.read_reg(src, frame);
                    let res = val.bitcast_to(&to_ty);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::GetElementPtr {
                    dest,
                    ptr,
                    indices,
                    elem_ty,
                } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    if let VMValue::Ptr(alloc_id, base_offset) = ptr_val {
                        let mut offset = base_offset;
                        let mut current_ty = elem_ty.clone();

                        for (i, &idx_reg) in indices.iter().enumerate() {
                            let idx_val = self.read_reg(idx_reg, frame).as_isize().unwrap();

                            if i == 0 {
                                offset += (idx_val as usize) * elem_ty.size;
                                continue;
                            }

                            match &current_ty.kind {
                                MIRTykind::Array(elem_ty, _) => {
                                    offset += (idx_val as usize) * elem_ty.size;
                                    current_ty = *elem_ty.clone();
                                }
                                MIRTykind::Struct(_, _, fields) => {
                                    let mut field_offset = 0usize;
                                    for (field_i, (_, field_ty)) in fields.iter().enumerate() {
                                        let padding = if field_ty.align == 0 {
                                            0
                                        } else {
                                            (field_ty.align - (field_offset % field_ty.align))
                                                % field_ty.align
                                        };
                                        field_offset += padding;
                                        if field_i as i64 == idx_val as i64 {
                                            current_ty = field_ty.clone();
                                            break;
                                        }
                                        field_offset += field_ty.size;
                                    }
                                    offset += field_offset;
                                }
                                MIRTykind::Tuple(elem_tys) => {
                                    let mut field_offset = 0usize;
                                    for (field_i, elem_ty) in elem_tys.iter().enumerate() {
                                        let padding = if elem_ty.align == 0 {
                                            0
                                        } else {
                                            (elem_ty.align - (field_offset % elem_ty.align))
                                                % elem_ty.align
                                        };
                                        field_offset += padding;
                                        if field_i as i64 == idx_val as i64 {
                                            // was `idx` — fixed to `idx_val`
                                            current_ty = elem_ty.clone();
                                            break;
                                        }
                                        field_offset += elem_ty.size;
                                    }
                                    offset += field_offset;
                                }
                                _ => self.report_ice(
                                    "Cannot GEP further into a non-aggregate type".to_string(),
                                ),
                            }
                        }
                        self.write_reg(frame, dest, VMValue::Ptr(alloc_id, offset));
                    } else {
                        self.report_ice(format!("GEP target register {} is not a pointer", ptr));
                    }
                }
                _ => todo!("VMOpcode {:?} not implemented", instr),
            }
        }
    }

    fn read_reg(&mut self, reg: u16, frame: &VMFrame) -> VMValue {
        let Some(val) = &frame.registers[reg as usize] else {
            self.report_ice(format!("Register {} is uninitialized", reg));
            return VMValue::Poison;
        };
        val.clone()
    }

    fn write_reg(&mut self, frame: &mut VMFrame, dest: u16, val: VMValue) {
        frame.registers[dest as usize] = Some(val)
    }

    pub fn report_ice(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, None));
    }
}
