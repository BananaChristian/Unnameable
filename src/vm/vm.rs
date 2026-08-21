use std::collections::HashMap;

use crate::{
    bc_builder::{BytecodeModule, VMOpcode},
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    impl_cmp_op, impl_int_op, impl_numeric_op,
    mir::{CmpOp, MIRTy, MIRTykind, StructId},
    vm::{
        Allocation,
        structures::{AllocId, EvalResultTable, VMFrame, VMMemory, VMValue},
    },
};

pub struct VM<'a> {
    pub module: &'a BytecodeModule,
    pub eval_table: EvalResultTable,
    pub memory: VMMemory,
    pub global_allocs: HashMap<u32, AllocId>, //Map global id to Alloc id
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl<'a> VM<'a> {
    pub fn new(module: &'a BytecodeModule, diagnostics: SharedDiagnostics) -> Self {
        let mut vm = VM {
            module,
            eval_table: EvalResultTable {
                results: HashMap::new(),
                global_allocs: HashMap::new(),
            },
            memory: VMMemory {
                allocations: HashMap::new(),
                next_alloc: 1,
            },
            global_allocs: HashMap::new(),
            diagnostics,
            corrupted: false,
        };
        vm.init_globals();
        vm
    }

    fn init_globals(&mut self) {
        for global in &self.module.globals {
            let alloc_id = AllocId(self.memory.next_alloc);
            self.memory.next_alloc += 1;

            let data = match &global.init_data {
                Some(VMValue::Array(elems)) => elems.clone(),
                Some(scalar) => vec![scalar.clone()],
                None => {
                    // Uninitialized space filled with Poison elements matching size
                    vec![VMValue::Poison; global.size_in_bytes as usize]
                }
            };
            self.memory
                .allocations
                .insert(alloc_id.clone(), Allocation { data });

            self.global_allocs.insert(global.id, alloc_id.clone());
            self.eval_table.global_allocs.insert(alloc_id, global.id);
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

                VMOpcode::LoadGlobal { dest, global_id } => {
                    let Some(alloc_id) = self.global_allocs.get(&global_id).cloned() else {
                        self.report_ice(format!("Undefined global_id: {}", global_id));
                        return VMValue::Poison;
                    };

                    // Load a base pointer referencing the global's allocation slot
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
                    let alloc_id = AllocId(self.memory.next_alloc);
                    self.memory.next_alloc += 1;

                    let allocation = Allocation {
                        data: vec![VMValue::Poison; size as usize],
                    };

                    self.memory.allocations.insert(alloc_id.clone(), allocation);
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, 0));
                }

                VMOpcode::Load {
                    dest,
                    ptr,
                    size,
                    ty,
                    ..
                } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    if let VMValue::Ptr(alloc_id, offset) = ptr_val {
                        let val = if size <= 1 {
                            self.mem_read(&alloc_id, offset)
                        } else {
                            match &ty.kind {
                                MIRTykind::Struct(struct_id, name, fields) => {
                                    self.load_struct(*struct_id, name, fields, &alloc_id, offset)
                                }
                                _ => {
                                    let elems: Vec<VMValue> = (0..size as usize)
                                        .map(|i| self.mem_read(&alloc_id, offset + i))
                                        .collect();
                                    VMValue::Array(elems)
                                }
                            }
                        };
                        self.write_reg(frame, dest, val);
                    } else {
                        self.report_ice("Load from non pointer".to_string());
                    }
                }
                VMOpcode::Store { ptr, val, .. } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    let src_val = self.read_reg(val, frame);
                    if let VMValue::Ptr(alloc_id, offset) = ptr_val {
                        self.mem_write(&alloc_id, offset, src_val);
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
                VMOpcode::Call { dest, fn_id, args } => {
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
                            // AllocId(0) denotes an unallocated/raw address space or null
                            VMValue::Ptr(AllocId(0), offset)
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
                    stride,
                } => {
                    let ptr_val = self.read_reg(ptr, frame);

                    if let VMValue::Ptr(alloc_id, current_offset) = ptr_val {
                        let mut total_offset = current_offset as isize;

                        for (i, index_reg) in indices.iter().enumerate() {
                            let index_val = self.read_reg(*index_reg, frame);
                            let idx = match index_val.as_isize() {
                                Some(idx_val) => idx_val,
                                None => {
                                    self.report_ice(format!(
                                        "GEP index register r{} ({:?}) is not an integer",
                                        index_reg, index_val
                                    ));
                                    0
                                }
                            };

                            if i == 0 {
                                // Index 0: Outer array/pointer offset scaled by total element stride
                                total_offset += idx * (stride as isize);
                            } else {
                                // Index 1+: Member/field access inside the aggregate (1 slot per index step)
                                total_offset += idx;
                            }
                        }

                        if total_offset < 0 {
                            self.report_ice(format!(
                                "GEP resulting offset {} underflowed below 0 on AllocId({})",
                                total_offset, alloc_id.0
                            ));
                            return VMValue::Poison;
                        }

                        self.write_reg(frame, dest, VMValue::Ptr(alloc_id, total_offset as usize));
                    } else {
                        self.report_ice(format!("GEP target register r{} is not a pointer", ptr));
                    }
                }
                _ => todo!("VMOpcode {:?} not implemented", instr),
            }
        }
    }

    fn load_struct(
        &mut self,
        struct_id: StructId,
        name: &str,
        fields: &[(String, MIRTy)],
        alloc_id: &AllocId,
        base_offset: usize,
    ) -> VMValue {
        let mut field_offset = base_offset;
        let mut field_vals = Vec::with_capacity(fields.len());

        for (_, field_ty) in fields {
            let field_slots = field_ty.slot_counter() as usize;
            let field_val = match &field_ty.kind {
                MIRTykind::Struct(nested_id, nested_name, nested_fields) => self.load_struct(
                    *nested_id,
                    nested_name,
                    nested_fields,
                    alloc_id,
                    field_offset,
                ),
                _ if field_slots > 1 => {
                    let elems: Vec<VMValue> = (0..field_slots)
                        .map(|i| self.mem_read(alloc_id, field_offset + i as usize))
                        .collect();
                    VMValue::Array(elems)
                }
                _ => self.mem_read(alloc_id, field_offset),
            };
            field_vals.push(field_val);
            field_offset += field_slots;
        }

        VMValue::Struct {
            struct_id,
            name: name.to_string(),
            fields: field_vals,
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

    fn mem_read(&mut self, alloc_id: &AllocId, offset: usize) -> VMValue {
        let Some(alloc) = self.memory.allocations.get(alloc_id) else {
            self.report_ice(format!("Invalid or freed allocation ID: {}", alloc_id.0));
            return VMValue::Poison;
        };

        if offset >= alloc.data.len() {
            self.report_ice(format!(
                "Out-of-bounds read at AllocId({}) with offset {} (allocation size: {})",
                alloc_id.0,
                offset,
                alloc.data.len()
            ));
            return VMValue::Poison;
        }

        alloc.data[offset].clone()
    }

    fn mem_write(&mut self, alloc_id: &AllocId, offset: usize, val: VMValue) {
        let len = match self.memory.allocations.get(alloc_id) {
            Some(alloc) => alloc.data.len(),
            None => {
                self.report_ice(format!("Invalid or freed allocation ID: {}", alloc_id.0));
                return;
            }
        };

        //  Perform bounds check using `self` safely
        if offset >= len {
            self.report_ice(format!(
                "Out-of-bounds write at AllocId({}) with offset {} (allocation size: {})",
                alloc_id.0, offset, len
            ));
            return;
        }

        self.memory.allocations.get_mut(alloc_id).unwrap().data[offset] = val;
    }

    pub fn report_ice(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, None));
    }
}
