use std::collections::HashMap;

use crate::{
    bc_builder::{BytecodeModule, VMOpcode},
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    impl_binary_op,
    vm::structures::{AllocId, EvalResultTable, VMFrame, VMMemory, VMValue},
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
        VM {
            module,
            eval_table: EvalResultTable {
                results: HashMap::new(),
            },
            memory: VMMemory {
                allocations: HashMap::new(),
                next_alloc: 0,
            },
            diagnostics,
            corrupted: false,
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
                VMOpcode::ConstBool { dest, val } => {
                    self.write_reg(frame, dest, VMValue::Bool(val));
                }
                VMOpcode::Jump { target_pc } => {
                    frame.ip = target_pc;
                }
                VMOpcode::Alloca { dest, .. } => {
                    let alloc_id = AllocId(self.memory.next_alloc);
                    self.memory.next_alloc += 1;
                    self.memory
                        .allocations
                        .insert(alloc_id.clone(), VMValue::Unit);
                    self.write_reg(frame, dest, VMValue::Ptr(alloc_id, 0));
                }
                VMOpcode::Load { dest, ptr, .. } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    if let VMValue::Ptr(alloc_id, _) = ptr_val {
                        let val = self.mem_read(&alloc_id);
                        self.write_reg(frame, dest, val);
                    } else {
                        self.report_ice("Load from non pointer".to_string());
                    }
                }
                VMOpcode::Store { ptr, val, .. } => {
                    let ptr_val = self.read_reg(ptr, frame);
                    let src_val = self.read_reg(val, frame);
                    if let VMValue::Ptr(alloc_id, _) = ptr_val {
                        self.mem_write(&alloc_id, src_val);
                    } else {
                        self.report_ice("Store to a non pointer".to_string());
                    }
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
                VMOpcode::Add { dest, src1, src2 } => {
                    let val1 = self.read_reg(src1, frame);
                    let val2 = self.read_reg(src2, frame);
                    let result = impl_binary_op!(self,val1,val2,+);
                    self.write_reg(frame, dest, result);
                }
                VMOpcode::Sub { dest, src1, src2 } => {
                    let val1 = self.read_reg(src1, frame);
                    let val2 = self.read_reg(src2, frame);
                    let res = impl_binary_op!(self, val1,val2,-);
                    self.write_reg(frame, dest, res);
                }
                VMOpcode::Mul { dest, src1, src2 } => {
                    let a = self.read_reg(src1, frame);
                    let b = self.read_reg(src2, frame);
                    let res = impl_binary_op!(self, a, b, *);
                    self.write_reg(frame, dest, res);
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

    fn mem_read(&mut self, alloc_id: &AllocId) -> VMValue {
        let Some(val) = self.memory.allocations.get(alloc_id).clone() else {
            self.report_ice(format!("Failed to get value at allocation {}", alloc_id.0));
            return VMValue::Poison;
        };
        val.clone()
    }

    fn mem_write(&mut self, alloc_id: &AllocId, val: VMValue) {
        self.memory.allocations.insert(alloc_id.clone(), val);
    }

    pub fn report_ice(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, None));
    }
}
