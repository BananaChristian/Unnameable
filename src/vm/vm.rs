use std::collections::HashMap;

use crate::{
    bc_builder::{BytecodeModule, VMOpcode},
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
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
                VMOpcode::ConstInt { dest, val } => {
                    frame.registers[dest as usize] = Some(VMValue::Int(val));
                }
                VMOpcode::ConstUSize { dest, val } => {
                    frame.registers[dest as usize] = Some(VMValue::USize(val));
                }

                VMOpcode::ConstBool { dest, val } => {
                    frame.registers[dest as usize] = Some(VMValue::Bool(val));
                }
                VMOpcode::Jump { target_pc } => {
                    frame.ip = target_pc;
                }
                VMOpcode::Alloca { dest, size, .. } => {
                    let alloc_id = AllocId(self.memory.next_alloc);
                    self.memory.next_alloc += 1;
                    self.memory
                        .allocations
                        .insert(alloc_id.clone(), vec![0u8; size as usize]);
                    frame.registers[dest as usize] = Some(VMValue::Ptr(alloc_id, 0));
                }
                VMOpcode::Return { val } => {
                    return match val {
                        Some(v) => self.get_reg_val(v, frame),
                        None => VMValue::Unit,
                    };
                }
                VMOpcode::Call { dest, fn_id, args } => {
                    let fn_name = self.module.functions[fn_id as usize].name.clone();
                    let arg_vals: Vec<VMValue> =
                        args.iter().map(|r| self.get_reg_val(*r, frame)).collect();
                    let result = self.execute_fn(fn_name.as_str(), arg_vals);
                    if let Some(dest_reg) = dest {
                        frame.registers[dest_reg as usize] = Some(result)
                    }
                }
                VMOpcode::DollarEval { dest, fn_id, args } => {
                    let scope_name = self.module.functions[fn_id as usize].name.clone();
                    let arg_vals: Vec<VMValue> =
                        args.iter().map(|r| self.get_reg_val(*r, frame)).collect();
                    let result = self.execute_fn(scope_name.as_str(), arg_vals);
                    if let Some(dest_reg) = dest {
                        frame.registers[dest_reg as usize] = Some(result.clone());
                    }
                    //Write into the eval table
                    self.eval_table.results.insert(scope_name, result);
                }
                _ => todo!(),
            }
        }
    }

    fn get_reg_val(&mut self, reg: u16, frame: &VMFrame) -> VMValue {
        let Some(val) = &frame.registers[reg as usize] else {
            self.report_ice(format!("Register {} is uninitialized", reg));
            return VMValue::Poison;
        };
        val.clone()
    }

    pub fn report_ice(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, None));
    }
}
