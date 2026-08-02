use std::collections::HashMap;

use crate::{
    bc_builder::{BytecodeModule, DollarMode, VMOpcode},
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

    fn execute_dollar_scopes(&mut self) {
        let scope_fns: Vec<String> = self
            .module
            .functions
            .iter()
            .filter(|f| f.mode == DollarMode::Full && f.name.starts_with("$$scope"))
            .map(|f| f.name.clone())
            .collect();
    }

    fn execute_fn(&mut self, fn_name: &str, args: Vec<VMValue>) {
        let Some(func) = self
            .module
            .functions
            .iter()
            .find(|f| f.name == fn_name)
            .clone()
        else {
            self.report_ice(format!("Cannot find function '{}' ", fn_name));
            return;
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

        self.run_frame(&mut frame, &func.instructions);
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
                _ => todo!(),
            }
        }
    }

    pub fn report_ice(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, None));
    }
}
