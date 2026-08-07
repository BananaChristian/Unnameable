use std::collections::HashMap;
use std::fmt::{self, Write};

use crate::bc_builder::bytecode::{BytecodeModule, DollarMode, VMOpcode};

impl fmt::Display for DollarMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DollarMode::Full => write!(f, "Full"),
            DollarMode::Read => write!(f, "Read"),
            DollarMode::None => write!(f, "None"),
        }
    }
}

pub struct BytecodePrinter;

impl BytecodePrinter {
    /// Formats an entire BytecodeModule into a readable assembly-style String.
    pub fn print_module(module: &BytecodeModule) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "=== BYTECODE MODULE ===");

        //  Print Globals
        let _ = writeln!(out, "Globals ({}):", module.globals.len());
        if module.globals.is_empty() {
            let _ = writeln!(out, "  (none)");
        } else {
            for global in &module.globals {
                let init_str = match &global.init_data {
                    Some(bytes) => format!(
                        " [init: {:?}]",
                        bytes
                            .iter()
                            .map(|b| format!("{:02X}", b))
                            .collect::<Vec<_>>()
                    ),
                    None => String::new(),
                };
                let _ = writeln!(
                    out,
                    "  @{}: {:?} ({} bytes){}",
                    global.id, global.name, global.size_in_bytes, init_str
                );
            }
        }
        let _ = writeln!(out);

        // Build a reverse index for function calls: fn_id -> fn_name
        let mut fn_id_to_name = HashMap::new();
        for (name, &id) in &module.fn_symbols {
            fn_id_to_name.insert(id, name.as_str());
        }

        // Print Functions
        let _ = writeln!(out, "Functions ({}):", module.functions.len());
        for func in &module.functions {
            let _ = writeln!(out, "{}", "-".repeat(50));
            let _ = writeln!(
                out,
                "func @{} (mode: {}, params: {}, registers: {})",
                func.name, func.mode, func.param_count, func.register_count
            );
            let _ = writeln!(out, "{}", "-".repeat(50));

            if func.instructions.is_empty() {
                let _ = writeln!(out, "  (empty function)");
            } else {
                for (pc, inst) in func.instructions.iter().enumerate() {
                    let formatted_inst = Self::format_opcode(inst, &fn_id_to_name);
                    let _ = writeln!(out, "  [{:04}] {}", pc, formatted_inst);
                }
            }
            let _ = writeln!(out);
        }

        let _ = writeln!(out, "{}", "=".repeat(50));
        out
    }

    /// Helper to format individual instructions with indexed registers (r0, r1)
    fn format_opcode(inst: &VMOpcode, fn_id_to_name: &HashMap<u32, &str>) -> String {
        match inst {
            VMOpcode::ConstInt { dest, val } => format!("r{} = {}", dest, val),
            VMOpcode::ConstUSize { dest, val } => format!("r{} = {} (usize)", dest, val),
            VMOpcode::ConstBool { dest, val } => format!("r{} = {}", dest, val),

            VMOpcode::Move { dest, src } => format!("r{} = r{}", dest, src),

            VMOpcode::Alloca { dest, size, align } => {
                format!("r{} = alloc_stack(size: {}, align: {})", dest, size, align)
            }

            VMOpcode::Load { dest, ptr, mode } => {
                format!("load r{}, [r{}] (mode: {})", dest, ptr, mode)
            }
            VMOpcode::Store { ptr, val, mode } => {
                format!("store [r{}], r{} (mode: {})", ptr, val, mode)
            }

            VMOpcode::LoadGlobal { dest, global_id } => {
                format!("r{} = load_global @{}", dest, global_id)
            }
            VMOpcode::StoreGlobal { global_id, src } => {
                format!("store_global @{}, r{}", global_id, src)
            }

            VMOpcode::Add { dest, src1, src2 } => format!("r{} = r{} + r{}", dest, src1, src2),
            VMOpcode::Sub { dest, src1, src2 } => format!("r{} = r{} - r{}", dest, src1, src2),
            VMOpcode::Mul { dest, src1, src2 } => format!("r{} = r{} * r{}", dest, src1, src2),
            VMOpcode::Div { dest, src1, src2 } => format!("r{} = r{} / r{}", dest, src1, src2),

            VMOpcode::Jump { target_pc } => format!("jump -> [{:04}]", target_pc),
            VMOpcode::BranchIf {
                cond,
                then_pc,
                else_pc,
            } => {
                format!(
                    "branch_if r{} -> then [{:04}] else [{:04}]",
                    cond, then_pc, else_pc
                )
            }

            VMOpcode::Call { dest, fn_id, args } => {
                let fn_label = fn_id_to_name
                    .get(fn_id)
                    .map(|name| format!("@{}", name))
                    .unwrap_or_else(|| format!("func@{}", fn_id));
                let args_str = args
                    .iter()
                    .map(|a| format!("r{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");

                match dest {
                    Some(d) => format!("r{} = call {}({})", d, fn_label, args_str),
                    None => format!("call {}({})", fn_label, args_str),
                }
            }

            VMOpcode::DollarEval { dest, fn_id, args } => {
                let fn_label = fn_id_to_name
                    .get(fn_id)
                    .map(|name| format!("@{}", name))
                    .unwrap_or_else(|| format!("func@{}", fn_id));
                let args_str = args
                    .iter()
                    .map(|a| format!("r{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");

                match dest {
                    Some(d) => format!("r{} = $$eval {}({})", d, fn_label, args_str),
                    None => format!("$$eval {}({})", fn_label, args_str),
                }
            }

            VMOpcode::Return { val } => match val {
                Some(v) => format!("return r{}", v),
                None => "return".to_string(),
            },
        }
    }
}
