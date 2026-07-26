use std::fmt;

use crate::mir::{
    builder::MIRModule,
    instructions::{
        BasicBlock, BlockId, CmpOp, ConstantValue, FnId, GlobalId, MIRFn, MIRGlobal,
        MIRInstruction, MIROps, MIRParam, MIRTy, MIRTykind, MIRValue, Terminator, Vreg,
    },
};

impl fmt::Display for MIRModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module \"{}\"\n", self.name)?;

        // Print Globals
        if !self.globals.is_empty() {
            let mut global_ids: Vec<_> = self.globals.keys().cloned().collect();
            global_ids.sort_by_key(|g| g.0);
            for id in global_ids {
                if let Some(global) = self.globals.get(&id) {
                    writeln!(f, "{global}")?;
                }
            }
            writeln!(f)?;
        }

        // Print Functions
        let mut fn_ids: Vec<_> = self.functions.keys().cloned().collect();
        fn_ids.sort_by_key(|f| f.0);
        for (i, id) in fn_ids.iter().enumerate() {
            if let Some(func) = self.functions.get(id) {
                write!(f, "{func}")?;
                if i < fn_ids.len() - 1 {
                    writeln!(f, "\n")?;
                } else {
                    writeln!(f)?;
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Vreg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Vreg::Numbered(num) => write!(f, "%{num}"),
            Vreg::Named(name) => {
                // If 'name' already starts with '%', don't add another '%'
                if name.starts_with('%') {
                    write!(f, "{name}")
                } else {
                    write!(f, "%{name}")
                }
            }
        }
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

impl fmt::Display for FnId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn{}", self.0)
    }
}

impl fmt::Display for GlobalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@g{}", self.0)
    }
}

impl fmt::Display for MIRTykind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRTykind::I8 => write!(f, "i8"),
            MIRTykind::U8 => write!(f, "u8"),
            MIRTykind::I16 => write!(f, "i16"),
            MIRTykind::U16 => write!(f, "u16"),
            MIRTykind::I32 => write!(f, "i32"),
            MIRTykind::U32 => write!(f, "u32"),
            MIRTykind::I64 => write!(f, "i64"),
            MIRTykind::U64 => write!(f, "u64"),
            MIRTykind::I128 => write!(f, "i128"),
            MIRTykind::U128 => write!(f, "u128"),
            MIRTykind::USIZE => write!(f, "usize"),
            MIRTykind::ISIZE => write!(f, "isize"),
            MIRTykind::F32 => write!(f, "f32"),
            MIRTykind::F64 => write!(f, "f64"),
            MIRTykind::Bool => write!(f, "bool"),
            MIRTykind::Unit => write!(f, "unit"),
            MIRTykind::Ptr => write!(f, "ptr"),
        }
    }
}

impl fmt::Display for MIRTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for ConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantValue::I8(v) => write!(f, "{v}"),
            ConstantValue::U8(v) => write!(f, "{v}"),
            ConstantValue::I16(v) => write!(f, "{v}"),
            ConstantValue::U16(v) => write!(f, "{v}"),
            ConstantValue::I32(v) => write!(f, "{v}"),
            ConstantValue::U32(v) => write!(f, "{v}"),
            ConstantValue::I64(v) => write!(f, "{v}"),
            ConstantValue::U64(v) => write!(f, "{v}"),
            ConstantValue::Int(v) => write!(f, "{v}"),
            ConstantValue::UInt(v) => write!(f, "{v}"),
            ConstantValue::I128(v) => write!(f, "{v}"),
            ConstantValue::U128(v) => write!(f, "{v}"),
            ConstantValue::F32(v) => write!(f, "{v}"),
            ConstantValue::F64(v) => write!(f, "{v}"),
            ConstantValue::Bool(v) => write!(f, "{v}"),
        }
    }
}

impl fmt::Display for MIRValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRValue::Register { vreg, .. } => write!(f, "{vreg}"),
            MIRValue::Constant(c) => write!(f, "{c}"),
        }
    }
}

impl fmt::Display for MIROps {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIROps::Add => write!(f, "add"),
            MIROps::Sub => write!(f, "sub"),
            MIROps::Mul => write!(f, "mul"),
            MIROps::Sdiv => write!(f, "sdiv"),
            MIROps::Udiv => write!(f, "udiv"),
            MIROps::Mod => write!(f, "mod"),
            MIROps::Xor => write!(f, "xor"),
        }
    }
}

impl fmt::Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op_str = match self {
            CmpOp::Eq => "eq",
            CmpOp::Neq => "neq",
            CmpOp::Slt => "slt",
            CmpOp::Sgt => "sgt",
            CmpOp::Sle => "sle",
            CmpOp::Sge => "sge",
            CmpOp::Ult => "ult",
            CmpOp::Ugt => "ugt",
            CmpOp::Ule => "ule",
            CmpOp::Uge => "uge",
            CmpOp::Flt => "flt",
            CmpOp::Fgt => "fgt",
            CmpOp::Fle => "fle",
            CmpOp::Fge => "fge",
        };
        write!(f, "{op_str}")
    }
}

impl fmt::Display for MIRInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRInstruction::BinaryOperation { dest, op, lhs, rhs } => {
                write!(f, "    {dest} = {op} {lhs}, {rhs}")
            }
            MIRInstruction::Compare { dest, op, lhs, rhs } => {
                write!(f, "    {dest} = cmp {op} {lhs}, {rhs}")
            }
            MIRInstruction::Phi { dest, incoming } => {
                let incs: Vec<String> = incoming
                    .iter()
                    .map(|(val, block)| format!("[{val}, {block}]"))
                    .collect();
                write!(f, "    {dest} = phi {}", incs.join(", "))
            }
            MIRInstruction::Alloca { dest, ty, align } => {
                write!(f, "    {dest} = alloca {ty}, align {align}")
            }
            MIRInstruction::Load {
                dest,
                ptr,
                ty,
                align,
            } => {
                write!(f, "    {dest} = load {ty}, ptr {ptr}, align {align}")
            }
            MIRInstruction::Store { ptr, val, align } => {
                write!(f, "    store {val}, ptr {ptr}, align {align}")
            }
            MIRInstruction::Call { dest, callee, args } => {
                let arg_strs: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "    {dest} = call @{callee}({})", arg_strs.join(", "))
            }
            MIRInstruction::Assign { dest, src } => {
                write!(f, "    {dest} = {src}")
            }
            MIRInstruction::GetElementPtr { dest, ptr, offset } => {
                write!(f, "    {dest} = gep {ptr}, offset {offset}")
            }
            MIRInstruction::Cast {
                dest,
                src,
                from_ty,
                to_ty,
            } => {
                write!(f, "    {dest} = cast {src} from {from_ty} to {to_ty}")
            }
            MIRInstruction::BitCast { dest, src, to_ty } => {
                write!(f, "    {dest} = bitcast {src} to {to_ty}")
            }
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(Some(val)) => write!(f, "    ret {val}"),
            Terminator::Return(None) => write!(f, "    ret"),
            Terminator::Goto(block) => write!(f, "    goto {block}"),
            Terminator::Unreachable => write!(f, "    unreachable"),
            Terminator::Branch {
                cond,
                then,
                else_block,
            } => {
                write!(f, "    br {cond}, {then}, {else_block}")
            }
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.id)?;
        for inst in &self.instructions {
            writeln!(f, "{inst}")?;
        }
        write!(f, "{}", self.terminator)
    }
}

impl fmt::Display for MIRParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Formats as: "usize %x" (or "%x: usize" if preferred)
        // If self.name already starts with '%', drop the extra '%' below
        if self.name.starts_with('%') {
            write!(f, "{} {}", self.ty, self.name)
        } else {
            write!(f, "{} %{}", self.ty, self.name)
        }
    }
}

impl fmt::Display for MIRFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Format parameter list into comma-separated string
        let params_str = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        // Print function signature with parameters
        writeln!(f, "func @{}({}) {{", self.name, params_str)?;

        // Sort block keys so output order is deterministic and readable (bb0, bb1, bb2...)
        let mut block_ids: Vec<_> = self.blocks.keys().cloned().collect();
        block_ids.sort_by_key(|b| b.0);

        for (i, id) in block_ids.iter().enumerate() {
            if let Some(block) = self.blocks.get(id) {
                write!(f, "{block}")?;
                if i < block_ids.len() - 1 {
                    writeln!(f, "\n")?;
                } else {
                    writeln!(f)?;
                }
            }
        }
        write!(f, "}}")
    }
}

impl fmt::Display for MIRGlobal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut_str = if self.is_const { "const" } else { "" };
        write!(
            f,
            "global @{} {}: {} = {}",
            self.name, mut_str, self.ty, self.init
        )
    }
}
