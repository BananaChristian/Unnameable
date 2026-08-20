use std::fmt::{self};

use crate::mir::{
    MIRModule,
    instructions::{
        BasicBlock, BlockId, CmpOp, ConstantValue, FnId, GlobalId, MIRDollarMode, MIRFn, MIRGlobal,
        MIRInstruction, MIRLinkage, MIROps, MIRParam, MIRStructDecl, MIRTy, MIRTykind, MIRValue,
        StructId, Terminator, Vreg,
    },
};

impl fmt::Display for MIRModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "module \"{}\"\n", self.name)?;

        // Print Structs
        if !self.structs.is_empty() {
            let mut struct_ids: Vec<_> = self.structs.keys().cloned().collect();
            struct_ids.sort_by_key(|s| s.0);
            for id in struct_ids {
                if let Some(struct_decl) = self.structs.get(&id) {
                    writeln!(f, "{struct_decl}")?;
                }
            }
            writeln!(f)?;
        }

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

impl fmt::Display for StructId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@s{}", self.0)
    }
}

impl fmt::Display for MIRDollarMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRDollarMode::None => write!(f, ""),
            MIRDollarMode::ReadOnly => write!(f, "$"),
            MIRDollarMode::Full => write!(f, "$$"),
        }
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
            MIRTykind::CHAR8 => write!(f, "char8"),
            MIRTykind::CHAR16 => write!(f, "char16"),
            MIRTykind::CHAR32 => write!(f, "char32"),
            MIRTykind::Array(elem_ty, len) => write!(f, "[{} x {}]", len, elem_ty.kind),
            MIRTykind::Struct(_, name,_) => write!(f, "%{}", name),
        }
    }
}

impl fmt::Display for MIRTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

fn format_escaped_char(f: &mut std::fmt::Formatter<'_>, ch: char) -> std::fmt::Result {
    match ch {
        '\0' => write!(f, "'\\0'"),
        '\n' => write!(f, "'\\n'"),
        '\r' => write!(f, "'\\r'"),
        '\t' => write!(f, "'\\t'"),
        '\\' => write!(f, "'\\\\'"),
        '\'' => write!(f, "'\\''"),
        // Printable ASCII and unicode characters
        c if !c.is_control() => write!(f, "'{c}'"),
        // Non-printable control characters output as hex escapes
        c => write!(f, "'\\u{{{:04x}}}'", c as u32),
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
            ConstantValue::Char8(v) => format_escaped_char(f, *v as char),
            ConstantValue::Char16(v) => {
                let ch = char::from_u32(*v as u32).unwrap_or('\u{FFFD}');
                format_escaped_char(f, ch)
            }
            ConstantValue::Char32(v) => {
                let ch = char::from_u32(*v).unwrap_or('\u{FFFD}');
                format_escaped_char(f, ch)
            }
            ConstantValue::Bool(v) => write!(f, "{v}"),
            ConstantValue::Ptr(addr) => match *addr {
                0 => write!(f, "ptr null"),
                addr if addr == usize::MAX => write!(f, "ptr -1 (0x{:x})", addr),
                addr => write!(f, "ptr {:#x}", addr),
            },
            ConstantValue::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl fmt::Display for MIRValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MIRValue::Register { vreg, .. } => write!(f, "{vreg}"),
            MIRValue::Global(id) => write!(f, "@global_{}", id.0),
            MIRValue::Constant(c) => write!(f, "{c}"),
            MIRValue::Poison => write!(f, "poison"),
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
            MIROps::And => write!(f, "and"),
            MIROps::Shr => write!(f, "shr"),
            MIROps::Shl => write!(f, "shl"),
            MIROps::Ashr => write!(f, "ashr"),
            MIROps::Or => write!(f, "or"),
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
            MIRInstruction::Alloca {
                dest,
                ty,
                dollar_mode,
                align,
            } => {
                write!(f, "    {dest} = alloca {ty} {dollar_mode}, align {align}")
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
            MIRInstruction::AddrOf { dest, src } => {
                write!(f, "    {dest} = addr_of {src}")
            }
            MIRInstruction::Call { dest, callee, args } => {
                let arg_strs: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "    {dest} = call @{callee}({})", arg_strs.join(", "))
            }
            MIRInstruction::Assign { dest, src } => {
                write!(f, "    {dest} = {src}")
            }
            MIRInstruction::GetElementPtr {
                dest,
                ptr,
                indices,
                elem_ty,
            } => {
                let formatted_indices = indices
                    .iter()
                    .map(|idx| format!("{idx}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(
                    f,
                    "    {dest} = gep {elem_ty}, ptr {ptr}, indices [{formatted_indices}]"
                )
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
            MIRInstruction::DollarEval {
                dest,
                scope_fn,
                args,
            } => {
                let arg_strs: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                if args.is_empty() {
                    write!(f, "    {dest} = $$eval @{scope_fn}")
                } else {
                    write!(
                        f,
                        "    {dest} = $$eval @{scope_fn}({})",
                        arg_strs.join(", ")
                    )
                }
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
        // Formats as: "usize %x [Full]" or "%x: usize [Full]"
        let name_str = if self.name.starts_with('%') {
            self.name.to_string()
        } else {
            format!("%{}", self.name)
        };
        write!(f, "{} {} {}", self.dollar_mode, self.ty, name_str)
    }
}

impl fmt::Display for MIRStructDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{} = type {{ ", self.name)?;
        for (i, (field_name, field_ty)) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{field_name}: {field_ty}")?;
        }
        write!(f, " }}")
    }
}

impl fmt::Display for MIRFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let linkage_str = match self.linkage {
            MIRLinkage::Public => "expose ",
            MIRLinkage::Private => "",
        };

        // Output signature: $$ expose func @my_fn(i32 %x) {
        writeln!(
            f,
            "{}{}func @{}({}) {{",
            self.dollar_mode, linkage_str, self.name, params_str
        )?;

        // Deterministic sorting of basic blocks starting from entry_block
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
        let linkage_str = match self.linkage {
            MIRLinkage::Public => "expose ",
            MIRLinkage::Private => "",
        };
        let mut_str = if self.is_const { "const" } else { "mut" };

        write!(
            f,
            "{}{}global @{} {}: {} = {}",
            self.dollar_mode, linkage_str, self.name, mut_str, self.ty, self.init
        )
    }
}
