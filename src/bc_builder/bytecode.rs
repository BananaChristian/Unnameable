use std::collections::HashMap;

use crate::mir::CmpOp;

#[derive(Debug, Clone, PartialEq)]
pub enum DollarMode {
    Full,
    Read,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VMOpcode {
    // Immediate Constants: r[dest] = val
    ConstI8 {
        dest: u16,
        val: i8,
    },
    ConstU8 {
        dest: u16,
        val: u8,
    },
    ConstI16 {
        dest: u16,
        val: i16,
    },
    ConstU16 {
        dest: u16,
        val: u16,
    },
    ConstI32 {
        dest: u16,
        val: i32,
    },
    ConstU32 {
        dest: u16,
        val: u32,
    },
    ConstI64 {
        dest: u16,
        val: i64,
    },
    ConstU64 {
        dest: u16,
        val: u64,
    },
    ConstIsize {
        dest: u16,
        val: isize,
    },
    ConstUSize {
        dest: u16,
        val: usize,
    },
    ConstI128 {
        dest: u16,
        val: i128,
    },
    ConstU128 {
        dest: u16,
        val: u128,
    },
    ConstBool {
        dest: u16,
        val: bool,
    },

    // Register Copy: r[dest] = r[src]
    Move {
        dest: u16,
        src: u16,
    },

    // Stack Allocation: r[dest] = alloc_stack(size, align)
    Alloca {
        dest: u16,
        size: u32,
        align: u32,
    },

    // Pointer Memory Access
    Load {
        dest: u16,
        ptr: u16,
        mode: DollarMode,
    },
    Store {
        ptr: u16,
        val: u16,
        mode: DollarMode,
    },

    // Global Memory Access
    LoadGlobal {
        dest: u16,
        global_id: u32,
    },
    StoreGlobal {
        global_id: u32,
        src: u16,
    },

    // Primitive Arithmetic
    Add {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Sub {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Mul {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Div {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Mod {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Xor {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    And {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Or {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Shl {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Shr {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    AShr {
        dest: u16,
        src1: u16,
        src2: u16,
    },
    Compare {
        dest: u16,
        op: CmpOp,
        src1: u16,
        src2: u16,
    },

    // Control Flow
    Jump {
        target_pc: usize,
    },
    BranchIf {
        cond: u16,
        then_pc: usize,
        else_pc: usize,
    },

    // Function Call
    Call {
        dest: Option<u16>,
        fn_id: u32,
        args: Vec<u16>,
    },

    //Special dollar scope evaluation
    DollarEval {
        dest: Option<u16>,
        fn_id: u32,
        args: Vec<u16>,
    },

    // Return from current frame
    Return {
        val: Option<u16>,
    },
}

#[derive(Default)]
pub struct RegisterMap {
    pub mapping: HashMap<String, u16>,
    pub next_index: u16,
}

impl RegisterMap {
    pub fn new() -> Self {
        Self::default()
    }

    // Gets the existing u16 register index for a string name,
    // or assigns the next available u16 number if seen for the first time.
    pub fn get_or_insert(&mut self, name: &str) -> u16 {
        if let Some(&idx) = self.mapping.get(name) {
            idx
        } else {
            let idx = self.next_index;
            self.mapping.insert(name.to_string(), idx);
            self.next_index += 1;
            idx
        }
    }

    // Returns the total number of unique registers assigned so far.
    // This value gets written directly into `BytecodeFn.register_count`.
    pub fn total_registers(&self) -> u16 {
        self.next_index
    }
}

#[derive(Debug, Clone)]
pub struct BytecodeFn {
    pub name: String,
    pub mode: DollarMode,
    pub register_count: u16,
    pub param_count: u16,
    pub instructions: Vec<VMOpcode>,
}

impl BytecodeFn {
    pub fn new() -> Self {
        BytecodeFn {
            name: String::new(),
            mode: DollarMode::None,
            register_count: 0,
            param_count: 0,
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub id: u32,
    pub name: String,
    pub size_in_bytes: u32,
    pub init_data: Option<Vec<u8>>,
}

#[derive(Debug, Clone, Default)]
pub struct BytecodeModule {
    // Lowered executable functions
    pub functions: Vec<BytecodeFn>,
    pub fn_symbols: HashMap<String, u32>,

    /// Global memory segment
    pub globals: Vec<GlobalVar>,
    pub global_symbols: HashMap<String, u32>,
}
