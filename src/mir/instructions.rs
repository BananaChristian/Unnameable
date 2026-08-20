use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Vreg {
    Numbered(usize),
    Named(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalId(pub usize); //ID for global variables

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct FnId(pub usize); //ID for functions

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct StructId(pub usize); //ID for structs

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRDollarMode {
    None,     //Off limits to dollar bill engine
    ReadOnly, //$
    Full,     //$$
}

#[derive(Debug, Clone)]
pub enum ConstantValue {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Int(isize),
    UInt(usize),
    I128(i128),
    U128(u128),
    F32(f32),
    F64(f64),
    Char8(u8),
    Char16(u16),
    Char32(u32),
    Bool(bool),
    Ptr(usize), //the pointer and the offset
    Array(Vec<ConstantValue>),
}

#[derive(Debug, Clone)]
pub enum MIRValue {
    Register { vreg: Vreg, ty: MIRTy },
    Global(GlobalId),
    Constant(ConstantValue),
    Poison,
}

#[derive(Debug, Clone)]
pub enum MIROps {
    Add,
    Sub,
    Mul,
    Sdiv,
    Udiv,
    Mod,
    Xor,
    And,  // bitwise and
    Or,   // bitwise or
    Shl,  // shift left
    Shr,  // shift right
    Ashr, //arithemtic right shift
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    Eq,
    Neq,
    // signed
    Slt, // signed less than
    Sgt, // signed greater than
    Sle, // signed less or equal
    Sge, // signed greater or equal
    // unsigned
    Ult,
    Ugt,
    Ule,
    Uge,
    // float
    Flt,
    Fgt,
    Fle,
    Fge,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MIRTy {
    pub kind: MIRTykind,
    pub size: usize,  //In bytes
    pub align: usize, //In bytes
}

impl MIRTy {
    pub fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            MIRTykind::I8
                | MIRTykind::U8
                | MIRTykind::I16
                | MIRTykind::U16
                | MIRTykind::I32
                | MIRTykind::U32
                | MIRTykind::I64
                | MIRTykind::U64
                | MIRTykind::I128
                | MIRTykind::U128
                | MIRTykind::ISIZE
                | MIRTykind::USIZE
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self.kind,
            MIRTykind::I8
                | MIRTykind::I16
                | MIRTykind::I32
                | MIRTykind::I64
                | MIRTykind::I128
                | MIRTykind::ISIZE
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, MIRTykind::F32 | MIRTykind::F64)
    }

    pub fn bit_width(&self) -> usize {
        self.size * 8
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, MIRTykind::Ptr)
    }

    pub fn slot_counter(&self) -> u32 {
        match &self.kind {
            MIRTykind::Array(elem_ty, count) => *count as u32 * elem_ty.slot_counter(),
            _ => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MIRTykind {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    I128,
    U128,
    USIZE,
    ISIZE,
    F32,
    F64,
    CHAR8,
    CHAR16,
    CHAR32,
    Bool,
    Unit,
    Ptr, //All pointers are opaque
    Array(Box<MIRTy>, usize),
    Struct(StructId, String),
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<MIRInstruction>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    //ret
    Return(Option<MIRValue>),
    //goto bb0
    Goto(BlockId),
    Unreachable,
    Branch {
        cond: MIRValue,
        then: BlockId,
        else_block: BlockId,
    },
}

#[derive(Debug, Clone)]
pub enum MIRInstruction {
    //%dest= op %lhs, %rhs
    BinaryOperation {
        dest: MIRValue,
        op: MIROps,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    Compare {
        dest: MIRValue, // always bool
        op: CmpOp,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    //%dest= phi [%val1,bb0], [%val2,bb1]
    Phi {
        dest: MIRValue,
        incoming: Vec<(MIRValue, BlockId)>,
    },

    Alloca {
        dest: MIRValue,
        ty: MIRTy,
        dollar_mode: MIRDollarMode,
        align: usize, //In bytes
    },

    Load {
        dest: MIRValue,
        ptr: MIRValue,
        ty: MIRTy,
        align: usize,
    },

    Store {
        ptr: MIRValue,
        val: MIRValue,
        align: usize,
    },

    //%dest= call %func_name(%arg1,%arg2)
    Call {
        dest: MIRValue,
        callee: String,
        args: Vec<MIRValue>,
    },

    Assign {
        dest: MIRValue,
        src: MIRValue,
    },

    GetElementPtr {
        dest: MIRValue,
        ptr: MIRValue,
        indices: Vec<MIRValue>,
        elem_ty: MIRTy,  // The type of element being pointed to
    },

    // Cast operations
    Cast {
        dest: MIRValue,
        src: MIRValue,
        from_ty: MIRTy,
        to_ty: MIRTy,
    },
    BitCast {
        dest: MIRValue,
        src: MIRValue,
        to_ty: MIRTy,
    },

    // %dest = &%src (yields a pointer to an lvalue / stack location / global)
    AddrOf {
        dest: MIRValue,
        src: MIRValue,
    },

    // Evaluates a compile-time dollar scope in the Dollar Bill engine
    DollarEval {
        dest: MIRValue,      // Where the trailing result of the block goes
        scope_fn: String,    // Name of the synthesized function, e.g. "@$$scope_0"
        args: Vec<MIRValue>, // Captured variables passed into the scope
    },
}

#[derive(Debug, Clone)]
pub enum MIRLinkage {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct MIRParam {
    pub name: String,
    pub dollar_mode: MIRDollarMode,
    pub ty: MIRTy,
}

#[derive(Debug, Clone)]
pub struct MIRFn {
    pub fn_id: FnId,
    pub name: String,
    pub params: Vec<MIRParam>,
    pub dollar_mode: MIRDollarMode,
    pub linkage: MIRLinkage,
    pub blocks: HashMap<BlockId, BasicBlock>,
    pub entry_block: BlockId,
    pub ret_ty: MIRTy,
}

#[derive(Debug, Clone)]
pub struct MIRStructDecl {
    pub struct_id: StructId,
    pub name: String,
    pub fields: Vec<(String, MIRTy)>,
}

#[derive(Debug, Clone)]
pub struct MIRGlobal {
    pub global_id: GlobalId,
    pub name: String,
    pub ty: MIRTy,
    pub dollar_mode: MIRDollarMode,
    pub is_const: bool,
    pub init: MIRValue,
    pub linkage: MIRLinkage,
}

#[derive(Debug, Clone)]
pub struct MIRModule {
    pub name: String, //Module name
    pub globals: HashMap<GlobalId, MIRGlobal>,
    pub structs: HashMap<StructId, MIRStructDecl>,
    pub functions: HashMap<FnId, MIRFn>,
}
