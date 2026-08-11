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
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum MIRValue {
    Register { vreg: Vreg, ty: MIRTy },
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct MIRTy {
    pub kind: MIRTykind,
    pub align: usize,
}

#[derive(Debug, Clone)]
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
    Bool,
    Unit,
    Ptr, //All pointers are opaque
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

    // GEP equivalent field/index access into structs and arrays
    GetElementPtr {
        dest: MIRValue,
        ptr: MIRValue,
        offset: MIRValue, // byte offset
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
pub struct MIRGlobal {
    pub global_id: GlobalId,
    pub name: String,
    pub ty: MIRTy,
    pub dollar_mode: MIRDollarMode,
    pub is_const: bool,
    pub init: MIRValue,
    pub linkage: MIRLinkage,
}
