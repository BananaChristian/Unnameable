use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Vreg(pub usize);

#[derive(Debug, Clone, PartialEq,Eq, Hash,Copy)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone)]
pub struct GlobalId(pub usize); //ID for global variables

#[derive(Debug, Clone, PartialEq,Eq, Hash, Copy)]
pub struct FnId(pub usize);//ID for functions

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
    Register(Vreg),
    Constant(ConstantValue),
}

#[derive(Debug, Clone)]
pub enum MIROps {
    Add,
    Sub,
    Mul,
    Sdiv,
    Udiv,
    Mod,
}

#[derive(Debug, Clone)]
pub enum MirTy {
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
    F32,
    F64,
    Bool,
    Unit,
    Ptr, //All pointers are opaque
    FnPtr {
        params: Vec<MirTy>,
        return_ty: Box<MirTy>,
    },
    Array {
        element_ty: Box<MirTy>,
        length: usize,
    },
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<MIRInstruction>,
    pub terminator: Option<Terminator>,
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

    //%dest= phi [%val1,bb0], [%val2,bb1]
    Phi {
        dest: MIRValue,
        incoming: Vec<(MIRValue, BlockId)>,
    },

    Alloca {
        dest: MIRValue,
        ty: MirTy,
    },

    Load {
        dest: MIRValue,
        ptr: MIRValue,
    },

    Store {
        ptr: MIRValue,
        val: MIRValue,
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
}

#[derive(Debug, Clone)]
pub enum MIRLinkage {
    Public,
    Internal,
}

#[derive(Debug, Clone)]
pub struct MIRFn {
    pub fn_id: FnId,
    pub name: String,
    pub blocks: HashMap<BlockId, BasicBlock>,
    pub entry_block: BlockId, //This also doubles to identify a function
}

#[derive(Debug, Clone)]
pub struct MIRGlobal {
    pub global_id: GlobalId,
    pub name: String,
    pub ty: MirTy,
    pub is_const: bool,
    pub init: Option<MIRValue>,
    pub linkage: MIRLinkage,
}
