use std::collections::HashMap;

use crate::bc_builder::DollarMode;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AllocId(pub usize);

#[derive(Debug, Clone)]
pub struct EvalResultTable {
    pub results: HashMap<String, VMValue>,
}

#[derive(Debug, Clone)]
pub enum VMValue {
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
    Ptr(AllocId, usize), //allocation id + offset
    Unit,
    Poison,
}

pub struct VMMemory {
    pub allocations: HashMap<AllocId, Vec<u8>>,
    pub next_alloc: usize,
}

pub struct VMFrame {
    pub fn_name: String,
    pub mode: DollarMode,
    pub ip: usize, //Instruction pointer
    pub registers: Vec<Option<VMValue>>,
}
