use std::collections::HashMap;

use crate::bc_builder::DollarMode;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AllocId(pub usize);

pub struct EvalResultTable {
    pub results: HashMap<String, VMValue>,
}

#[derive(Debug, Clone)]
pub enum VMValue {
    Int(i64),
    USize(usize),
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
