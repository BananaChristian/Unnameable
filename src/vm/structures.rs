use std::collections::HashMap;

use crate::{
    bc_builder::DollarMode,
    mir::{MIRTy, MIRTykind},
};

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

impl VMValue {
    // Extract signed integer representation
    pub fn as_i128(&self) -> i128 {
        match *self {
            VMValue::I8(v) => v as i128,
            VMValue::U8(v) => v as i128,
            VMValue::I16(v) => v as i128,
            VMValue::U16(v) => v as i128,
            VMValue::I32(v) => v as i128,
            VMValue::U32(v) => v as i128,
            VMValue::I64(v) => v as i128,
            VMValue::U64(v) => v as i128,
            VMValue::Int(v) => v as i128,
            VMValue::UInt(v) => v as i128,
            VMValue::I128(v) => v,
            VMValue::U128(v) => v as i128,
            VMValue::Bool(v) => v as i128,
            _ => panic!("Cannot convert {:?} to integer", self),
        }
    }

    pub fn as_isize(&self) -> Option<isize> {
        match *self {
            VMValue::I8(v) => Some(v as isize),
            VMValue::U8(v) => Some(v as isize),
            VMValue::I16(v) => Some(v as isize),
            VMValue::U16(v) => Some(v as isize),
            VMValue::I32(v) => Some(v as isize),
            VMValue::U32(v) => Some(v as isize),
            VMValue::I64(v) => Some(v as isize),
            VMValue::U64(v) => Some(v as isize),
            VMValue::Int(v) => Some(v),
            VMValue::UInt(v) => Some(v as isize),
            VMValue::I128(v) => Some(v as isize),
            VMValue::U128(v) => Some(v as isize),
            _ => None,
        }
    }

    // Extract unsigned integer representation
    pub fn as_u128(&self) -> u128 {
        match *self {
            VMValue::I8(v) => v as u128,
            VMValue::U8(v) => v as u128,
            VMValue::I16(v) => v as u128,
            VMValue::U16(v) => v as u128,
            VMValue::I32(v) => v as u128,
            VMValue::U32(v) => v as u128,
            VMValue::I64(v) => v as u128,
            VMValue::U64(v) => v as u128,
            VMValue::Int(v) => v as u128,
            VMValue::UInt(v) => v as u128,
            VMValue::I128(v) => v as u128,
            VMValue::U128(v) => v,
            VMValue::Bool(v) => v as u128,
            _ => panic!("Cannot convert {:?} to unsigned integer", self),
        }
    }

    /// Extract float representation
    pub fn as_f64(&self) -> f64 {
        match *self {
            VMValue::F32(v) => v as f64,
            VMValue::F64(v) => v,
            VMValue::I8(v) => v as f64,
            VMValue::U8(v) => v as f64,
            VMValue::I16(v) => v as f64,
            VMValue::U16(v) => v as f64,
            VMValue::I32(v) => v as f64,
            VMValue::U32(v) => v as f64,
            VMValue::I64(v) => v as f64,
            VMValue::U64(v) => v as f64,
            VMValue::Int(v) => v as f64,
            VMValue::UInt(v) => v as f64,
            _ => panic!("Cannot convert {:?} to float", self),
        }
    }

    // Reinterprets the underlying bits of this value as an integer/float of the same width.
    pub fn bitcast_to(&self, target_ty: &MIRTy) -> VMValue {
        match (self, &target_ty.kind) {
            // Same type or no-op
            (val, kind) if self.matches_kind(kind) => val.clone(),

            // 32-bit Transmutations (F32 <-> U32 / I32)
            (VMValue::F32(f), MIRTykind::U32) => VMValue::U32(f.to_bits()),
            (VMValue::F32(f), MIRTykind::I32) => VMValue::I32(f.to_bits() as i32),
            (VMValue::U32(u), MIRTykind::F32) => VMValue::F32(f32::from_bits(*u)),
            (VMValue::I32(i), MIRTykind::F32) => VMValue::F32(f32::from_bits(*i as u32)),
            (VMValue::I32(i), MIRTykind::U32) => VMValue::U32(*i as u32),
            (VMValue::U32(u), MIRTykind::I32) => VMValue::I32(*u as i32),

            // 64-bit Transmutations (F64 <-> U64 / I64)
            (VMValue::F64(f), MIRTykind::U64) => VMValue::U64(f.to_bits()),
            (VMValue::F64(f), MIRTykind::I64) => VMValue::I64(f.to_bits() as i64),
            (VMValue::U64(u), MIRTykind::F64) => VMValue::F64(f64::from_bits(*u)),
            (VMValue::I64(i), MIRTykind::F64) => VMValue::F64(f64::from_bits(*i as u64)),
            (VMValue::I64(i), MIRTykind::U64) => VMValue::U64(*i as u64),
            (VMValue::U64(u), MIRTykind::I64) => VMValue::I64(*u as i64),

            // Pointer reinterpretation (e.g. Ptr <-> USize / U64)
            (VMValue::Ptr(_, offset), MIRTykind::U64) => VMValue::U64(*offset as u64),
            (VMValue::Ptr(_, offset), MIRTykind::USIZE) => VMValue::UInt(*offset),

            _ => panic!("Invalid VM bitcast from {:?} to {:?}", self, target_ty),
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            VMValue::I8(_)
                | VMValue::U8(_)
                | VMValue::I16(_)
                | VMValue::U16(_)
                | VMValue::I32(_)
                | VMValue::U32(_)
                | VMValue::I64(_)
                | VMValue::U64(_)
                | VMValue::I128(_)
                | VMValue::U128(_)
                | VMValue::Int(_)  // represents isize
                | VMValue::UInt(_) // represents usize
        )
    }

    fn matches_kind(&self, kind: &MIRTykind) -> bool {
        matches!(
            (self, kind),
            (VMValue::I8(_), MIRTykind::I8)
                | (VMValue::U8(_), MIRTykind::U8)
                | (VMValue::I32(_), MIRTykind::I32)
                | (VMValue::U32(_), MIRTykind::U32)
                | (VMValue::I64(_), MIRTykind::I64)
                | (VMValue::U64(_), MIRTykind::U64)
                | (VMValue::F32(_), MIRTykind::F32)
                | (VMValue::F64(_), MIRTykind::F64)
                | (VMValue::Ptr(_, _), MIRTykind::Ptr)
        )
    }
}

#[derive(Debug, Clone)]
pub struct Allocation {
    pub data: Vec<VMValue>,
}

pub struct VMMemory {
    pub allocations: HashMap<AllocId, Allocation>,
    pub next_alloc: usize,
}

pub struct VMFrame {
    pub fn_name: String,
    pub mode: DollarMode,
    pub ip: usize, //Instruction pointer
    pub registers: Vec<Option<VMValue>>,
}
