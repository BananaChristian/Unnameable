mod structures;
mod vm;
#[macro_use]
mod ops;
mod helpers;

pub use structures::{AllocId, Allocation, EvalResultTable, VMValue, MemoryKind};
pub use vm::VM;
