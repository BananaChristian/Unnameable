mod structures;
mod vm;
#[macro_use]
mod ops;
mod helpers;

pub use structures::{AllocId, Allocation, EvalResultTable, VMValue};
pub use vm::VM;
