mod structures;
mod vm;
#[macro_use]
mod ops;

pub use structures::{Allocation, EvalResultTable, VMValue};
pub use vm::VM;
