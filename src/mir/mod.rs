mod builder;
mod exprs;
mod instructions;
mod printer;
mod stmts;

pub use builder::{MIRBuilder, MIRModule};
pub use instructions::{
    BlockId, ConstantValue, MIRDollarMode, MIRFn, MIRGlobal, MIRInstruction, MIRLinkage, MIROps,
    MIRTy, MIRTykind, MIRValue, Terminator, Vreg, CmpOp
};
