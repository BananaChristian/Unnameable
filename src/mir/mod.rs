mod builder;
mod exprs;
mod instructions;
mod printer;
mod stmts;

pub use builder::{MIRBuilder, MIRModule};
pub use instructions::{
    BlockId, CmpOp, ConstantValue, GlobalId, MIRDollarMode, MIRFn, MIRGlobal, MIRInstruction,
    MIRLinkage, MIROps, MIRTy, MIRTykind, MIRValue, Terminator, Vreg,
};
