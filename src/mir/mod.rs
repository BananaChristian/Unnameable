mod builder;
mod exprs;
mod instructions;
mod printer;
mod stmts;

pub use builder::MIRBuilder;
pub use instructions::{
    BlockId, CmpOp, ConstantValue, GlobalId, MIRDollarMode, MIRFn, MIRGlobal, MIRInstruction,
    MIRLinkage, MIRModule, MIROps, MIRStructDecl, MIRTy, MIRTykind, MIRValue, StructId, Terminator,
    Vreg,
};
