mod builder;
mod exprs;
mod instructions;
mod printer;
mod stmts;

pub use builder::MIRBuilder;
pub use instructions::{
    BlockId, CmpOp, ConstantValue, FnId, FuncSig, GlobalId, MIRConv, MIRDollarMode, MIRFn,
    MIRGlobal, MIRInstruction, MIRLinkage, MIRModule, MIROps, MIRStructDecl, MIRTy, MIRTykind,
    MIRValue, MIRVariant, StructId, Terminator, Vreg,
};
