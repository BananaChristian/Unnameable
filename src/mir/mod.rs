mod builder;
mod exprs;
mod instructions;
mod printer;
mod stmts;

pub use builder::MIRBuilder;
pub use instructions::{
    BasicBlock, BlockId, CmpOp, ConstantValue, FnId, FuncSig, GlobalId, MIRBody, MIRConv,
    MIRDollarMode, MIRFn, MIRGlobal, MIRInstruction, MIRLinkage, MIRModule, MIROps, MIRStructDecl,
    MIRTy, MIRTykind, MIRValue, MIRVariant, StructId, Terminator, Vreg,
};
