use inkwell::{
    attributes::Attribute,
    types::FunctionType,
    values::{BasicMetadataValueEnum, PointerValue},
};

use crate::{
    codegen::Codegen,
    mir::{FuncSig, MIRValue},
};

pub enum RetKind {
    Direct, //Normal register return
    SRet,   //Hidden pointer in param 0
    Void,   //No return value
}

pub struct LoweredSig<'ctx> {
    pub fn_type: FunctionType<'ctx>,
    pub fn_attrs: Vec<Attribute>,
    /// Attributes to attach to specific parameters (index, attribute)
    /// e.g., (0, "sret") or (2, "byval(24)")
    pub param_attrs: Vec<(u32, Attribute)>,
    pub ret_kind: RetKind,
}

pub struct CoercedCall<'ctx> {
    pub args: Vec<BasicMetadataValueEnum<'ctx>>,
    pub call_attrs: Vec<(u32, Attribute)>,
    pub sret_ptr: Option<PointerValue<'ctx>>,
}

pub trait ABIContract<'ctx> {
    fn lower_signature(&self, codegen: &Codegen<'ctx>, sig: &FuncSig) -> LoweredSig<'ctx>;

    fn coerce_call(
        &self,
        codegen: &mut Codegen<'ctx>,
        sig: &FuncSig,
        args: &[MIRValue],
    ) -> CoercedCall<'ctx>;
}
