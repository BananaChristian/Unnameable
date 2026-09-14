use inkwell::types::BasicTypeEnum;

use crate::{
    codegen::{
        Codegen,
        abi::abi::{ABIContract, CoercedCall, LoweredSig, RetKind},
    },
    mir::{FuncSig, MIRTykind, MIRValue},
};

pub struct InternalABI;

impl<'ctx> ABIContract<'ctx> for InternalABI {
    fn lower_signature(&self, codegen: &Codegen<'ctx>, sig: &FuncSig) -> LoweredSig<'ctx> {
        let param_tys: Vec<BasicTypeEnum<'ctx>> = sig
            .params
            .iter()
            .map(|p_ty| codegen.get_llvmty(p_ty))
            .collect();

        let (fn_type, ret_kind) = {
            let ty = codegen.build_fn_type(&sig.ret, &param_tys, false);
            let kind = match sig.ret.kind {
                MIRTykind::Unit => RetKind::Void,
                _ => RetKind::Direct,
            };
            (ty, kind)
        };

        LoweredSig {
            fn_type,
            fn_attrs: vec![],
            param_attrs: vec![],
            ret_kind,
        }
    }

    fn coerce_call(
        &self,
        codegen: &mut Codegen<'ctx>,
        _sig: &FuncSig,
        args: &[MIRValue],
    ) -> CoercedCall<'ctx> {
        CoercedCall {
            args: args.iter().map(|a| codegen.lower_value(a).into()).collect(),
            call_attrs: vec![],
            sret_ptr: None,
        }
    }
}
