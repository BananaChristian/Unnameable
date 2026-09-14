use inkwell::attributes::Attribute;
use inkwell::types::{AnyTypeEnum, BasicTypeEnum};
use inkwell::{AddressSpace, types::BasicType};

use crate::{
    codegen::{
        Codegen,
        abi::abi::{ABIContract, CoercedCall, LoweredSig, RetKind},
    },
    mir::{FuncSig, MIRTy, MIRTykind, MIRValue},
};

pub struct SysV64;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Class {
    Integer,
    Sse,
    Memory,
}

fn any_type(ty: BasicTypeEnum) -> AnyTypeEnum {
    match ty {
        BasicTypeEnum::ArrayType(t) => t.into(),
        BasicTypeEnum::FloatType(t) => t.into(),
        BasicTypeEnum::IntType(t) => t.into(),
        BasicTypeEnum::PointerType(t) => t.into(),
        BasicTypeEnum::StructType(t) => t.into(),
        BasicTypeEnum::VectorType(t) => t.into(),
    }
}

impl SysV64 {
    fn classify(ty: &MIRTy) -> Class {
        match &ty.kind {
            MIRTykind::Bool
            | MIRTykind::I8
            | MIRTykind::U8
            | MIRTykind::I16
            | MIRTykind::U16
            | MIRTykind::I32
            | MIRTykind::U32
            | MIRTykind::I64
            | MIRTykind::U64
            | MIRTykind::ISIZE
            | MIRTykind::USIZE
            | MIRTykind::I128
            | MIRTykind::U128
            | MIRTykind::Ptr
            | MIRTykind::CHAR8
            | MIRTykind::CHAR16
            | MIRTykind::CHAR32 => Class::Integer,

            MIRTykind::F32 | MIRTykind::F64 => Class::Sse,

            MIRTykind::Unit => Class::Integer,

            MIRTykind::Struct(_, _, _) | MIRTykind::Array(_, _) | MIRTykind::Tuple(_) => {
                if ty.size == 0 || ty.size > 16 {
                    Class::Memory
                } else {
                    Self::classify_aggregate(ty)
                }
            }
        }
    }

    fn classify_aggregate(ty: &MIRTy) -> Class {
        fn is_homogeneous_float(ty: &MIRTy) -> bool {
            match &ty.kind {
                MIRTykind::F32 | MIRTykind::F64 => true,
                MIRTykind::Struct(_, _, fields) => {
                    fields.iter().all(|(_, fty)| is_homogeneous_float(fty))
                }
                MIRTykind::Tuple(elem_tys) => elem_tys.iter().all(|ety| is_homogeneous_float(ety)),
                MIRTykind::Array(elem_ty, _) => is_homogeneous_float(elem_ty),
                _ => false,
            }
        }

        if is_homogeneous_float(ty) {
            Class::Sse
        } else {
            Class::Integer
        }
    }
}

impl<'ctx> ABIContract<'ctx> for SysV64 {
    fn lower_signature(&self, codegen: &Codegen<'ctx>, sig: &FuncSig) -> LoweredSig<'ctx> {
        let ctx = codegen.context;
        let mut param_tys = Vec::new();
        let mut param_attrs = Vec::new();
        let ret_class = Self::classify(&sig.ret);
        let (llvm_ret, has_sret) = if ret_class == Class::Memory && sig.ret.size > 0 {
            let ptr_ty = ctx.ptr_type(AddressSpace::default());
            param_tys.push(ptr_ty.into());

            let sret_kind = Attribute::get_named_enum_kind_id("sret");
            let ret_llvm_ty = codegen.get_llvmty(&sig.ret);
            let sret_attr = ctx.create_type_attribute(sret_kind, any_type(ret_llvm_ty));
            param_attrs.push((0, sret_attr));

            (None, true)
        } else {
            (Some(codegen.get_llvmty(&sig.ret)), false)
        };

        let mut param_idx = if has_sret { 1 } else { 0 };
        for param_ty in &sig.params {
            let class = Self::classify(param_ty);
            if class == Class::Memory && param_ty.size > 0 {
                let ptr_ty = ctx.ptr_type(AddressSpace::default());
                param_tys.push(ptr_ty.into());

                let byval_kind = Attribute::get_named_enum_kind_id("byval");
                let param_llvm_ty = codegen.get_llvmty(param_ty);
                let byval_attr = ctx.create_type_attribute(byval_kind, any_type(param_llvm_ty));
                param_attrs.push((param_idx, byval_attr));
            } else {
                param_tys.push(codegen.get_llvmty(param_ty).into());
            }
            param_idx += 1;
        }

        let fn_type = if has_sret {
            ctx.void_type().fn_type(&param_tys, false)
        } else {
            match &sig.ret.kind {
                MIRTykind::Unit => ctx.void_type().fn_type(&param_tys, false),
                _ => {
                    let llvm_ret = codegen.get_llvmty(&sig.ret);
                    llvm_ret.fn_type(&param_tys, false)
                }
            }
        };

        LoweredSig {
            fn_type,
            fn_attrs: vec![],
            param_attrs,
            ret_kind: if has_sret {
                RetKind::SRet
            } else if llvm_ret.is_none() {
                RetKind::Void
            } else {
                RetKind::Direct
            },
        }
    }

    fn coerce_call(
        &self,
        codegen: &mut Codegen<'ctx>,
        sig: &FuncSig,
        args: &[MIRValue],
    ) -> CoercedCall<'ctx> {
        let ctx = codegen.context;
        let mut llvm_args = Vec::new();
        let mut call_attrs = Vec::new();

        let mut sret_ptr = None;
        let ret_class = Self::classify(&sig.ret);
        if ret_class == Class::Memory && sig.ret.size > 0 {
            let ret_ty = codegen.get_llvmty(&sig.ret);
            let ptr = codegen.builder.build_alloca(ret_ty, "sret").unwrap();
            sret_ptr = Some(ptr);
            llvm_args.push(ptr.into());

            let sret_kind = Attribute::get_named_enum_kind_id("sret");
            let sret_attr = ctx.create_type_attribute(sret_kind, any_type(ret_ty));
            call_attrs.push((0, sret_attr));
        }

        let mut param_idx = if sret_ptr.is_some() { 1 } else { 0 };
        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
            let class = Self::classify(param_ty);
            if class == Class::Memory && param_ty.size > 0 {
                let ptr = match arg {
                    MIRValue::Global(id) => codegen.global_map.get(id).unwrap().as_pointer_value(),
                    MIRValue::Register { .. } => {
                        let val = codegen.lower_value(arg);
                        let alloca = codegen
                            .builder
                            .build_alloca(val.get_type(), "byval")
                            .unwrap();
                        codegen.builder.build_store(alloca, val).unwrap();
                        alloca
                    }
                    MIRValue::Constant(c) => {
                        let val = codegen.lower_constant(c);
                        let alloca = codegen
                            .builder
                            .build_alloca(val.get_type(), "byval")
                            .unwrap();
                        codegen.builder.build_store(alloca, val).unwrap();
                        alloca
                    }
                    other => panic!("Cannot pass {:?} as byval argument", other),
                };
                llvm_args.push(ptr.into());

                let byval_kind = Attribute::get_named_enum_kind_id("byval");
                let param_llvm_ty = codegen.get_llvmty(param_ty);
                let byval_attr = ctx.create_type_attribute(byval_kind, any_type(param_llvm_ty));
                call_attrs.push((param_idx, byval_attr));
            } else {
                llvm_args.push(codegen.lower_value(arg).into());
            }
            param_idx += 1;
        }

        CoercedCall {
            args: llvm_args,
            call_attrs,
            sret_ptr,
        }
    }
}
