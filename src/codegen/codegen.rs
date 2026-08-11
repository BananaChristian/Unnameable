use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::BasicTypeEnum,
    values::BasicValueEnum,
};

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    mir::{ConstantValue, MIRGlobal, MIRLinkage, MIRModule, MIRTy, MIRTykind, MIRValue, Vreg},
    target::TargetSpec,
};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    target_spec: &'ctx TargetSpec,
    pub vreg_map: HashMap<Vreg, BasicValueEnum<'ctx>>,
    pub corrupted: bool,
    diagnostics: SharedDiagnostics,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        target_spec: &'ctx TargetSpec,
        module_name: &str,
        diagnostics: SharedDiagnostics,
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Codegen {
            context,
            module,
            builder,
            target_spec,
            vreg_map: HashMap::new(),
            corrupted: false,
            diagnostics,
        }
    }

    pub fn compile_module(&mut self, mir_module: &MIRModule) {
        for global in mir_module.globals.values() {
            self.lower_globals(global);
        }

        let mut fn_pairs = Vec::with_capacity(mir_module.functions.len());
        for mir_fn in mir_module.functions.values() {
            let fn_val = self.lower_func(mir_fn);
            fn_pairs.push((mir_fn, fn_val));
        }

        for (mir_fn, fn_val) in fn_pairs {
            if !mir_fn.blocks.is_empty() {
                self.lower_func_body(mir_fn, fn_val);
            }
        }
    }

    fn lower_globals(&mut self, global: &MIRGlobal) {
        let llvm_ty = self.get_llvmty(&global.ty);

        let global_val =
            self.module
                .add_global(llvm_ty, Some(AddressSpace::default()), &global.name);

        match global.linkage {
            MIRLinkage::Public => {} // Default external linkage
            MIRLinkage::Private => global_val.set_linkage(Linkage::Private),
        }
        global_val.set_constant(global.is_const);
        if let MIRValue::Constant(ref const_val) = global.init {
            let init_val = self.lower_constant(const_val);
            global_val.set_initializer(&init_val);
        }
    }

    pub fn get_llvmty(&self, mirty: &MIRTy) -> BasicTypeEnum<'ctx> {
        match mirty.kind {
            MIRTykind::Bool => self.context.bool_type().into(),
            MIRTykind::I8 | MIRTykind::U8 => self.context.i8_type().into(),
            MIRTykind::I16 | MIRTykind::U16 => self.context.i16_type().into(),
            MIRTykind::I32 | MIRTykind::U32 => self.context.i32_type().into(),
            MIRTykind::I64 | MIRTykind::U64 => self.context.i64_type().into(),
            MIRTykind::I128 | MIRTykind::U128 => self.context.custom_width_int_type(128).into(),

            MIRTykind::ISIZE | MIRTykind::USIZE => {
                let int_width = self.target_spec.int_width * 8;
                self.context.custom_width_int_type(int_width as u32).into()
            }

            MIRTykind::F32 => self.context.f32_type().into(),
            MIRTykind::F64 => self.context.f64_type().into(),
            MIRTykind::Unit => self.context.struct_type(&[], false).into(),
            MIRTykind::Ptr => self
                .context
                .ptr_type(inkwell::AddressSpace::default())
                .into(),
        }
    }

    fn lower_constant(&self, constant: &ConstantValue) -> BasicValueEnum<'ctx> {
        match constant {
            ConstantValue::I8(v) => self.context.i8_type().const_int(*v as u64, true).into(),
            ConstantValue::U8(v) => self.context.i8_type().const_int(*v as u64, false).into(),
            ConstantValue::I16(v) => self.context.i16_type().const_int(*v as u64, true).into(),
            ConstantValue::U16(v) => self.context.i16_type().const_int(*v as u64, false).into(),
            ConstantValue::I32(v) => self.context.i32_type().const_int(*v as u64, true).into(),
            ConstantValue::U32(v) => self.context.i32_type().const_int(*v as u64, false).into(),
            ConstantValue::I64(v) => self.context.i64_type().const_int(*v as u64, true).into(),
            ConstantValue::U64(v) => self.context.i64_type().const_int(*v as u64, *v < 0).into(),
            ConstantValue::I128(v) => self
                .context
                .custom_width_int_type(128)
                .const_int(*v as u64, true)
                .into(),
            ConstantValue::U128(v) => self
                .context
                .custom_width_int_type(128)
                .const_int(*v as u64, false)
                .into(),
            ConstantValue::Int(v) => {
                let ptr_bits = self.target_spec.int_width * 8;
                self.context
                    .custom_width_int_type(ptr_bits as u32)
                    .const_int(*v as u64, true)
                    .into()
            }
            ConstantValue::UInt(v) => {
                let ptr_bits = self.target_spec.int_width * 8;
                self.context
                    .custom_width_int_type(ptr_bits as u32)
                    .const_int(*v as u64, false)
                    .into()
            }
            ConstantValue::F32(v) => self.context.f32_type().const_float(*v as f64).into(),
            ConstantValue::F64(v) => self.context.f64_type().const_float(*v).into(),
            ConstantValue::Bool(v) => self.context.bool_type().const_int(*v as u64, false).into(),
        }
    }

    pub fn lower_value(&self, value: &MIRValue) -> BasicValueEnum<'ctx> {
        match value {
            MIRValue::Constant(c) => self.lower_constant(c),
            MIRValue::Register { vreg, .. } => self
                .vreg_map
                .get(vreg)
                .copied()
                .unwrap_or_else(|| panic!("Undefined virtual register: {:?}", vreg)),
            MIRValue::Poison => self.context.i32_type().get_undef().into(),
        }
    }

    pub fn print_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn report_ice(&mut self, message: String, span: Option<Span>) -> ! {
        self.corrupted = true;
        let err = CompilerError::ice(message, Phase::Codegen, span);

        self.diagnostics.borrow_mut().report_ice_and_panic(err);
    }
}
