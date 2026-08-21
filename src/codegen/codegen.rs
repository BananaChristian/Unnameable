use std::collections::HashMap;

use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::BasicTypeEnum,
    values::{BasicValueEnum, FloatValue, GlobalValue, IntValue, PointerValue},
};

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    mir::{
        ConstantValue, GlobalId, MIRGlobal, MIRLinkage, MIRModule, MIRStructDecl, MIRTy, MIRTykind,
        MIRValue, Vreg,
    },
    target::TargetSpec,
};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    target_spec: &'ctx TargetSpec,
    pub vreg_map: HashMap<Vreg, BasicValueEnum<'ctx>>,
    pub global_map: HashMap<GlobalId, GlobalValue<'ctx>>,
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
            global_map: HashMap::new(),
            corrupted: false,
            diagnostics,
        }
    }

    pub fn compile_module(&mut self, mir_module: &MIRModule) {
        for struct_decl in mir_module.structs.values() {
            self.lower_structs(struct_decl);
        }

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

    fn lower_structs(&mut self, struct_decl: &MIRStructDecl) {
        let struct_ty = self.context.opaque_struct_type(&struct_decl.name);

        let field_llvm_tys: Vec<BasicTypeEnum> = struct_decl
            .fields
            .iter()
            .map(|(_, field_ty)| self.get_llvmty(field_ty))
            .collect();
        struct_ty.set_body(&field_llvm_tys, false);
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

        self.global_map.insert(global.global_id.clone(), global_val);
    }

    pub fn get_llvmty(&self, mirty: &MIRTy) -> BasicTypeEnum<'ctx> {
        match &mirty.kind {
            MIRTykind::Bool => self.context.bool_type().into(),
            MIRTykind::I8 | MIRTykind::U8 | MIRTykind::CHAR8 => self.context.i8_type().into(),
            MIRTykind::I16 | MIRTykind::U16 | MIRTykind::CHAR16 => self.context.i16_type().into(),
            MIRTykind::I32 | MIRTykind::U32 | MIRTykind::CHAR32 => self.context.i32_type().into(),
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

            MIRTykind::Struct(_, name, _) => self
                .context
                .get_struct_type(name)
                .expect("Struct type not yet declared, struct decl must be lowered before use")
                .into(),

            MIRTykind::Array(elem_ty, len) => {
                let elem_llvm_ty = self.get_llvmty(&elem_ty);

                match elem_llvm_ty {
                    BasicTypeEnum::IntType(t) => t.array_type(*len as u32).into(),
                    BasicTypeEnum::FloatType(t) => t.array_type(*len as u32).into(),
                    BasicTypeEnum::PointerType(t) => t.array_type(*len as u32).into(),
                    BasicTypeEnum::StructType(t) => t.array_type(*len as u32).into(),
                    BasicTypeEnum::ArrayType(t) => t.array_type(*len as u32).into(),
                    BasicTypeEnum::VectorType(t) => t.array_type(*len as u32).into(),
                }
            }
        }
    }

    fn lower_constant(&mut self, constant: &ConstantValue) -> BasicValueEnum<'ctx> {
        match constant {
            ConstantValue::I8(v) => self.context.i8_type().const_int(*v as u64, true).into(),
            ConstantValue::U8(v) => self.context.i8_type().const_int(*v as u64, false).into(),
            ConstantValue::I16(v) => self.context.i16_type().const_int(*v as u64, true).into(),
            ConstantValue::U16(v) => self.context.i16_type().const_int(*v as u64, false).into(),
            ConstantValue::I32(v) => self.context.i32_type().const_int(*v as u64, true).into(),
            ConstantValue::U32(v) => self.context.i32_type().const_int(*v as u64, false).into(),
            ConstantValue::I64(v) => self.context.i64_type().const_int(*v as u64, true).into(),
            ConstantValue::U64(v) => self.context.i64_type().const_int(*v as u64, false).into(),
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
            ConstantValue::Char8(c) => self.context.i8_type().const_int(*c as u64, false).into(),
            ConstantValue::Char16(c) => self.context.i16_type().const_int(*c as u64, false).into(),
            ConstantValue::Char32(c) => self.context.i32_type().const_int(*c as u64, false).into(),
            ConstantValue::Bool(v) => self.context.bool_type().const_int(*v as u64, false).into(),
            ConstantValue::Ptr(addr) => {
                let ptr_bits = (self.target_spec.int_width * 8) as u32;
                let int_type = self.context.custom_width_int_type(ptr_bits);
                let int_val = int_type.const_int(*addr as u64, false);

                // Casting the integer constant directly into an LLVM pointer constant (inttoptr)
                let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                int_val.const_to_pointer(ptr_type).into()
            }

            ConstantValue::Array(elements) => {
                let elem_values: Vec<BasicValueEnum<'ctx>> =
                    elements.iter().map(|e| self.lower_constant(e)).collect();

                match elem_values[0] {
                    BasicValueEnum::IntValue(_) => {
                        let ints: Vec<IntValue<'ctx>> = elem_values
                            .into_iter()
                            .map(|v| v.into_int_value())
                            .collect();
                        let elem_ty = ints[0].get_type();
                        elem_ty.const_array(&ints).into()
                    }
                    BasicValueEnum::FloatValue(_) => {
                        let floats: Vec<FloatValue<'ctx>> = elem_values
                            .into_iter()
                            .map(|v| v.into_float_value())
                            .collect();
                        let elem_ty = floats[0].get_type();
                        elem_ty.const_array(&floats).into()
                    }
                    BasicValueEnum::PointerValue(_) => {
                        let ptrs: Vec<PointerValue<'ctx>> = elem_values
                            .into_iter()
                            .map(|v| v.into_pointer_value())
                            .collect();
                        let elem_ty = ptrs[0].get_type();
                        elem_ty.const_array(&ptrs).into()
                    }
                    _ => self.report_ice("Unsupported element type in constant array".to_string()),
                }
            }
            ConstantValue::Struct { fields, .. } => {
                let field_values: Vec<BasicValueEnum<'ctx>> =
                    fields.iter().map(|f| self.lower_constant(f)).collect();

                self.context.const_struct(&field_values, false).into()
            }
        }
    }

    pub fn lower_value(&mut self, value: &MIRValue) -> BasicValueEnum<'ctx> {
        match value {
            MIRValue::Constant(c) => self.lower_constant(c),
            MIRValue::Register { vreg, .. } => self
                .vreg_map
                .get(vreg)
                .copied()
                .unwrap_or_else(|| panic!("Undefined virtual register: {:?}", vreg)),
            MIRValue::Global(global_id) => {
                let global_val = self
                    .global_map
                    .get(global_id)
                    .expect("Undefined global variable");
                // Returns an opaque pointer to the global variable in LLVM
                global_val.as_pointer_value().into()
            }
            MIRValue::Poison => self.context.i32_type().get_undef().into(),
        }
    }

    pub fn print_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn report_ice(&mut self, message: String) -> ! {
        self.corrupted = true;
        let err = CompilerError::ice(message, Phase::Codegen, None);

        self.diagnostics.borrow_mut().report_ice_and_panic(err);
    }
}
