use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral, HirStmt, HirStmtKind, HirUnaryOp},
    indexer::NodeIndex,
    lowering::NodeId,
    mir::{
        MIRFn, MIRLinkage, MIRModule, MIRStructDecl, MIRVariant,
        instructions::{
            ArmInfo, BasicBlock, BlockId, CmpOp, ConstantValue, EnumId, FnId, GlobalId, MIRBody,
            MIRDollarMode, MIREnum, MIRInstruction, MIROps, MIRParam, MIRTy, MIRTykind, MIRValue,
            StructId, Terminator, VariantId, Vreg,
        },
    },
    semantics::{ResolvedTypeKind, TypeInfo, TypesTable},
    target::TargetSpec,
};

pub struct MIRBuilder<'a> {
    //The imput hir to be convereted to MIR
    indexed_hir: &'a NodeIndex,

    pub types_table: &'a TypesTable,

    pub target_spec: &'a TargetSpec,
    //Counters to increment to track vregs, blocks and globals
    vreg_counter: usize,
    block_counter: usize,
    fn_counter: usize,
    global_counter: usize,
    struct_counter: usize,
    enum_counter: usize,
    variant_counter: usize,
    pub dollar_scope_counter: usize,

    pub current_block_id: Option<BlockId>,
    pub current_func: Option<FnId>,
    pub current_dollar_mode: MIRDollarMode,
    pub current_dollar_name: Option<String>,

    pub enums: HashMap<EnumId, MIREnum>, //Used by the builder in handling enums

    pub struct_name_to_id: HashMap<String, StructId>,
    pub enum_name_to_id: HashMap<String, EnumId>,
    pub variant_name_to_id: HashMap<String, VariantId>,
    pub fn_name_to_id: HashMap<String, FnId>,

    // StructId to (Arm Name -> ArmInfo)
    pub arm_map: HashMap<StructId, HashMap<String, ArmInfo>>,

    var_stack: Vec<HashMap<String, MIRValue>>,
    pub last_value: Option<MIRValue>,

    pub module: MIRModule, //The builder writes to this
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

//Implementation of the core helpers used by the MIR builder
impl<'a> MIRBuilder<'a> {
    pub fn new(
        indexed_hir: &'a NodeIndex,
        types_table: &'a TypesTable,
        target_spec: &'a TargetSpec,
        diagnostics: SharedDiagnostics,
        module_name: String,
    ) -> Self {
        MIRBuilder {
            indexed_hir,
            vreg_counter: 0,
            block_counter: 0,
            fn_counter: 0,
            global_counter: 0,
            struct_counter: 0,
            enum_counter: 0,
            variant_counter: 0,
            dollar_scope_counter: 0,
            current_block_id: None,
            current_func: None,
            current_dollar_mode: MIRDollarMode::None,
            current_dollar_name: None,
            enums: HashMap::new(),
            var_stack: Vec::new(),
            arm_map: HashMap::new(),
            last_value: None,
            diagnostics,
            module: MIRModule {
                name: module_name,
                globals: HashMap::new(),
                structs: HashMap::new(),
                functions: HashMap::new(),
            },
            struct_name_to_id: HashMap::new(),
            enum_name_to_id: HashMap::new(),
            variant_name_to_id: HashMap::new(),
            fn_name_to_id: HashMap::new(),
            types_table,
            target_spec,
            corrupted: false,
        }
    }

    pub fn build_module(&mut self) -> MIRModule {
        let root_ids = self.indexed_hir.roots.clone();

        self.registration_pass(&root_ids);

        for root_id in root_ids {
            if let Some(stmt) = self.indexed_hir.get(&root_id) {
                self.build_stmt(stmt);
            }
        }
        self.module.clone()
    }

    fn registration_pass(&mut self, root_ids: &[NodeId]) {
        for root_id in root_ids {
            if let Some(stmt) = self.indexed_hir.get(root_id).cloned() {
                self.register_stmt(&stmt);
            }
        }
    }

    fn register_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirStructDecl { .. } => {
                self.build_struct(stmt);
            }

            HirStmtKind::HirEnumDecl { .. } => {
                self.build_enum(stmt);
            }

            HirStmtKind::HirVariantDecl { .. } => {
                self.build_variant(stmt);
            }

            HirStmtKind::HirFunctionDecl { .. } => {
                self.build_fn_decl(stmt);
            }

            HirStmtKind::HirFunctionDef { body, .. } => {
                for body_stmt in body {
                    self.register_stmt(body_stmt);
                }
            }

            HirStmtKind::HirVarDecl { init, .. } => {
                self.register_expr(init);
            }

            HirStmtKind::HirIf {
                body,
                else_body,
                condition,
            } => {
                self.register_expr(condition);
                for s in body {
                    self.register_stmt(s);
                }
                if let Some(else_stmts) = else_body {
                    for s in else_stmts {
                        self.register_stmt(s);
                    }
                }
            }

            HirStmtKind::HirWhile { condition, body } => {
                self.register_expr(condition);
                for s in body {
                    self.register_stmt(s);
                }
            }

            HirStmtKind::HirExpr(expr) => {
                self.register_expr(expr);
            }

            _ => {}
        }
    }

    fn register_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::DollarScope { body, result, .. } => {
                for stmt in body {
                    self.register_stmt(&stmt);
                }

                match result {
                    Some(res) => self.register_expr(res),
                    None => (),
                }
            }
            HirExprKind::Binary(left, _, right) => {
                self.register_expr(left);
                self.register_expr(right);
            }
            _ => (),
        }
    }

    fn alloc_vreg(&mut self, name: Option<&str>) -> Vreg {
        match name {
            Some(s) if !s.is_empty() => Vreg::Named(s.to_string()),
            _ => {
                let current = Vreg::Numbered(self.vreg_counter);
                self.vreg_counter += 1;
                current
            }
        }
    }

    pub fn alloc_fn_id(&mut self) -> FnId {
        let current = FnId(self.fn_counter);
        self.fn_counter += 1;
        current
    }

    fn alloc_block_id(&mut self) -> BlockId {
        let current = BlockId(self.block_counter);
        self.block_counter += 1;
        current
    }

    pub fn alloc_global_id(&mut self) -> GlobalId {
        let current = GlobalId(self.global_counter);
        self.global_counter += 1;
        current
    }

    pub fn alloc_struct_id(&mut self) -> StructId {
        let current = StructId(self.struct_counter);
        self.struct_counter += 1;
        current
    }

    pub fn alloc_variant_id(&mut self) -> VariantId {
        let current = VariantId(self.variant_counter);
        self.variant_counter += 1;
        current
    }

    pub fn alloc_enum_id(&mut self) -> EnumId {
        let current = EnumId(self.enum_counter);
        self.enum_counter += 1;
        current
    }

    pub fn new_register(&mut self, reg_ty: MIRTy, name: Option<&str>) -> MIRValue {
        let vreg = self.alloc_vreg(name);
        MIRValue::Register { vreg, ty: reg_ty }
    }

    pub fn build_assign(
        &mut self,
        src: MIRValue,
        ty: MIRTy,
        span: Option<Span>,
        name: Option<&str>,
    ) {
        let dest = self.new_register(ty, name);
        let assign = MIRInstruction::Assign { dest, src };
        self.add_instruction(assign, span);
    }

    pub fn build_dollar_eval(
        &mut self,
        dest: MIRValue,
        scope_fn_name: String,
        args: Vec<MIRValue>,
        span: Option<Span>,
    ) {
        let inst = MIRInstruction::DollarEval {
            dest,
            scope_fn: scope_fn_name,
            args,
        };
        self.add_instruction(inst, span);
    }

    pub fn build_alloca(&mut self, dest: MIRValue, ty: MIRTy, span: Option<Span>) {
        let alloca = MIRInstruction::Alloca {
            dest,
            align: ty.align,
            dollar_mode: self.current_dollar_mode,
            ty,
        };
        self.add_instruction(alloca, span);
    }

    pub fn build_store(&mut self, ptr: MIRValue, val: MIRValue, ty: MIRTy, span: Option<Span>) {
        let alignment = ty.align;

        let store = MIRInstruction::Store {
            ptr,
            align: alignment,
            val,
            ty,
        };
        self.add_instruction(store, span);
    }

    pub fn build_load(&mut self, dest: MIRValue, ptr: MIRValue, ty: MIRTy, span: Option<Span>) {
        let load = MIRInstruction::Load {
            dest,
            ptr,
            align: ty.align,
            ty,
        };
        self.add_instruction(load, span);
    }

    pub fn is_signed(&self, lhs: &MIRValue) -> bool {
        match lhs {
            MIRValue::Register { ty, .. } => matches!(
                ty.kind,
                MIRTykind::I8
                    | MIRTykind::I16
                    | MIRTykind::I32
                    | MIRTykind::I64
                    | MIRTykind::I128
                    | MIRTykind::ISIZE
            ),
            MIRValue::Constant(c) => matches!(
                c,
                ConstantValue::I8(_)
                    | ConstantValue::I16(_)
                    | ConstantValue::I32(_)
                    | ConstantValue::I64(_)
                    | ConstantValue::I128(_)
                    | ConstantValue::Int(_)
            ),
            MIRValue::Global(_) => false,
            MIRValue::FunctionRef(_) => false,
            MIRValue::Poison => false,
        }
    }

    pub fn map_arithmetic_op(&self, op: &HirBinaryOp, lhs: &MIRValue) -> MIROps {
        match op {
            HirBinaryOp::Add => MIROps::Add,
            HirBinaryOp::Sub => MIROps::Sub,
            HirBinaryOp::Mul => MIROps::Mul,
            HirBinaryOp::Mod => MIROps::Mod,
            HirBinaryOp::Div => {
                if self.is_signed(lhs) {
                    MIROps::Sdiv
                } else {
                    MIROps::Udiv
                }
            }
            _ => unreachable!("Not an arithmetic operator"),
        }
    }

    pub fn map_bitwise_op(&mut self, op: &HirBinaryOp, lhs: &MIRValue) -> MIROps {
        match op {
            HirBinaryOp::Xor => MIROps::Xor,
            HirBinaryOp::BitAnd => MIROps::And,
            HirBinaryOp::BitOr => MIROps::Or,
            HirBinaryOp::Shr => {
                if self.is_signed(lhs) {
                    MIROps::Ashr
                } else {
                    MIROps::Shr
                }
            }
            HirBinaryOp::Shl => MIROps::Shl,
            _ => self.report_ice("Not a bitwise operator".to_string(), None),
        }
    }

    pub fn map_cmp_op(&self, op: &HirBinaryOp, lhs: &MIRValue) -> CmpOp {
        let is_float = match lhs {
            MIRValue::Register { ty, .. } => {
                matches!(ty.kind, MIRTykind::F32 | MIRTykind::F64)
            }
            MIRValue::Constant(c) => {
                matches!(c, ConstantValue::F32(_) | ConstantValue::F64(_))
            }
            MIRValue::Global(_) => false,
            MIRValue::FunctionRef(_) => false,
            MIRValue::Poison => false,
        };

        let is_signed = self.is_signed(lhs);

        match op {
            HirBinaryOp::Lt => {
                if is_float {
                    CmpOp::Flt
                } else if is_signed {
                    CmpOp::Slt
                } else {
                    CmpOp::Ult
                }
            }
            HirBinaryOp::Gt => {
                if is_float {
                    CmpOp::Fgt
                } else if is_signed {
                    CmpOp::Sgt
                } else {
                    CmpOp::Ugt
                }
            }
            HirBinaryOp::Leq => {
                if is_float {
                    CmpOp::Fle
                } else if is_signed {
                    CmpOp::Sle
                } else {
                    CmpOp::Ule
                }
            }
            HirBinaryOp::Geq => {
                if is_float {
                    CmpOp::Fge
                } else if is_signed {
                    CmpOp::Sge
                } else {
                    CmpOp::Uge
                }
            }
            HirBinaryOp::Eq => CmpOp::Eq,
            HirBinaryOp::Neq => CmpOp::Neq,
            _ => unreachable!("Not a comparison operator"),
        }
    }

    pub fn make_constant_for_ty(&mut self, ty: &MIRTy, value: isize) -> ConstantValue {
        match &ty.kind {
            MIRTykind::I8 => ConstantValue::I8(value as i8),
            MIRTykind::U8 => ConstantValue::U8(value as u8),
            MIRTykind::I16 => ConstantValue::I16(value as i16),
            MIRTykind::U16 => ConstantValue::U16(value as u16),
            MIRTykind::I32 => ConstantValue::I32(value as i32),
            MIRTykind::U32 => ConstantValue::U32(value as u32),
            MIRTykind::I64 => ConstantValue::I64(value as i64),
            MIRTykind::U64 => ConstantValue::U64(value as u64),
            MIRTykind::I128 => ConstantValue::I128(value as i128),
            MIRTykind::U128 => ConstantValue::U128(value as u128),
            MIRTykind::ISIZE => ConstantValue::Int(value),
            MIRTykind::USIZE => ConstantValue::UInt(value as usize),
            _ => self.report_ice(
                format!(
                    "Enum underlying type '{}' cannot back an enum constant",
                    ty.kind
                ),
                None,
            ),
        }
    }

    pub fn get_corresponding_neg_val(&mut self, ty: &MIRTy) -> MIRValue {
        match ty.kind {
            MIRTykind::I8 => MIRValue::Constant(ConstantValue::I8(-1)),
            MIRTykind::I16 => MIRValue::Constant(ConstantValue::I16(-1)),
            MIRTykind::I32 => MIRValue::Constant(ConstantValue::I32(-1)),
            MIRTykind::I64 => MIRValue::Constant(ConstantValue::I64(-1)),
            MIRTykind::ISIZE => MIRValue::Constant(ConstantValue::Int(-1)),
            MIRTykind::I128 => MIRValue::Constant(ConstantValue::I128(-1)),
            _ => self.report_ice(
                format!("Cannot get negative value for invalid type '{}'", ty),
                None,
            ),
        }
    }

    pub fn build_binary(
        &mut self,
        operator: MIROps,
        lhs: MIRValue,
        rhs: MIRValue,
        ty: MIRTy,
        span: Option<Span>,
    ) {
        let dest = self.new_register(ty, None);
        let bin = MIRInstruction::BinaryOperation {
            dest: dest.clone(),
            op: operator,
            lhs,
            rhs,
        };
        self.add_instruction(bin, span);
        self.last_value = Some(dest)
    }

    fn cast_value(
        &mut self,
        src: MIRValue,
        from_ty: MIRTy,
        to_ty: MIRTy,
        span: Option<Span>,
    ) -> MIRValue {
        let dest = self.new_register(to_ty.clone(), Some("cast"));
        let cast_instr = MIRInstruction::Cast {
            dest: dest.clone(),
            src,
            from_ty,
            to_ty,
        };
        self.add_instruction(cast_instr, span);
        dest
    }

    pub fn build_gep(
        &mut self,
        ptr: MIRValue,
        indices: Vec<MIRValue>, // Changed from single index: MIRValue
        elem_ty: MIRTy,
        span: Option<Span>,
    ) {
        let ptr_ty = self.ptr_type();
        let dest = self.new_register(ptr_ty, None);

        let gep_instr = MIRInstruction::GetElementPtr {
            dest: dest.clone(),
            ptr,
            indices,
            elem_ty,
        };
        self.add_instruction(gep_instr, span);
        self.last_value = Some(dest);
    }

    pub fn build_gep_single(
        &mut self,
        ptr: MIRValue,
        index: MIRValue,
        elem_ty: MIRTy,
        span: Option<Span>,
    ) {
        self.build_gep(ptr, vec![index], elem_ty, span);
    }

    pub fn build_pointer_arithmetic(
        &mut self,
        op: &HirBinaryOp,
        lhs: &HirExpr,
        lhs_val: MIRValue,
        rhs: &HirExpr,
        rhs_val: MIRValue,
        span: Option<Span>,
    ) {
        let left_ty = self.get_type(&lhs.hir_id);
        let right_ty = self.get_type(&rhs.hir_id);

        match op {
            // Pointer Addition (Ptr + Int or Int + Ptr)
            HirBinaryOp::Add => {
                if left_ty.is_pointer() {
                    // ptr + index
                    let elem_ty = self.get_pointed_elem_mir_ty(lhs);
                    self.build_gep_single(lhs_val, rhs_val, elem_ty, span);
                } else if right_ty.is_pointer() {
                    // index + ptr (commutative)
                    let elem_ty = self.get_pointed_elem_mir_ty(rhs);
                    self.build_gep_single(rhs_val, lhs_val, elem_ty, span);
                } else {
                    self.report_ice("Expected at least one pointer in ptr Add".to_string(), span);
                }
            }

            // Pointer Subtraction
            HirBinaryOp::Sub => {
                if left_ty.is_pointer() && right_ty.is_pointer() {
                    // Ptr - Ptr -> Pointer Difference (returns USize)
                    let elem_ty = self.get_pointed_elem_mir_ty(lhs);

                    let usize_ty = MIRTy {
                        kind: MIRTykind::USIZE,
                        size: self.target_spec.int_width,
                        align: self.target_spec.int_width,
                    };

                    let ptr_ty = self.ptr_type();

                    let lhs_int =
                        self.cast_value(lhs_val, ptr_ty.clone(), usize_ty.clone(), span.clone());
                    let rhs_int = self.cast_value(rhs_val, ptr_ty, usize_ty.clone(), span.clone());

                    self.build_binary(
                        MIROps::Sub,
                        lhs_int,
                        rhs_int,
                        usize_ty.clone(),
                        span.clone(),
                    );

                    let byte_diff = self.get_last_val(span.clone());

                    let elem_size_bytes = elem_ty.size;
                    let elem_size_val =
                        MIRValue::Constant(ConstantValue::Int(elem_size_bytes as isize));

                    self.build_binary(MIROps::Udiv, byte_diff, elem_size_val, usize_ty, span);
                } else if left_ty.is_pointer() {
                    // Ptr - Int -> GEP with negative index
                    let elem_ty = self.get_pointed_elem_mir_ty(lhs);
                    self.build_unary(&crate::hir::HirUnaryOp::Neg, right_ty, rhs);
                    let neg_index = self.get_last_val(span.clone());

                    self.build_gep_single(lhs_val, neg_index, elem_ty, span);
                } else {
                    self.report_ice("Invalid operands for pointer Sub".to_string(), span);
                }
            }

            _ => self.report_ice("Unsupported pointer operation".to_string(), span),
        }
    }

    pub fn get_last_val(&mut self, span: Option<Span>) -> MIRValue {
        let Some(last_val) = self.last_value.as_ref() else {
            self.report_ice(format!("Failed to get the last value register"), span)
        };
        last_val.clone()
    }

    pub fn build_cmp(&mut self, cmp_op: CmpOp, lhs: MIRValue, rhs: MIRValue, span: Option<Span>) {
        let dest = self.new_register(
            MIRTy {
                kind: MIRTykind::Bool,
                size: 1,
                align: 1,
            },
            None,
        );
        let cmp = MIRInstruction::Compare {
            dest: dest.clone(),
            op: cmp_op,
            lhs,
            rhs,
        };
        self.add_instruction(cmp, span);
        self.last_value = Some(dest)
    }

    fn get_pointed_elem_mir_ty(&mut self, expr: &HirExpr) -> MIRTy {
        let Some(hir_ty) = self.types_table.types.get(&expr.hir_id) else {
            self.report_ice(
                format!("Failed to get type info for id: {:?}", expr.hir_id),
                Some(expr.span.clone()),
            )
        };

        if let ResolvedTypeKind::Pointer { inner } = &hir_ty.kind {
            let mir_tykind = self.convert_tyinfo_to_mirtykind(inner);
            let size = inner.layout.size;
            let align = inner.layout.alignment;
            MIRTy {
                kind: mir_tykind,
                size,
                align,
            }
        } else {
            self.report_ice(
                "Expected pointer type when lowering GEP".to_string(),
                Some(expr.span.clone()),
            );
        }
    }

    fn convert_tyinfo_to_mirtykind(&mut self, ty_info: &TypeInfo) -> MIRTykind {
        match &ty_info.kind {
            ResolvedTypeKind::I8 => MIRTykind::I8,
            ResolvedTypeKind::U8 => MIRTykind::U8,
            ResolvedTypeKind::I16 => MIRTykind::I16,
            ResolvedTypeKind::U16 => MIRTykind::U16,
            ResolvedTypeKind::I32 => MIRTykind::I32,
            ResolvedTypeKind::U32 => MIRTykind::U32,
            ResolvedTypeKind::I64 => MIRTykind::I64,
            ResolvedTypeKind::U64 => MIRTykind::U64,
            ResolvedTypeKind::I128 => MIRTykind::I128,
            ResolvedTypeKind::U128 => MIRTykind::U128,
            ResolvedTypeKind::USize => MIRTykind::USIZE,
            ResolvedTypeKind::ISize => MIRTykind::ISIZE,
            ResolvedTypeKind::Bool => MIRTykind::Bool,
            ResolvedTypeKind::F32 => MIRTykind::F32,
            ResolvedTypeKind::F64 => MIRTykind::F64,
            ResolvedTypeKind::Char8 => MIRTykind::CHAR8,
            ResolvedTypeKind::Char16 => MIRTykind::CHAR16,
            ResolvedTypeKind::Char32 => MIRTykind::CHAR32,
            ResolvedTypeKind::Struct { name, members, .. } => {
                let struct_id = match self.struct_name_to_id.get(name) {
                    Some(id) => *id,
                    None => {
                        self.report_ice(
                            format!("Failed to get the struct id corresponding to {}", name),
                            Some(ty_info.span.clone()),
                        );
                    }
                };

                let mems: Vec<(String, MIRTy)> = members
                    .iter()
                    .map(|m| {
                        let name = m.0.clone();
                        let ty = self.lower_type_info_to_mir_ty(&m.1);
                        (name, ty)
                    })
                    .collect();

                MIRTykind::Struct(struct_id, name.clone(), mems)
            }
            ResolvedTypeKind::Variant { name, .. } => {
                let struct_id = match self.struct_name_to_id.get(name) {
                    Some(id) => id,
                    None => {
                        self.report_ice(
                            format!(
                                "Failed to get the variant struct id corresponding to '{}'",
                                name
                            ),
                            Some(ty_info.span.clone()),
                        );
                    }
                };
                let Some(struct_decl) = self.module.structs.get(struct_id) else {
                    self.report_ice(
                        format!(
                            "Failed to get variant struct corresponding to id {}",
                            struct_id
                        ),
                        Some(ty_info.span.clone()),
                    )
                };

                MIRTykind::Struct(struct_id.clone(), name.clone(), struct_decl.fields.clone())
            }
            ResolvedTypeKind::Pointer { .. }
            | ResolvedTypeKind::Ref { .. }
            | ResolvedTypeKind::Func { .. } => MIRTykind::Ptr,
            ResolvedTypeKind::Array { inner, size } => {
                let elem_ty_kind = self.convert_tyinfo_to_mirtykind(&inner);
                let elem_align = inner.layout.alignment;
                let elem_size = inner.layout.size;
                let elem_ty = MIRTy {
                    kind: elem_ty_kind,
                    size: elem_size,
                    align: elem_align,
                };
                let arr_size = match size {
                    Some(s) => *s as usize,
                    None => 1,
                };
                MIRTykind::Array(Box::new(elem_ty), arr_size)
            }
            ResolvedTypeKind::Str => MIRTykind::Ptr,
            ResolvedTypeKind::Enum { underlying, .. } => {
                self.convert_tyinfo_to_mirtykind(underlying)
            }
            ResolvedTypeKind::Tuple { fields } => {
                let members: Vec<MIRTy> = fields
                    .iter()
                    .map(|f| self.lower_type_info_to_mir_ty(f))
                    .collect();

                MIRTykind::Tuple(members)
            }
            ResolvedTypeKind::Unit => MIRTykind::Unit,
            _ => self.report_ice(
                format!("Unhandled type '{}'", ty_info.name),
                Some(ty_info.span.clone()),
            ),
        }
    }

    pub fn padding_for(offset: usize, align: usize) -> usize {
        if align == 0 {
            0
        } else {
            (align - (offset % align)) % align
        }
    }

    fn padded_arm_size(&self, payload_tys: &[MIRTy]) -> usize {
        let mut offset = 0;
        for ty in payload_tys {
            offset += Self::padding_for(offset, ty.align);
            offset += ty.size;
        }
        offset
    }

    pub fn convert_variant_to_struct(
        &mut self,
        variant: &MIRVariant,
        ty_info: &TypeInfo,
    ) -> MIRStructDecl {
        let max_payload_size = variant
            .arms
            .iter()
            .map(|arm| self.padded_arm_size(&arm.payload_tys))
            .max()
            .unwrap_or(0);

        let struct_id = self.alloc_struct_id();
        self.struct_name_to_id
            .insert(variant.name.clone(), struct_id);

        let u8_ty = MIRTy {
            kind: MIRTykind::U8,
            size: 1,
            align: 1,
        };

        let payload_arr_ty = MIRTy {
            kind: MIRTykind::Array(Box::new(u8_ty), max_payload_size),
            size: max_payload_size,
            align: ty_info.layout.alignment,
        };

        MIRStructDecl {
            struct_id,
            name: variant.name.clone(),
            fields: vec![
                ("tag".to_string(), variant.discriminant_ty.clone()),
                ("payload".to_string(), payload_arr_ty),
            ],
        }
    }

    pub fn get_type_info(&mut self, id: &NodeId, span: Option<Span>) -> TypeInfo {
        let Some(ty_info) = self.types_table.types.get(id) else {
            self.report_ice(format!("Failed to get from id {:?}", id), span)
        };
        ty_info.clone()
    }

    pub fn get_type(&mut self, id: &NodeId) -> MIRTy {
        let ty_info = self.types_table.types.get(id);

        match ty_info {
            Some(ty) => {
                let kind = self.convert_tyinfo_to_mirtykind(ty);
                let size = ty.layout.size;
                let align = ty.layout.alignment;
                MIRTy { kind, size, align }
            }

            None => self.report_ice(format!("Type for {:?} not found in types table", id), None),
        }
    }

    pub fn ptr_type(&self) -> MIRTy {
        MIRTy {
            kind: MIRTykind::Ptr,
            size: self.target_spec.pointer_width,
            align: self.target_spec.pointer_width,
        }
    }

    pub fn get_or_create_func(
        &mut self,
        name: &str,
        params: &[MIRParam],
        ret_ty: &MIRTy,
        linkage: MIRLinkage,
        body: Option<MIRBody>,
    ) -> FnId {
        if let Some(&existing_id) = self.fn_name_to_id.get(name) {
            let Some(existing) = self.module.functions.get_mut(&existing_id) else {
                self.report_ice(format!("Cannot find function '{}", name), None);
            };

            let mismatch = existing.params.len() != params.len()
                || existing
                    .params
                    .iter()
                    .zip(params.iter())
                    .any(|(a, b)| a.name != b.name || a.ty != b.ty)
                || existing.ret_ty != *ret_ty;

            if mismatch {
                self.report_ice(
                    format!(
                        "Declaration and definition signature mismatch for '{}'",
                        name
                    ),
                    None,
                );
            }

            if existing.body.is_some() && body.is_some() {
                self.report_ice(format!("Duplicate function body for {}", existing_id), None);
            }

            if existing.body.is_none() && body.is_some() {
                existing.body = body;
            }
            return existing_id;
        }

        let id = self.alloc_fn_id();
        let placeholder = MIRFn {
            fn_id: id,
            name: name.to_string(),
            params: params.to_vec(),
            linkage,
            ret_ty: ret_ty.clone(),
            body,
        };
        self.module.functions.insert(id, placeholder);
        self.fn_name_to_id.insert(name.to_string(), id);
        id
    }

    pub fn lower_type_info_to_mir_ty(&mut self, ty_info: &TypeInfo) -> MIRTy {
        let size = ty_info.layout.size;
        let align = ty_info.layout.alignment;
        let kind = self.convert_tyinfo_to_mirtykind(ty_info);

        MIRTy { kind, size, align }
    }

    pub fn get_val_alignment(&self, val: &MIRValue) -> usize {
        match val {
            MIRValue::Register { ty, .. } => ty.align,
            MIRValue::Global(_) | MIRValue::FunctionRef(_) => self.target_spec.pointer_width,
            MIRValue::Constant(c) => match c {
                ConstantValue::I8(_)
                | ConstantValue::U8(_)
                | ConstantValue::Char8(_)
                | ConstantValue::Bool(_) => 1,
                ConstantValue::I16(_) | ConstantValue::U16(_) | ConstantValue::Char16(_) => 2,
                ConstantValue::I32(_)
                | ConstantValue::U32(_)
                | ConstantValue::Char32(_)
                | ConstantValue::F32(_) => 4,
                ConstantValue::I64(_) | ConstantValue::U64(_) | ConstantValue::F64(_) => 8,
                ConstantValue::Int(_) | ConstantValue::UInt(_) | ConstantValue::Ptr(_) => {
                    self.target_spec.pointer_width
                }
                ConstantValue::I128(_) | ConstantValue::U128(_) => 16,
                ConstantValue::Array(elements) => {
                    if let Some(first) = elements.first() {
                        self.get_val_alignment(&MIRValue::Constant(first.clone()))
                    } else {
                        1 // empty array — no real alignment requirement, 1 is a safe default
                    }
                }
                ConstantValue::Struct { fields, .. } | ConstantValue::Tuple(fields) => {
                    fields
                        .iter()
                        .map(|field| self.get_val_alignment(&MIRValue::Constant(field.clone())))
                        .max()
                        .unwrap_or(1) // Empty struct defaults to alignment 1
                }
                ConstantValue::Undef => 0,
            },
            MIRValue::Poison => 0,
        }
    }

    pub fn get_alignment(&mut self, expr: &HirExpr) -> usize {
        if let Some(ty) = self.types_table.types.get(&expr.hir_id) {
            return ty.layout.alignment;
        } else {
            self.report_ice(
                "Failed to get type alignment".to_string(),
                Some(expr.span.clone()),
            );
        }
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Cannot emit instruction, no active function context".to_string(),
                span,
            );
        };

        let Some(block_id) = self.current_block_id else {
            self.report_ice(
                "Cannot emit instruction: no active basic block".to_string(),
                span,
            );
        };

        let Some(func) = self.module.functions.get_mut(&fn_id) else {
            self.report_ice(
                format!("Active function {} not found in module", fn_id),
                span,
            );
        };

        let body = match func.body.as_mut() {
            Some(b) => b,
            None => {
                self.report_ice(
                    format!(
                        "Cannot insert instructions to a function declaration {}",
                        fn_id
                    ),
                    span,
                );
            }
        };

        let block = match body.blocks.get_mut(&block_id) {
            Some(b) => b,
            None => {
                self.report_ice(
                    format!("active block {} not found in function {}", block_id, fn_id),
                    span,
                );
            }
        };

        block.instructions.push(instruction);
    }

    pub fn create_basic_block(&mut self) -> BasicBlock {
        let new_id = self.alloc_block_id();
        BasicBlock {
            id: new_id,
            instructions: Vec::new(),
            terminator: Terminator::Return(None),
        }
    }

    pub fn push_scope(&mut self) {
        self.var_stack.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.var_stack.pop();
    }

    pub fn declare_var(&mut self, name: String, ptr: MIRValue) {
        self.var_stack.last_mut().unwrap().insert(name, ptr);
    }

    pub fn lookup_var(&self, name: &str) -> Option<&MIRValue> {
        for scope in self.var_stack.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    pub fn lookup_ptr(&mut self, expr: &HirExpr) -> MIRValue {
        match &expr.kind {
            HirExprKind::Identifier(name) => match self.lookup_var(name) {
                Some(val) => val.clone(),
                None => self.report_ice(
                    format!("Could not find variable '{}'", name),
                    Some(expr.span.clone()),
                ),
            },
            HirExprKind::Index { target, index } => {
                let target_ty = self.get_type(&target.hir_id);

                let (target_ptr, elem_ty) = match &target_ty.kind {
                    MIRTykind::Array(elem_ty, _) => {
                        // array case: lookup_ptr gives the array's own address, step in directly
                        (self.lookup_ptr(target), elem_ty.as_ref().clone())
                    }
                    MIRTykind::Ptr => {
                        // pointer case: expr_value loads the pointer's VALUE (the address it holds),
                        // then get_pointed_elem_mir_ty tells us what that address points to
                        let ptr_val = self.expr_value(target);
                        let pointee_ty = self.get_pointed_elem_mir_ty(target);

                        let elem_ty = match &pointee_ty.kind {
                            MIRTykind::Array(elem_ty, _) => elem_ty.as_ref().clone(),
                            // pointer directly to a scalar — p[0] treats it as a 1-element array,
                            // C-array-arithmetic style; elem_ty is just the pointee itself
                            _ => pointee_ty.clone(),
                        };
                        (ptr_val, elem_ty)
                    }
                    _ => self.report_ice(
                        "Indexing into a non-array, non-pointer type".to_string(),
                        Some(expr.span.clone()),
                    ),
                };

                let index_val = self.expr_value(index);
                self.build_gep_single(target_ptr, index_val, elem_ty, Some(expr.span.clone()));
                self.get_last_val(Some(expr.span.clone()))
            }
            HirExprKind::Binary(lhs, op, rhs) if matches!(op, HirBinaryOp::Access) => {
                let (field_ptr, _) = self.resolve_field_access(lhs, rhs, Some(expr.span.clone()));
                field_ptr
            }
            HirExprKind::Unary(op, inner) if matches!(op, HirUnaryOp::Dereference) => {
                self.expr_value(inner)
            }
            _ => self.report_ice(
                "Cannot resolve a pointer for this expression".to_string(),
                Some(expr.span.clone()),
            ),
        }
    }

    pub fn resolve_field_access(
        &mut self,
        lhs: &HirExpr,
        rhs: &HirExpr,
        span: Option<Span>,
    ) -> (MIRValue, MIRTy) {
        let struct_ptr = self.lookup_ptr(lhs);
        let lhs_ty = self.get_type(&lhs.hir_id);

        match &lhs_ty.kind {
            MIRTykind::Struct(_, _, fields) => {
                let field_name = match &rhs.kind {
                    HirExprKind::Identifier(name) => name,
                    _ => self.report_ice(
                        "Expected identifier as field name".to_string(),
                        span.clone(),
                    ),
                };
                let field_index = fields
                    .iter()
                    .position(|(name, _)| name == field_name)
                    .expect("Field not found, should have been caught by type checker");
                let field_ty = fields[field_index].1.clone();

                let zero = MIRValue::Constant(ConstantValue::UInt(0));
                let index_val = MIRValue::Constant(ConstantValue::UInt(field_index));
                self.build_gep(
                    struct_ptr,
                    vec![zero, index_val],
                    lhs_ty.clone(),
                    span.clone(),
                );
                let field_ptr = self.get_last_val(span);
                (field_ptr, field_ty)
            }
            MIRTykind::Tuple(elem_tys) => {
                let tuple_index = match &rhs.kind {
                    HirExprKind::Literal(HirLiteral::Int(i)) => *i as usize,
                    _ => self.report_ice(
                        "Expected integer literal as tuple index".to_string(),
                        span.clone(),
                    ),
                };
                let elem_ty = elem_tys[tuple_index].clone();

                let zero = MIRValue::Constant(ConstantValue::UInt(0));
                let index_val = MIRValue::Constant(ConstantValue::UInt(tuple_index));
                self.build_gep(
                    struct_ptr,
                    vec![zero, index_val],
                    lhs_ty.clone(),
                    span.clone(),
                );
                let field_ptr = self.get_last_val(span);
                (field_ptr, elem_ty)
            }
            _ => self.report_ice(
                "Cannot access a field on this type".to_string(),
                span.clone(),
            ),
        }
    }

    pub fn get_struct_decl(&mut self, name: &String, span: Option<Span>) -> MIRStructDecl {
        let Some(struct_id) = self.struct_name_to_id.get(name) else {
            self.report_ice(
                format!("Failed to get struct id corresponding to name '{}'", name),
                span,
            )
        };
        let Some(struct_decl) = self.module.structs.get(struct_id) else {
            self.report_ice(
                format!(
                    "Failed to get struct declaration corresponding to id {}",
                    struct_id
                ),
                span,
            )
        };

        struct_decl.clone()
    }

    pub fn add_block(&mut self, block: &BasicBlock, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Cannot add basic block: no active function context".to_string(),
                span,
            );
        };

        let Some(func) = self.module.functions.get_mut(&fn_id) else {
            self.report_ice(
                format!("Active function {} not found in module", fn_id),
                span,
            );
        };

        let body = match func.body.as_mut() {
            Some(b) => b,
            None => {
                self.report_ice(
                    format!(
                        "Cannot insert instructions to a function declaration {}",
                        fn_id
                    ),
                    span,
                );
            }
        };

        body.blocks.insert(block.id, block.clone());
    }

    pub fn set_terminator(&mut self, terminator: Terminator, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Attempted to set terminator with no active function".to_string(),
                span,
            );
        };

        let Some(block_id) = self.current_block_id else {
            self.report_ice(
                "Attempted to set terminator with no active block".to_string(),
                span,
            );
        };

        let func = match self.module.functions.get_mut(&fn_id) {
            Some(f) => f,
            None => {
                self.report_ice(
                    format!("Active function {} not found in module", fn_id),
                    span,
                );
            }
        };

        let body = match func.body.as_mut() {
            Some(b) => b,
            None => {
                self.report_ice(
                    format!(
                        "Cannot insert instructions to a function declaration {}",
                        fn_id
                    ),
                    span,
                );
            }
        };

        let block = match body.blocks.get_mut(&block_id) {
            Some(b) => b,
            None => {
                self.report_ice(
                    format!("active block {} not found in function {}", block_id, fn_id),
                    span,
                );
            }
        };

        block.terminator = terminator;
    }

    //All errors in the MIR builder are ICE in nature
    pub fn report_ice(&mut self, message: String, span: Option<Span>) -> ! {
        self.corrupted = true;
        let err = CompilerError::ice(message, Phase::MIRBuilder, span);

        self.diagnostics.borrow_mut().report_ice_and_panic(err);
    }
}
