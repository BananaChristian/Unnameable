use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirBinaryOp, HirExpr, HirExprKind},
    indexer::NodeIndex,
    lowering::NodeId,
    mir::instructions::{
        BasicBlock, BlockId, CmpOp, ConstantValue, DollarMode, FnId, GlobalId, MIRFn, MIRGlobal,
        MIRInstruction, MIROps, MIRTy, MIRTykind, MIRValue, Terminator, Vreg,
    },
    semantics::{ResolvedTypeKind, TypesTable},
    target::TargetSpec,
};

#[derive(Debug, Clone)]
pub struct MIRModule {
    pub name: String, //Module name
    pub globals: HashMap<GlobalId, MIRGlobal>,
    pub functions: HashMap<FnId, MIRFn>,
}

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
    pub dollar_scope_counter: usize,

    pub current_block_id: Option<BlockId>,
    pub current_func: Option<FnId>,
    pub current_dollar_mode: DollarMode,
    pub current_dollar_name: Option<String>,

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
            dollar_scope_counter: 0,
            current_block_id: None,
            current_func: None,
            current_dollar_mode: DollarMode::None,
            current_dollar_name: None,
            var_stack: Vec::new(),
            last_value: None,
            diagnostics,
            module: MIRModule {
                name: module_name,
                globals: HashMap::new(),
                functions: HashMap::new(),
            },
            types_table,
            target_spec,
            corrupted: false,
        }
    }

    pub fn build_module(&mut self) -> MIRModule {
        let root_ids = self.indexed_hir.roots.clone();

        for root_id in root_ids {
            if let Some(stmt) = self.indexed_hir.get(&root_id) {
                self.build_stmt(stmt);
            }
        }
        self.module.clone()
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

    pub fn build_store(&mut self, ptr: MIRValue, val: MIRValue, span: Option<Span>) {
        let store = MIRInstruction::Store {
            ptr,
            align: self.get_val_alignment(&val),
            val,
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

    pub fn map_cmp_op(&self, op: &HirBinaryOp, lhs: &MIRValue) -> CmpOp {
        let is_float = match lhs {
            MIRValue::Register { ty, .. } => {
                matches!(ty.kind, MIRTykind::F32 | MIRTykind::F64)
            }
            MIRValue::Constant(c) => {
                matches!(c, ConstantValue::F32(_) | ConstantValue::F64(_))
            }
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

    pub fn build_cmp(&mut self, cmp_op: CmpOp, lhs: MIRValue, rhs: MIRValue, span: Option<Span>) {
        let dest = self.new_register(
            MIRTy {
                kind: MIRTykind::Bool,
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

    pub fn get_type(&self, id: &NodeId) -> MIRTy {
        let ty_info = self.types_table.types.get(id);

        match ty_info {
            Some(ty) => {
                let kind = match ty.kind {
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
                    _ => todo!("Will map the other types later {:?}", ty),
                };
                let align = ty.layout.alignment;
                MIRTy { kind, align }
            }

            None => todo!("Handle a failed type"),
        }
    }

    pub fn get_val_alignment(&self, val: &MIRValue) -> usize {
        match val {
            MIRValue::Register { ty, .. } => ty.align,
            MIRValue::Constant(c) => match c {
                ConstantValue::I8(_) | ConstantValue::U8(_) | ConstantValue::Bool(_) => 1,
                ConstantValue::I16(_) | ConstantValue::U16(_) => 2,
                ConstantValue::I32(_) | ConstantValue::U32(_) | ConstantValue::F32(_) => 4,
                ConstantValue::I64(_) | ConstantValue::U64(_) | ConstantValue::F64(_) => 8,
                ConstantValue::Int(_) | ConstantValue::UInt(_) => self.target_spec.pointer_width,
                ConstantValue::I128(_) | ConstantValue::U128(_) => 16,
            },
            MIRValue::Poison => 0,
        }
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Cannot emit instruction: no active function context".to_string(),
                span,
            );
            return;
        };

        let Some(block_id) = self.current_block_id else {
            self.report_ice(
                "Cannot emit instruction: no active basic block".to_string(),
                span,
            );
            return;
        };

        let Some(func) = self.module.functions.get_mut(&fn_id) else {
            self.report_ice(
                format!("Active function {:?} not found in module", fn_id),
                span,
            );
            return;
        };

        let Some(block) = func.blocks.get_mut(&block_id) else {
            self.report_ice(
                format!(
                    "Active block {:?} not found in function {:?}",
                    block_id, fn_id
                ),
                span,
            );
            return;
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

    pub fn lookup_ptr(&self, expr: &HirExpr) -> MIRValue {
        if let HirExprKind::Identifier(name) = &expr.kind {
            self.lookup_var(name).cloned().expect("Variable not found")
        } else {
            panic!("Cannot assign to non-identifier")
        }
    }

    pub fn add_block(&mut self, block: &BasicBlock, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Cannot add basic block: no active function context".to_string(),
                span,
            );
            return;
        };

        let Some(func) = self.module.functions.get_mut(&fn_id) else {
            self.report_ice(
                format!("Active function {} not found in module", fn_id),
                span,
            );
            return;
        };

        func.blocks.insert(block.id, block.clone());
    }
    pub fn set_terminator(&mut self, terminator: Terminator, span: Option<Span>) {
        let Some(fn_id) = self.current_func else {
            self.report_ice(
                "Attempted to set terminator with no active function".to_string(),
                span,
            );
            return;
        };

        let Some(block_id) = self.current_block_id else {
            self.report_ice(
                "Attempted to set terminator with no active block".to_string(),
                span,
            );
            return;
        };

        let Some(func) = self.module.functions.get_mut(&fn_id) else {
            self.report_ice(
                format!("Active function {} not found in module", fn_id),
                None,
            );
            return;
        };

        let Some(block) = func.blocks.get_mut(&block_id) else {
            self.report_ice(
                format!("Active block {} not found in function {}", block_id, fn_id),
                None,
            );
            return;
        };

        block.terminator = terminator;
    }

    //All errors in the MIR builder are ICE in nature
    pub fn report_ice(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::ice(message, Phase::MIRBuilder, span));
    }
}
