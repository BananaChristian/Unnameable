use std::collections::HashMap;

use crate::{
    hir::HirBinaryOp,
    indexer::NodeIndex,
    lowering::NodeId,
    mir::instructions::{
        BasicBlock, BlockId, ConstantValue, FnId, GlobalId, MIRFn, MIRGlobal, MIRInstruction,
        MIROps, MIRTy, MIRTykind, MIRValue, Terminator, Vreg,
    },
    semantics::{ResolvedTypeKind, TypesTable}, target::TargetSpec,
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

    //The current block we are writing to
    pub current_block_id: Option<BlockId>,
    pub current_func: Option<FnId>,

    pub module: MIRModule, //The builder writes to this
    pub corrupted: bool,
}

//Implementation of the core helpers used by the MIR builder
impl<'a> MIRBuilder<'a> {
    pub fn new(
        indexed_hir: &'a NodeIndex,
        types_table: &'a TypesTable,
        target_spec: &'a TargetSpec,
        module_name: String
    ) -> Self {
        MIRBuilder {
            indexed_hir,
            vreg_counter: 0,
            block_counter: 0,
            fn_counter: 0,
            global_counter: 0,
            current_block_id: None,
            current_func: None,
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

    fn alloc_vreg(&mut self) -> Vreg {
        let current = Vreg(self.vreg_counter);
        self.vreg_counter += 1;
        current
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

    pub fn new_register(&mut self, reg_ty: MIRTy) -> MIRValue {
        let vreg = self.alloc_vreg();
        let register = MIRValue::Register { vreg, ty: reg_ty };
        register
    }

    pub fn build_assign(&mut self, src: MIRValue, ty: MIRTy) -> MIRInstruction {
        let dest = self.new_register(ty);
        MIRInstruction::Assign { dest, src }
    }

    pub fn build_alloca(&mut self, dest: MIRValue, ty: MIRTy) -> MIRInstruction {
        MIRInstruction::Alloca {
            dest,
            align: ty.align,
            ty,
        }
    }

    pub fn build_store(&self, ptr: MIRValue, val: MIRValue) -> MIRInstruction {
        MIRInstruction::Store {
            ptr,
            align: self.get_val_alignment(&val),
            val,
        }
    }

    //Will expand it to handle even for sdiv and crap like that
    pub fn map_binary_operator(&self, op: &HirBinaryOp) -> MIROps {
        match op {
            HirBinaryOp::Add => MIROps::Add,
            HirBinaryOp::Sub => MIROps::Sub,
            HirBinaryOp::Mul => MIROps::Mul,
            HirBinaryOp::Mod => MIROps::Mod,
            _ => todo!("Map other binary operators"),
        }
    }

    pub fn build_binary(
        &mut self,
        operator: MIROps,
        lhs: MIRValue,
        rhs: MIRValue,
        ty: MIRTy,
    ) -> MIRInstruction {
        let dest = self.new_register(ty);
        MIRInstruction::BinaryOperation {
            dest,
            op: operator,
            lhs,
            rhs,
        }
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
                    _ => todo!("Will map the rest later"),
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
        }
    }

    pub fn add_instruction(&mut self, instruction: MIRInstruction) {
        let fn_id = self
            .current_func
            .expect("Cannot add an instruction outside a function");

        let block_id = self
            .current_block_id
            .as_mut()
            .expect("Cannot emit without an active block");

        self.module
            .functions
            .get_mut(&fn_id)
            .expect("Invalid active function")
            .blocks
            .get_mut(&block_id)
            .expect("Invalid active block")
            .instructions
            .push(instruction);
    }

    pub fn create_basic_block(&mut self) -> BasicBlock {
        let new_id = self.alloc_block_id();
        BasicBlock {
            id: new_id,
            instructions: Vec::new(), //For now empty
            terminator: Terminator::Return(None),
        }
    }

    pub fn add_block(&mut self, block: &BasicBlock) {
        let fn_id = self
            .current_func
            .expect("Cannot add a basic block outside of a function");

        self.module
            .functions
            .get_mut(&fn_id)
            .expect("Invalid active function")
            .blocks
            .insert(block.id.clone(), block.clone());
    }

    pub fn set_terminator(&mut self, terminator: Terminator) {
        let fn_id = self.current_func.expect("No active function");
        let block_id = self.current_block_id.expect("No active block");

        self.module
            .functions
            .get_mut(&fn_id)
            .unwrap()
            .blocks
            .get_mut(&block_id)
            .unwrap()
            .terminator = terminator;
    }
}
