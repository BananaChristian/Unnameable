use std::collections::HashMap;

use crate::{
    indexer::NodeIndex,
    mir::instructions::{
        BasicBlock, BlockId, FnId, GlobalId, MIRFn, MIRGlobal, MIRInstruction, MIRValue, Vreg,
    },
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

    //Counters to increment to track vregs and blocks
    vreg_counter: usize,
    block_counter: usize,
    fn_counter: usize,

    //The current block we are writing to
    pub current_block_id: Option<BlockId>,
    pub current_func: Option<FnId>,

    pub module: MIRModule, //The builder writes to this
    pub corrupted: bool,
}

//Implementation of the core helpers used by the MIR builder
impl<'a> MIRBuilder<'a> {
    pub fn new(indexed_hir: &'a NodeIndex, module_name: String) -> Self {
        MIRBuilder {
            indexed_hir,
            vreg_counter: 0,
            block_counter: 0,
            fn_counter: 0,
            current_block_id: None,
            current_func: None,
            module: MIRModule {
                name: module_name,
                globals: HashMap::new(),
                functions: HashMap::new(),
            },
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

    pub fn is_val_reg(&self, operand: &MIRValue) -> bool {
        match operand {
            MIRValue::Register(_) => true,
            MIRValue::Constant(_) => false,
        }
    }

    pub fn is_val_const(&self, operand: &MIRValue) -> bool {
        match operand {
            MIRValue::Register(_) => false,
            MIRValue::Constant(_) => true,
        }
    }

    pub fn new_register(&mut self) -> MIRValue {
        let vreg = self.alloc_vreg();
        let register = MIRValue::Register(vreg);
        register
    }

    pub fn build_assign(&mut self, src: MIRValue) -> MIRInstruction {
        let dest = self.new_register();
        MIRInstruction::Assign { dest, src }
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
            terminator: None,
        }
    }

    pub fn add_block(&mut self, block: &BasicBlock) {
        let fn_id = self
            .current_func
            .expect("Cannot emit outside of a function");

        self.module
            .functions
            .get_mut(&fn_id)
            .expect("Invalid active function")
            .blocks
            .insert(block.id.clone(), block.clone());
    }
}
