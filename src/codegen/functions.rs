use std::collections::{HashMap, HashSet};

use inkwell::{
    attributes::AttributeLoc, basic_block::BasicBlock, module::Linkage, values::FunctionValue,
};

use crate::{
    codegen::Codegen,
    mir::{BasicBlock as MIRBlock, BlockId, FuncSig, MIRBody, MIRFn, MIRLinkage, Terminator, Vreg},
};

impl<'ctx> Codegen<'ctx> {
    pub fn lower_func(&mut self, func: &MIRFn) -> FunctionValue<'ctx> {
        self.vreg_map.clear();

        let sig = FuncSig {
            conv: func.conv,
            params: func.params.iter().map(|p| p.ty.clone()).collect(),
            ret: func.ret_ty.clone(),
        };
        let contract = self.abi_contract(func.conv);
        let lowered = contract.lower_signature(self, &sig);

        let linkage = match &func.linkage {
            MIRLinkage::Public => None,
            MIRLinkage::Private => Some(Linkage::Internal),
        };

        let fn_val = self
            .module
            .add_function(&func.name, lowered.fn_type, linkage);

        for (idx, attr) in lowered.param_attrs {
            fn_val.add_attribute(AttributeLoc::Param(idx), attr);
        }

        self.func_map.insert(func.fn_id.clone(), fn_val);
        fn_val
    }

    pub fn lower_func_body(&mut self, func: &MIRFn, function: FunctionValue<'ctx>) {
        let body = func
            .body
            .as_ref()
            .expect("lower_func_body called on function declaration");

        let mut bb_map: HashMap<BlockId, BasicBlock<'ctx>> = HashMap::new();

        let entry_bb = self
            .context
            .append_basic_block(function, &format!("bb{}", body.entry_block.0));
        bb_map.insert(body.entry_block, entry_bb);

        for (&block_id, _) in body.blocks.iter() {
            if block_id != body.entry_block {
                let bb = self
                    .context
                    .append_basic_block(function, &format!("bb{}", block_id.0));
                bb_map.insert(block_id, bb);
            }
        }

        for (i, param) in func.params.iter().enumerate() {
            if let Some(param_val) = function.get_nth_param(i as u32) {
                param_val.set_name(param.name.as_str());
                self.vreg_map
                    .insert(Vreg::Named(param.name.clone()), param_val);
            }
        }

        for block_id in Self::compute_block_order(body) {
            let block = body.blocks.get(&block_id).expect("Missing block");
            let llvm_bb = bb_map.get(&block_id).expect("Missing bb");
            self.builder.position_at_end(*llvm_bb);
            for instr in &block.instructions {
                self.lower_instruction(instr, &bb_map);
            }

            self.lower_terminator(&block.terminator, &bb_map);
        }
    }

    fn successors(block: &MIRBlock) -> Vec<BlockId> {
        match &block.terminator {
            Terminator::Goto(target) => vec![*target],
            Terminator::Branch {
                then,
                else_block,
                ..
            } => vec![*then, *else_block],
            Terminator::Return(_) | Terminator::Unreachable => Vec::new(),
        }
    }

    fn dfs_postorder(
        id: BlockId,
        blocks: &HashMap<BlockId, MIRBlock>,
        visited: &mut HashSet<BlockId>,
        post: &mut Vec<BlockId>,
    ) {
        if !visited.insert(id) {
            return;
        }
        for succ in Self::successors(&blocks[&id]) {
            Self::dfs_postorder(succ, blocks, visited, post);
        }
        post.push(id);
    }

    /// Returns the blocks of `body` in definition-before-use (reverse
    /// postorder) order so that lowering a register's producer before its
    /// consumers doesn't depend on HashMap iteration order. Unreachable
    /// blocks are appended afterward (sorted) so they are still emitted.
    fn compute_block_order(body: &MIRBody) -> Vec<BlockId> {
        let mut visited = HashSet::new();
        let mut post = Vec::new();
        Self::dfs_postorder(body.entry_block, &body.blocks, &mut visited, &mut post);

        let mut order: Vec<BlockId> = post.into_iter().rev().collect();

        let mut leftovers: Vec<BlockId> = body
            .blocks
            .keys()
            .filter(|id| !visited.contains(id))
            .cloned()
            .collect();
        leftovers.sort_by_key(|id| id.0);
        order.extend(leftovers);
        order
    }

    fn lower_terminator(
        &mut self,
        terminator: &Terminator,
        bb_map: &HashMap<BlockId, BasicBlock<'ctx>>,
    ) {
        match terminator {
            Terminator::Return(val) => match val {
                Some(v) => {
                    let llvm_val = self.lower_value(v);
                    self.builder.build_return(Some(&llvm_val)).unwrap();
                }
                None => {
                    self.builder.build_return(None).unwrap();
                }
            },
            Terminator::Goto(target_id) => {
                let target_bb = bb_map.get(target_id).expect("Target block missing");
                self.builder.build_unconditional_branch(*target_bb).unwrap();
            }
            Terminator::Branch {
                cond,
                then,
                else_block,
            } => {
                let cond_val = self.lower_value(cond).into_int_value();
                let then_bb = bb_map.get(then).expect("Then block missing");
                let else_bb = bb_map.get(else_block).expect("Else block missing");
                self.builder
                    .build_conditional_branch(cond_val, *then_bb, *else_bb)
                    .unwrap();
            }
            Terminator::Unreachable => {
                self.builder.build_unreachable().unwrap();
            }
        }
    }
}
