use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    mir::{ConstantValue, GlobalId, MIRInstruction, MIRModule, MIRValue},
    vm::{AllocId, EvalResultTable, VMValue},
};

pub struct Folder<'a> {
    eval_table: &'a EvalResultTable,
    mir_module: &'a mut MIRModule,
    pub corrupted: bool,
    diagnostics: SharedDiagnostics,
}

impl<'a> Folder<'a> {
    pub fn new(
        diagnostics: SharedDiagnostics,
        mir_module: &'a mut MIRModule,
        eval_table: &'a EvalResultTable,
    ) -> Self {
        Folder {
            eval_table,
            mir_module,
            corrupted: false,
            diagnostics,
        }
    }

    pub fn fold(&mut self) {
        for func in self.mir_module.functions.values_mut() {
            for block in func.blocks.values_mut() {
                for inst in &mut block.instructions {
                    if let MIRInstruction::DollarEval { dest, scope_fn, .. } = inst {
                        let vm_val = self.eval_table.results.get(scope_fn);

                        if let Some(val) = vm_val {
                            let const_mir_val =
                                Self::vm_val_to_mir_val(val, &self.eval_table.global_allocs);
                            *inst = MIRInstruction::Assign {
                                dest: dest.clone(),
                                src: const_mir_val,
                            };
                        } else {
                            self.corrupted = true;
                            self.diagnostics.borrow_mut().report(CompilerError::ice(
                                format!(
                                "Dollar scope '{}' was evaluated but missing in EvalResultTable",
                                scope_fn),Phase::MIRBuilder,
                                None,
                            ));
                        }
                    }
                }
            }
        }

        self.mir_module
            .functions
            .retain(|_, func| !func.name.starts_with("$$scope"));
    }

    fn vm_val_to_mir_val(vm_val: &VMValue, global_allocs: &HashMap<AllocId, u32>) -> MIRValue {
        match vm_val {
            VMValue::I8(n) => MIRValue::Constant(ConstantValue::I8(*n)),
            VMValue::U8(n) => MIRValue::Constant(ConstantValue::U8(*n)),
            VMValue::I16(n) => MIRValue::Constant(ConstantValue::I16(*n)),
            VMValue::U16(n) => MIRValue::Constant(ConstantValue::U16(*n)),
            VMValue::I32(n) => MIRValue::Constant(ConstantValue::I32(*n)),
            VMValue::U32(n) => MIRValue::Constant(ConstantValue::U32(*n)),
            VMValue::I64(n) => MIRValue::Constant(ConstantValue::I64(*n)),
            VMValue::U64(n) => MIRValue::Constant(ConstantValue::U64(*n)),
            VMValue::Int(n) => MIRValue::Constant(ConstantValue::Int(*n)),
            VMValue::UInt(n) => MIRValue::Constant(ConstantValue::UInt(*n)),
            VMValue::I128(n) => MIRValue::Constant(ConstantValue::I128(*n)),
            VMValue::U128(n) => MIRValue::Constant(ConstantValue::U128(*n)),
            VMValue::F32(f) => MIRValue::Constant(ConstantValue::F32(*f)),
            VMValue::F64(f) => MIRValue::Constant(ConstantValue::F64(*f)),
            VMValue::Char8(c) => MIRValue::Constant(ConstantValue::Char8(*c)),
            VMValue::Char16(c) => MIRValue::Constant(ConstantValue::Char16(*c)),
            VMValue::Char32(c) => MIRValue::Constant(ConstantValue::Char32(*c)),
            VMValue::Bool(b) => MIRValue::Constant(ConstantValue::Bool(*b)),
            VMValue::Ptr(alloc_id, offset) => match global_allocs.get(alloc_id) {
                Some(&global_id) => MIRValue::Global(GlobalId(global_id as usize)),
                None => MIRValue::Constant(ConstantValue::Ptr(*offset)),
            },
            VMValue::Array(elements) => {
                let mir_elements: Vec<ConstantValue> = elements
                    .iter()
                    .map(|e| match Self::vm_val_to_mir_val(e, global_allocs) {
                        MIRValue::Constant(c) => c,
                        other => {
                            panic!("Array element folded to non-constant MIRValue: {:?}", other)
                        }
                    })
                    .collect();
                MIRValue::Constant(ConstantValue::Array(mir_elements))
            }
            VMValue::Struct {
                name,
                fields,
                struct_id,
            } => {
                let mir_fields: Vec<ConstantValue> = fields
                    .iter()
                    .map(|f| match Self::vm_val_to_mir_val(f, global_allocs) {
                        MIRValue::Constant(c) => c,
                        other => {
                            panic!("Struct field folded to non-constant MIRValue: {:?}", other)
                        }
                    })
                    .collect();

                MIRValue::Constant(ConstantValue::Struct {
                    struct_id: *struct_id,
                    name: name.clone(),
                    fields: mir_fields,
                })
            }
            VMValue::Poison => MIRValue::Poison,
            VMValue::Unit => {
                todo!("Handle pointer/unit folding if dollar scopes return references")
            }
        }
    }
}
