use crate::{
    hir::{HirExpr, HirExprKind, HirTypeNode},
    monomorph::monomorph::Monomorphizer,
    semantics::{InstanceKey, TypeInfo},
};

impl<'a> Monomorphizer<'a> {
    pub fn monomorphize_expr(
        &mut self,
        expr: &mut HirExpr,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
    ) {
        match &mut expr.kind {
            HirExprKind::GenericInstantion { name, type_params } => {
                for tp in &mut type_params.clone() {
                    self.substitute_type(tp, generic_params, concrete_args);
                }

                let evaluated_args = type_params
                    .iter()
                    .map(|tp| {
                        self.ctxt
                            .types
                            .types
                            .get(&tp.hir_id)
                            .expect("Failed to get type info for corresponding id")
                            .clone()
                    })
                    .collect();

                let original_def_id = self
                    .ctxt
                    .names
                    .resolved
                    .get(&expr.hir_id)
                    .expect("Failed to id");

                let search_key = InstanceKey {
                    original_def_id: *original_def_id,
                    concrete_args: evaluated_args,
                };

                if let Some(flat_mangled_name) = self.mangled_mappings.get(&search_key) {
                    *name = flat_mangled_name.clone();
                }
            }
            HirExprKind::Call(callee, args) => {
                self.monomorphize_expr(callee, generic_params, concrete_args);
                for arg in args {
                    self.monomorphize_expr(arg, generic_params, concrete_args);
                }
            }
            HirExprKind::Binary(right, _, left) => {
                self.monomorphize_expr(right, generic_params, concrete_args);
                self.monomorphize_expr(left, generic_params, concrete_args);
            }
            HirExprKind::Unary(_, operand) => {
                self.monomorphize_expr(operand, generic_params, concrete_args)
            }
            _ => (),
        }
    }
}
