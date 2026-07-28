use crate::{
    hir::{HirExpr, HirExprKind, HirType, HirTypeNode},
    monomorph::monomorph::Monomorphizer,
    semantics::{InstanceKey, TypeInfo},
};

impl<'a> Monomorphizer<'a> {
    pub fn monomorphize_expr(
        &mut self,
        expr: &mut HirExpr,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
        new_name: String,
    ) {
        match &mut expr.kind {
            HirExprKind::GenericInstantion { type_params, .. } => {
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
                            .expect(
                                format!(
                                    "Failed to get type info for corresponding id {:?}",
                                    tp.hir_id
                                )
                                .as_str(),
                            )
                            .clone()
                    })
                    .collect();

                let original_def_id = self
                    .ctxt
                    .names
                    .resolved
                    .get(&expr.hir_id)
                    .expect("Failed to get id");

                let search_key = InstanceKey {
                    original_def_id: *original_def_id,
                    concrete_args: evaluated_args,
                };

                if let Some(flat_mangled_name) = self.mangled_mappings.get(&search_key) {
                    expr.kind = HirExprKind::Identifier(flat_mangled_name.clone());
                }
            }
            HirExprKind::Call(callee, args) => {
                self.monomorphize_expr(callee, generic_params, concrete_args, new_name.clone());
                for arg in args {
                    self.monomorphize_expr(arg, generic_params, concrete_args, new_name.clone());
                }
            }
            HirExprKind::Binary(right, _, left) => {
                self.monomorphize_expr(right, generic_params, concrete_args, new_name.clone());
                self.monomorphize_expr(left, generic_params, concrete_args, new_name);
            }
            HirExprKind::Unary(_, operand) => {
                self.monomorphize_expr(operand, generic_params, concrete_args, new_name)
            }
            HirExprKind::DollarScope { body, result } => {
                for st in body {
                    self.monormophize_stmt(st, generic_params, concrete_args, new_name.clone());
                }
                if let Some(res) = result {
                    self.monomorphize_expr(res, generic_params, concrete_args, new_name);
                }
            }
            _ => (),
        }
    }

    pub fn monomorphize_type(&mut self, ty_node: &mut HirTypeNode) {
        match &mut ty_node.kind {
            HirType::GenericType { type_params, .. } => {
                for ty in type_params.iter_mut() {
                    self.monomorphize_type(ty);
                }

                let evaluated_args: Vec<TypeInfo> = type_params
                    .iter()
                    .map(|param| {
                        self.ctxt
                            .types
                            .types
                            .get(&param.hir_id)
                            .expect(
                                format!("Failed to get type info for this id {:?}", param.hir_id)
                                    .as_str(),
                            )
                            .clone()
                    })
                    .collect();

                let original_def_id = self
                    .ctxt
                    .names
                    .resolved
                    .get(&ty_node.hir_id)
                    .expect("Failed to get id");

                let search_key = InstanceKey {
                    original_def_id: *original_def_id,
                    concrete_args: evaluated_args,
                };

                if let Some(mangled_name) = self.mangled_mappings.get(&search_key) {
                    ty_node.kind = HirType::CustomType(mangled_name.clone());
                }
            }
            HirType::Func(params, ret) => {
                for p in params {
                    self.monomorphize_type(p);
                }

                self.monomorphize_type(ret);
            }
            HirType::Ptr(inner)
            | HirType::Ref(inner)
            | HirType::Array(inner, _)
            | HirType::Nullable(inner) => {
                self.monomorphize_type(inner);
            }
            HirType::Tuple(content) => {
                for ty in content {
                    self.monomorphize_type(ty);
                }
            }
            HirType::Failable(ok_ty, err_ty) => {
                self.monomorphize_type(ok_ty);
                self.monomorphize_type(err_ty);
            }
            _ => (),
        }
    }
}
