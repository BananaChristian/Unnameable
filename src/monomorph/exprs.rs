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
                for tp in type_params.iter_mut() {
                    self.substitute_type(tp, generic_params, concrete_args);
                }

                let evaluated_args: Vec<TypeInfo> = type_params
                    .iter()
                    .filter_map(|tp| self.type_info_for(tp))
                    .collect();

                if evaluated_args.len() != type_params.len() {
                    // Still abstract (contains an unresolved generic); leave the
                    // instantiation node in place until its args are concrete.
                    return;
                }

                let original_def_id = self
                    .ctxt
                    .names
                    .resolved
                    .get(&expr.hir_id)
                    .expect(&format!(
                        "Name resolver missing mapping for generic instantiation with id {:?}",
                        expr.hir_id
                    ))
                    .clone();

                let search_key = InstanceKey {
                    original_def_id,
                    concrete_args: evaluated_args,
                };

                if let Some(flat_mangled_name) = self.mangled_mappings.get(&search_key) {
                    expr.kind = HirExprKind::Identifier(flat_mangled_name.clone());
                } else if let Some(mangled_name) = self.ensure_instance(&search_key) {
                    expr.kind = HirExprKind::Identifier(mangled_name);
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
            HirExprKind::Unary(_, operand)
            | HirExprKind::Unwrap(operand)
            | HirExprKind::Postfix(operand, _) => {
                self.monomorphize_expr(operand, generic_params, concrete_args, new_name)
            }
            HirExprKind::StaticCast(ty, operand) | HirExprKind::BitCast(ty, operand) => {
                self.substitute_type(ty, generic_params, concrete_args);
                self.monomorphize_type(ty);
                self.monomorphize_expr(operand, generic_params, concrete_args, new_name);
            }
            HirExprKind::SizeOf(ty) => {
                self.substitute_type(ty, generic_params, concrete_args);
                self.monomorphize_type(ty);
            }
            HirExprKind::Index { target, index } => {
                self.monomorphize_expr(target, generic_params, concrete_args, new_name.clone());
                self.monomorphize_expr(index, generic_params, concrete_args, new_name);
            }
            HirExprKind::TupleInst { body } => {
                for e in body {
                    self.monomorphize_expr(e, generic_params, concrete_args, new_name.clone());
                }
            }
            HirExprKind::Instantiation { init_ty, body } => {
                self.substitute_type(init_ty, generic_params, concrete_args);
                self.monomorphize_type(init_ty);
                for field in body {
                    self.monomorphize_expr(
                        &mut field.value,
                        generic_params,
                        concrete_args,
                        new_name.clone(),
                    );
                }
            }
            HirExprKind::DollarScope {
                params,
                body,
                result,
            } => {
                for p in params {
                    self.monomorphize_expr(p, generic_params, concrete_args, new_name.clone());
                }

                for st in body {
                    self.monormophize_stmt(st, generic_params, concrete_args, None);
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
                    .filter_map(|param| self.ctxt.types.types.get(&param.hir_id).cloned())
                    .collect();

                if evaluated_args.len() != type_params.len() {
                    return;
                }

                let Some(original_def_id) = self.ctxt.names.resolved.get(&ty_node.hir_id).cloned()
                else {
                    return;
                };

                let search_key = InstanceKey {
                    original_def_id,
                    concrete_args: evaluated_args,
                };

                if let Some(mangled_name) = self.mangled_mappings.get(&search_key) {
                    ty_node.kind = HirType::CustomType(mangled_name.clone());
                } else if let Some(mangled_name) = self.ensure_instance(&search_key) {
                    ty_node.kind = HirType::CustomType(mangled_name);
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