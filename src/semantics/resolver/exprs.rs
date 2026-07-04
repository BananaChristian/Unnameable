use crate::{
    hir::{HirExpr, HirExprKind, HirType, HirTypeNode},
    semantics::{resolver::Resolver, semantics::NameTable},
};

impl<'a> Resolver<'a> {
    pub fn resolve_type(&mut self, ty: &HirTypeNode, table: &mut NameTable) {
        match &ty.kind {
            HirType::CustomType(name) => {
                self.resolve_name(name, ty.hir_id, ty.span.clone(), table);
            }
            HirType::GenericType { name, type_params } => {
                self.resolve_name(name, ty.hir_id, ty.span.clone(), table);
                for gen_ty in type_params {
                    self.resolve_type(gen_ty, table);
                }
            }
            HirType::Ptr(inner) => {
                self.resolve_type(inner, table);
            }
            HirType::Ref(inner) => {
                self.resolve_type(inner, table);
            }
            HirType::Array(inner, ..) => {
                self.resolve_type(inner, table);
            }
            HirType::Func(params, ret_ty) => {
                for param in params {
                    self.resolve_type(param, table);
                }
                self.resolve_type(ret_ty, table);
            }
            HirType::Nullable(ty) => {
                self.resolve_type(ty, table);
            }
            HirType::Failable(ok, err) => {
                self.resolve_type(ok, table);
                self.resolve_type(err, table);
            }
            _ => (),
        }
    }

    pub fn resolve_expr(&mut self, expr: &HirExpr, table: &mut NameTable) {
        match &expr.kind {
            HirExprKind::Identifier(name) => {
                self.resolve_name(name, expr.hir_id, expr.span.clone(), table);
            }
            HirExprKind::Call(callee, args) => {
                self.resolve_expr(callee, table);
                for arg in args {
                    self.resolve_expr(arg, table);
                }
            }
            HirExprKind::GenericInstantion { name, type_params } => {
                self.resolve_name(name, expr.hir_id, expr.span.clone(), table);
                for param in type_params {
                    self.resolve_type(param, table);
                }
            }
            HirExprKind::Unwrap(inner) => {
                self.resolve_expr(inner, table);
            }
            HirExprKind::Unary(.., inner) => {
                self.resolve_expr(inner, table);
            }
            HirExprKind::Postfix(inner, ..) => {
                self.resolve_expr(inner, table);
            }
            HirExprKind::Binary(left, .., right) => {
                self.resolve_expr(left, table);
                self.resolve_expr(right, table);
            }
            HirExprKind::Instantiation { init_ty, body } => {
                if let Some(ty)= init_ty{
                    self.resolve_type(ty,table);
                };
                for field in body {
                    self.resolve_expr(&field.value, table);
                }
            }
            HirExprKind::Index { target, index } => {
                self.resolve_expr(target, table);
                self.resolve_expr(index, table);
            }
            HirExprKind::SizeOf(ty) => {
                self.resolve_type(ty, table);
            }
            HirExprKind::BitCast(ty, exp) => {
                self.resolve_type(ty, table);
                self.resolve_expr(exp, table);
            }
            HirExprKind::StaticCast(ty, exp) => {
                self.resolve_type(ty, table);
                self.resolve_expr(exp, table);
            }
            _ => (),
        }
    }
}
