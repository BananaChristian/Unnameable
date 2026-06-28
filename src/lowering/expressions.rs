use crate::{
    ast::{Expr, ExprKind, Literal, Type, TypeKind},
    hir::{HirType, HirTypeNode},
    lowering::lowering::Lowering,
};

impl<'a> Lowering<'a> {
    pub fn lower_type(&mut self, type_node: &Type) -> Option<HirTypeNode> {
        let kind = match &type_node.kind {
            TypeKind::I8 => HirType::I8,
            TypeKind::U8 => HirType::U8,
            TypeKind::I16 => HirType::I16,
            TypeKind::U16 => HirType::U16,
            TypeKind::I32 => HirType::I32,
            TypeKind::U32 => HirType::U32,
            TypeKind::I64 => HirType::I64,
            TypeKind::U64 => HirType::U64,
            TypeKind::I128 => HirType::I128,
            TypeKind::U128 => HirType::U128,
            TypeKind::ISIZE => HirType::ISize,
            TypeKind::USIZE => HirType::USize,
            TypeKind::F32 => HirType::F32,
            TypeKind::F64 => HirType::F64,
            TypeKind::Bool => HirType::Bool,
            TypeKind::Unit => HirType::Unit,

            TypeKind::Ptr(inner) => {
                let inner_hir = self.lower_type(inner)?;
                HirType::Ptr(Box::new(inner_hir.kind))
            }
            TypeKind::Ref(inner) => {
                let inner_hir = self.lower_type(inner)?;
                HirType::Ref(Box::new(inner_hir.kind))
            }
            TypeKind::Nullable(inner) => {
                let inner_hir = self.lower_type(inner)?;
                HirType::Nullable(Box::new(inner_hir.kind))
            }
            TypeKind::Failable(ok, err) => {
                let ok_hir = self.lower_type(ok)?;
                let err_hir = self.lower_type(err)?;
                HirType::Failable(Box::new(ok_hir.kind), Box::new(err_hir.kind))
            }
            TypeKind::Array(inner, size_expr) => {
                let inner_hir = self.lower_type(inner)?;
                let size = match size_expr {
                    Some(expr) => self.eval_const_size(expr),
                    None => None,
                };
                HirType::Array(Box::new(inner_hir.kind), size)
            }
            TypeKind::Func(params, return_type) => {
                let hir_params = params
                    .iter()
                    .map(|p| self.lower_type(p).map(|t| t.kind))
                    .collect::<Option<Vec<_>>>()?;
                let hir_return = match return_type.as_ref() {
                    Some(r) => Some(Box::new(self.lower_type(r)?.kind)),
                    None => None,
                };
                HirType::Func(hir_params, hir_return)
            }
            TypeKind::CustomType(expr) => {
                let name = self.extract_name_string(expr)?;
                HirType::CustomType(name)
            }
            TypeKind::GenericType { name, type_params } => {
                let name_str = self.extract_name_string(name)?;
                let hir_params = type_params
                    .iter()
                    .map(|p| self.lower_type(p).map(|t| t.kind))
                    .collect::<Option<Vec<_>>>()?;
                HirType::GenericType {
                    name: name_str,
                    type_params: hir_params,
                }
            }
            TypeKind::None => {
                self.report("Unresolved type".to_string(), Some(type_node.span.clone()));
                return None;
            }
        };

        Some(HirTypeNode::new(kind, type_node.span.clone()))
    }

    fn eval_const_size(&self, expr: &Expr) -> Option<u64> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => Some(*n as u64),
            ExprKind::Literal(Literal::Uint64(n)) => Some(*n),
            ExprKind::Literal(Literal::Uint32(n)) => Some(*n as u64),
            _ => {
                None
            }
        }
    }

    pub fn extract_name_string(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Identifier(name) => Some(name.clone()),
            ExprKind::Path(left, right) => {
                let left_str = self.extract_name_string(left)?;
                let right_str = self.extract_name_string(right)?;
                Some(format!("{}_{}", left_str, right_str))
            }
            _ => None,
        }
    }
}
