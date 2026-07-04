use crate::{
    diagnostics::Span,
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral},
    semantics::{
        semantics::{ResolvedTypeKind, TypeInfo},
        type_checker::checker::TypeChecker,
    },
};

impl<'a> TypeChecker<'a> {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        self.expr_type(expr);
    }

    pub fn expr_type(&mut self, expr: &HirExpr) -> TypeInfo {
        let ty = match &expr.kind {
            HirExprKind::SizeOf(_) => {
                TypeInfo::primitive(ResolvedTypeKind::USize, expr.span.clone())
            }
            HirExprKind::Identifier(_) => self.identifier_type(expr),
            HirExprKind::Literal(_) => self.literal_type(expr),
            HirExprKind::Binary(_, _, _) => self.binary_type(expr),
            HirExprKind::StaticCast(_, _) => self.cast_type(expr),
            HirExprKind::BitCast(_, _) => self.bitcast_type(expr),
            _ => TypeInfo::unknown(expr.span.clone()),
        };
        self.insert(expr.hir_id, ty.clone());
        ty
    }

    fn cast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::StaticCast(target, _) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);

            //Will have to apply some casting rules here
            target_ty
        } else {
            TypeInfo::unknown(expr.span.clone())
        }
    }

    fn bitcast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::BitCast(target, _) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);

            //Will have to apply some bitcasting rules here
            target_ty
        } else {
            TypeInfo::unknown(expr.span.clone())
        }
    }

    fn identifier_type(&mut self, expr: &HirExpr) -> TypeInfo {
        let decl_id = match self.name_table.resolved.get(&expr.hir_id) {
            Some(id) => id,
            None => {
                return TypeInfo::unknown(expr.span.clone());
            }
        };

        self.get_decl_type(decl_id)
    }

    fn binary_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Binary(left, op, right) = &expr.kind {
            let left_ty = self.expr_type(left);
            let right_ty = self.expr_type(right);
            match op {
                HirBinaryOp::Add
                | HirBinaryOp::Sub
                | HirBinaryOp::Div
                | HirBinaryOp::Mul
                | HirBinaryOp::Mod => self.arithmetic_type(left_ty, right_ty, expr.span.clone()),
                HirBinaryOp::Eq
                | HirBinaryOp::Neq
                | HirBinaryOp::Lt
                | HirBinaryOp::Gt
                | HirBinaryOp::Geq
                | HirBinaryOp::Leq => TypeInfo::boolean(expr.span.clone()),
                _ => TypeInfo::unknown(expr.span.clone()),
            }
        } else {
            TypeInfo::unknown(expr.span.clone())
        }
    }

    fn arithmetic_type(&mut self, left_ty: TypeInfo, right_ty: TypeInfo, span: Span) -> TypeInfo {
        // both must be numeric
        if !self.is_numeric(&left_ty) {
            self.report(
                "Left operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return TypeInfo::unknown(span.clone());
        }

        if !self.is_numeric(&right_ty) {
            self.report(
                "Right operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return TypeInfo::unknown(span.clone());
        }

        if !self.types_match(&left_ty, &right_ty) {
            self.type_mismatch(&left_ty, &right_ty, span.clone());
            return TypeInfo::unknown(span.clone());
        }

        left_ty
    }

    fn literal_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Literal(lit) = &expr.kind {
            let ty = match lit {
                HirLiteral::Int8(_) => TypeInfo::primitive(ResolvedTypeKind::I8, expr.span.clone()),
                HirLiteral::Uint8(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::U8, expr.span.clone())
                }
                HirLiteral::Int16(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::I16, expr.span.clone())
                }
                HirLiteral::Uint16(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::U16, expr.span.clone())
                }
                HirLiteral::Int32(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::I32, expr.span.clone())
                }
                HirLiteral::Uint32(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::U32, expr.span.clone())
                }
                HirLiteral::Int64(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::I64, expr.span.clone())
                }
                HirLiteral::Uint64(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::U64, expr.span.clone())
                }
                HirLiteral::Int128(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::I128, expr.span.clone())
                }
                HirLiteral::Uint128(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::U128, expr.span.clone())
                }
                HirLiteral::IntSize(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::ISize, expr.span.clone())
                }
                HirLiteral::UintSize(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::USize, expr.span.clone())
                }
                HirLiteral::Int(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::USize, expr.span.clone())
                }
                HirLiteral::Float(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::F32, expr.span.clone())
                }
                HirLiteral::F32(_) => TypeInfo::primitive(ResolvedTypeKind::F32, expr.span.clone()),
                HirLiteral::F64(_) => TypeInfo::primitive(ResolvedTypeKind::F64, expr.span.clone()),
                HirLiteral::Bool(_) => {
                    TypeInfo::primitive(ResolvedTypeKind::Bool, expr.span.clone())
                }
                HirLiteral::Null => TypeInfo::primitive(
                    ResolvedTypeKind::Nullable {
                        ty: Box::new(TypeInfo::unknown(expr.span.clone())),
                    },
                    expr.span.clone(),
                ),
                HirLiteral::ArrayLiteral(elements) => {
                    self.check_array_literal(elements, expr.span.clone())
                }
            };

            ty
        } else {
            TypeInfo::unknown(expr.span.clone())
        }
    }

    fn check_array_literal(&mut self, elements: &Vec<HirExpr>, span: Span) -> TypeInfo {
        if elements.is_empty() {
            TypeInfo::unknown(span)
        } else {
            let first_ty = self.expr_type(&elements[0]);
            let mut all_okay = true;
            for element in elements.iter().skip(1) {
                let element_ty = self.expr_type(element);
                if !self.types_match(&first_ty, &element_ty) {
                    self.report(
                        format!("array elements must all have the same type"),
                        Some(span.clone()),
                    );
                    all_okay = false;
                }
            }

            if all_okay {
                TypeInfo::array(first_ty, Some(elements.len() as u64), span)
            } else {
                TypeInfo::unknown(span)
            }
        }
    }
}
