use crate::{
    diagnostics::Span,
    hir::{HirExpr, HirExprKind, HirLiteral},
    semantics::{
        semantics::{ResolvedTypeKind, TypeInfo},
        type_checker::checker::TypeChecker,
    },
};

impl<'a> TypeChecker<'a> {
    pub fn check_expr(&mut self, expr: &HirExpr) -> TypeInfo {
        let ty = match &expr.kind {
            HirExprKind::SizeOf(..) => TypeInfo {
                kind: ResolvedTypeKind::USize,
                span: expr.span.clone(),
            },
            _ => TypeInfo {
                kind: ResolvedTypeKind::Unknown,
                span: expr.span.clone(),
            },
        };
        self.types_table.types.insert(expr.hir_id, ty.clone());
        ty
    }

    fn literal_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Literal(lit) = &expr.kind {
            let ty = match lit {
                HirLiteral::Int8(_) => TypeInfo {
                    kind: ResolvedTypeKind::I8,
                    span: expr.span.clone(),
                },
                HirLiteral::Uint8(_) => TypeInfo {
                    kind: ResolvedTypeKind::U8,
                    span: expr.span.clone(),
                },
                HirLiteral::Int16(_) => TypeInfo {
                    kind: ResolvedTypeKind::I16,
                    span: expr.span.clone(),
                },
                HirLiteral::Uint16(_) => TypeInfo {
                    kind: ResolvedTypeKind::U16,
                    span: expr.span.clone(),
                },
                HirLiteral::Int32(_) => TypeInfo {
                    kind: ResolvedTypeKind::I32,
                    span: expr.span.clone(),
                },
                HirLiteral::Uint32(_) => TypeInfo {
                    kind: ResolvedTypeKind::U32,
                    span: expr.span.clone(),
                },
                HirLiteral::Int64(_) => TypeInfo {
                    kind: ResolvedTypeKind::I64,
                    span: expr.span.clone(),
                },
                HirLiteral::Uint64(_) => TypeInfo {
                    kind: ResolvedTypeKind::U64,
                    span: expr.span.clone(),
                },
                HirLiteral::Int128(_) => TypeInfo {
                    kind: ResolvedTypeKind::I128,
                    span: expr.span.clone(),
                },
                HirLiteral::Uint128(_) => TypeInfo {
                    kind: ResolvedTypeKind::U128,
                    span: expr.span.clone(),
                },
                HirLiteral::IntSize(_) => TypeInfo {
                    kind: ResolvedTypeKind::ISize,
                    span: expr.span.clone(),
                },
                HirLiteral::UintSize(_) => TypeInfo {
                    kind: ResolvedTypeKind::USize,
                    span: expr.span.clone(),
                },
                HirLiteral::Int(_) => TypeInfo {
                    kind: ResolvedTypeKind::USize,
                    span: expr.span.clone(),
                },
                HirLiteral::Float(_) => TypeInfo {
                    kind: ResolvedTypeKind::F32,
                    span: expr.span.clone(),
                },
                HirLiteral::F32(_) => TypeInfo {
                    kind: ResolvedTypeKind::F32,
                    span: expr.span.clone(),
                },
                HirLiteral::F64(_) => TypeInfo {
                    kind: ResolvedTypeKind::F64,
                    span: expr.span.clone(),
                },
                HirLiteral::Bool(_) => TypeInfo {
                    kind: ResolvedTypeKind::Bool,
                    span: expr.span.clone(),
                },
                HirLiteral::Null => TypeInfo {
                    kind: ResolvedTypeKind::Nullable {
                        ty: Box::new(TypeInfo {
                            kind: ResolvedTypeKind::Unknown,
                            span: expr.span.clone(),
                        }),
                    },
                    span: expr.span.clone(),
                },
                HirLiteral::ArrayLiteral(elements) => {
                    self.check_array_literal(elements, expr.span.clone())
                }
            };

            ty
        } else {
            TypeInfo {
                kind: ResolvedTypeKind::Unknown,
                span: expr.span.clone(),
            }
        }
    }

    fn check_array_literal(&mut self, elements: &Vec<HirExpr>, span: Span) -> TypeInfo {
        if elements.is_empty() {
            TypeInfo {
                kind: ResolvedTypeKind::Unknown,
                span: span.clone(),
            }
        } else {
            let first_ty = self.check_expr(&elements[0]);
            let mut all_okay = true;
            for element in elements.iter().skip(1) {
                let element_ty = self.check_expr(element);
                if !self.types_match(&first_ty, &element_ty) {
                    self.report(
                        format!("array elements must all have the same type"),
                        Some(span.clone()),
                    );
                    all_okay = false;
                }
            }

            if all_okay {
                TypeInfo {
                    kind: ResolvedTypeKind::Array {
                        inner: Box::new(first_ty),
                        size: Some(elements.len() as u64),
                    },
                    span,
                }
            } else {
                TypeInfo {
                    kind: ResolvedTypeKind::Unknown,
                    span,
                }
            }
        }
    }
}
