use crate::{
    ast::{
        BinaryOp, Expr, ExprKind, InstParam, Literal, PostfixOp, Qualifier, QualifierKind, Type,
        TypeKind, UnaryOp,
    },
    diagnostics::Span,
    hir::{
        HirBinaryOp, HirExpr, HirExprKind, HirInstParam, HirLiteral, HirPostfixOp, HirType,
        HirTypeNode, HirUnaryOp, QualifierMap,
    },
    lowering::lowering::Lowering,
};

impl<'a> Lowering<'a> {
    pub fn lower_expr(&mut self, expr: &Expr) -> Option<HirExpr> {
        let kind = match &expr.kind {
            ExprKind::Literal(lit) => HirExprKind::Literal(self.lower_literal(lit)?),

            ExprKind::Identifier(name) => HirExprKind::Identifier(name.clone()),

            ExprKind::Path(_, _) => {
                let name = self.extract_name_string(expr)?;
                HirExprKind::Identifier(name)
            }

            ExprKind::Binary(left, op, right) => {
                let hir_left = self.lower_expr(left)?;
                let hir_right = self.lower_expr(right)?;
                let hir_op = self.lower_binary_op(op)?;
                HirExprKind::Binary(Box::new(hir_left), hir_op, Box::new(hir_right))
            }

            ExprKind::Unary(op, operand) => {
                let hir_operand = self.lower_expr(operand)?;
                let hir_op = self.lower_unary_op(op);
                HirExprKind::Unary(hir_op, Box::new(hir_operand))
            }

            ExprKind::Call(callee, args) => {
                let hir_callee = self.lower_expr(callee)?;
                let hir_args = args
                    .iter()
                    .map(|a| self.lower_expr(a))
                    .collect::<Option<Vec<_>>>()?;
                HirExprKind::Call(Box::new(hir_callee), hir_args)
            }

            ExprKind::Unwrap(inner) => {
                let hir_inner = self.lower_expr(inner)?;
                HirExprKind::Unwrap(Box::new(hir_inner))
            }

            ExprKind::GenericInstantion { name, type_params } => {
                let name_str = self.extract_name_string(name)?;
                let hir_params = type_params
                    .iter()
                    .map(|p| self.lower_type(p).map(|t| t.kind))
                    .collect::<Option<Vec<_>>>()?;
                HirExprKind::GenericInstantion {
                    name: name_str,
                    type_params: hir_params,
                }
            }

            ExprKind::Postfix(operand, op) => {
                let hir_operand = self.lower_expr(operand)?;
                let hir_op = self.lower_postfix_op(op);
                HirExprKind::Postfix(Box::new(hir_operand), hir_op)
            }

            // Sizeof
            ExprKind::SizeOfExpr(ty) => {
                let hir_ty = self.lower_type(ty)?;
                HirExprKind::SizeOf(hir_ty.kind)
            }

            // Struct instantiation
            ExprKind::Instantiation { init_ty, body } => {
                let hir_ty = self.lower_type(init_ty)?;
                let hir_body = body
                    .iter()
                    .map(|p| self.lower_inst_param(p))
                    .collect::<Option<Vec<_>>>()?;
                HirExprKind::Instantiation {
                    init_ty: hir_ty.kind,
                    body: hir_body,
                }
            }

            // Index access
            ExprKind::Index { target, index } => {
                let hir_target = self.lower_expr(target)?;
                let hir_index = self.lower_expr(index)?;
                HirExprKind::Index {
                    target: Box::new(hir_target),
                    index: Box::new(hir_index),
                }
            }
        };

        Some(HirExpr::new(self.next_id(), kind, expr.span.clone()))
    }

    fn lower_binary_op(&self, op: &BinaryOp) -> Option<HirBinaryOp> {
        match op {
            BinaryOp::Add => Some(HirBinaryOp::Add),
            BinaryOp::Sub => Some(HirBinaryOp::Sub),
            BinaryOp::Mul => Some(HirBinaryOp::Mul),
            BinaryOp::Div => Some(HirBinaryOp::Div),
            BinaryOp::Mod => Some(HirBinaryOp::Mod),
            BinaryOp::Eq => Some(HirBinaryOp::Eq),
            BinaryOp::Neq => Some(HirBinaryOp::Neq),
            BinaryOp::Lt => Some(HirBinaryOp::Lt),
            BinaryOp::Gt => Some(HirBinaryOp::Gt),
            BinaryOp::Leq => Some(HirBinaryOp::Leq),
            BinaryOp::Geq => Some(HirBinaryOp::Geq),
            BinaryOp::And => Some(HirBinaryOp::And),
            BinaryOp::Or => Some(HirBinaryOp::Or),
            BinaryOp::Coalesce => Some(HirBinaryOp::Coalesce),
            BinaryOp::Shr => Some(HirBinaryOp::Shr),
            BinaryOp::Shl => Some(HirBinaryOp::Shl),
            BinaryOp::Xor => Some(HirBinaryOp::Xor),
            BinaryOp::BitAnd => Some(HirBinaryOp::BitAnd),
            BinaryOp::BitOr => Some(HirBinaryOp::BitOr),
            BinaryOp::AddAssign => Some(HirBinaryOp::AddAssign),
            BinaryOp::SubAssign => Some(HirBinaryOp::SubAssign),
            BinaryOp::DivAssign => Some(HirBinaryOp::DivAssign),
            BinaryOp::ModAssign => Some(HirBinaryOp::ModAssign),
            BinaryOp::MulAssign => Some(HirBinaryOp::MulAssign),
            BinaryOp::Access => Some(HirBinaryOp::Access),
            BinaryOp::Assign => Some(HirBinaryOp::Assign),
            BinaryOp::Scope => {
                // Scope should never reach here — paths resolved earlier
                None
            }
        }
    }

    fn lower_unary_op(&self, op: &UnaryOp) -> HirUnaryOp {
        match op {
            UnaryOp::Neg => HirUnaryOp::Neg,
            UnaryOp::Not => HirUnaryOp::Not,
            UnaryOp::Increment => HirUnaryOp::Increment,
            UnaryOp::Decrement => HirUnaryOp::Decrement,
            UnaryOp::AddressOf => HirUnaryOp::AddressOf,
            UnaryOp::Dereference => HirUnaryOp::Dereference,
        }
    }

    fn lower_postfix_op(&self, op: &PostfixOp) -> HirPostfixOp {
        match op {
            PostfixOp::Increment => HirPostfixOp::Increment,
            PostfixOp::Decrement => HirPostfixOp::Decrement,
            PostfixOp::Propagate => HirPostfixOp::Propagate,
        }
    }

    fn lower_inst_param(&mut self, param: &InstParam) -> Option<HirInstParam> {
        let name = match &param.name.kind {
            ExprKind::Identifier(s) => s.clone(),
            _ => return None,
        };
        let value = self.lower_expr(&param.value)?;
        Some(HirInstParam {
            hir_id: self.next_id(),
            name,
            value: Box::new(value),
            span: param.span.clone(),
        })
    }

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
            _ => None,
        }
    }

    pub fn eval_const_int(&self, expr: &Expr) -> Option<i64> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => Some(*n),
            ExprKind::Unary(UnaryOp::Neg, inner) => {
                self.eval_const_int(&*inner.clone()).map(|n| -n)
            }
            _ => None,
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

    pub fn map_qualifiers(&self, qualifiers: &Vec<Qualifier>) -> QualifierMap {
        let mut map = QualifierMap::new();
        map.mutable = qualifiers.iter().any(|q| q.kind == QualifierKind::Mut);
        map.constant = qualifiers.iter().any(|q| q.kind == QualifierKind::Const);
        map.heap = qualifiers.iter().any(|q| q.kind == QualifierKind::Heap);
        map.expose = qualifiers.iter().any(|q| q.kind == QualifierKind::Exposed);

        map
    }

    fn lower_literal(&mut self, lit: &Literal) -> Option<HirLiteral> {
        match lit {
            Literal::Int8(v) => Some(HirLiteral::Int8(*v)),
            Literal::Uint8(v) => Some(HirLiteral::Uint8(*v)),
            Literal::Int16(v) => Some(HirLiteral::Int16(*v)),
            Literal::Uint16(v) => Some(HirLiteral::Uint16(*v)),
            Literal::Int32(v) => Some(HirLiteral::Int32(*v)),
            Literal::Uint32(v) => Some(HirLiteral::Uint32(*v)),
            Literal::Int64(v) => Some(HirLiteral::Int64(*v)),
            Literal::Uint64(v) => Some(HirLiteral::Uint64(*v)),
            Literal::Int128(v) => Some(HirLiteral::Int128(*v)),
            Literal::Uint128(v) => Some(HirLiteral::Uint128(*v)),
            Literal::IntSize(v) => Some(HirLiteral::IntSize(*v)),
            Literal::UintSize(v) => Some(HirLiteral::UintSize(*v)),
            Literal::Int(v) => Some(HirLiteral::Int(*v)),
            Literal::Float(v) => Some(HirLiteral::Float(*v)),
            Literal::F32(v) => Some(HirLiteral::F32(*v)),
            Literal::F64(v) => Some(HirLiteral::F64(*v)),
            Literal::Bool(v) => Some(HirLiteral::Bool(*v)),
            Literal::Null => Some(HirLiteral::Null),
            Literal::ArrayLiteral(elements) => {
                let hir_elements = elements
                    .iter()
                    .map(|e| self.lower_expr(e))
                    .collect::<Option<Vec<_>>>()?;
                Some(HirLiteral::ArrayLiteral(hir_elements))
            }
        }
    }

    pub fn make_identifier(&mut self, name: &str, span: Span) -> HirExpr {
        HirExpr::new(
            self.next_id(),
            HirExprKind::Identifier(name.to_string()),
            span,
        )
    }

    pub fn make_access(&mut self, target: HirExpr, field: &str, span: Span) -> HirExpr {
        HirExpr::new(
            self.next_id(),
            HirExprKind::Binary(
                Box::new(target),
                HirBinaryOp::Access,
                Box::new(self.make_identifier(field, span.clone())),
            ),
            span.clone(),
        )
    }

    pub fn make_call(&mut self, callee: HirExpr, args: Vec<HirExpr>, span: Span) -> HirExpr {
        HirExpr::new(
            self.next_id(),
            HirExprKind::Call(Box::new(callee), args),
            span,
        )
    }

    pub fn make_method_call(
        &mut self,
        target: HirExpr,
        method: &str,
        args: Vec<HirExpr>,
        span: Span,
    ) -> HirExpr {
        let access = self.make_access(target, method, span.clone());
        self.make_call(access, args, span.clone())
    }

    pub fn make_binary(
        &mut self,
        left: HirExpr,
        op: HirBinaryOp,
        right: HirExpr,
        span: Span,
    ) -> HirExpr {
        HirExpr::new(
            self.next_id(),
            HirExprKind::Binary(Box::new(left), op, Box::new(right)),
            span,
        )
    }
}
