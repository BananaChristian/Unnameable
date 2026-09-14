use crate::{
    ast::{
        BinaryOp, Expr, ExprKind, InstParam, Literal, PostfixOp, Qualifier, QualifierKind, Type,
        TypeKind, UnaryOp,
    },
    diagnostics::Span,
    hir::{
        Conv, HirBinaryOp, HirExpr, HirExprKind, HirInstParam, HirLiteral, HirPostfixOp,
        HirStmtKind, HirType, HirTypeNode, HirUnaryOp, QualifierMap,
    },
    lowering::lowering::Lowering,
};

impl Lowering {
    pub fn lower_expr(&mut self, expr: &Expr) -> Option<HirExpr> {
        let kind = match &expr.kind {
            ExprKind::Literal(lit) => HirExprKind::Literal(self.lower_literal(lit)?),

            ExprKind::Identifier(name) => HirExprKind::Identifier(name.clone()),

            ExprKind::Path(_, _) => {
                let name = self.extract_name_string(expr)?;
                HirExprKind::Identifier(name)
            }

            ExprKind::Binary(left, op, right) => {
                if *op == BinaryOp::Scope {
                    return self.lower_scope_expr(left, right, expr.span.clone());
                }
                if *op == BinaryOp::Access {
                    return self.lower_access_expr(left, right, expr.span.clone());
                }
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
                    .map(|p| self.lower_type(p))
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
                HirExprKind::SizeOf(hir_ty)
            }

            // Struct instantiation
            ExprKind::Instantiation { init_ty, body } => {
                let hir_ty = self.lower_type(init_ty)?;

                let hir_body = body
                    .iter()
                    .map(|p| self.lower_inst_param(p))
                    .collect::<Option<Vec<_>>>()?;

                HirExprKind::Instantiation {
                    init_ty: Box::new(hir_ty),
                    body: hir_body,
                }
            }
            //Tuple instantiation
            ExprKind::TupleInst { body } => {
                let members = body
                    .iter()
                    .map(|a| self.lower_expr(a))
                    .collect::<Option<Vec<_>>>()?;
                HirExprKind::TupleInst { body: members }
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

            ExprKind::StaticCast(ty, ex) => {
                let hir_ty = self.lower_type(ty)?;
                let ep = self.lower_expr(ex)?;
                HirExprKind::StaticCast(Box::new(hir_ty), Box::new(ep))
            }
            ExprKind::BitcastExpr(ty, ex) => {
                let ty = self.lower_type(ty)?;
                let ep = self.lower_expr(ex)?;
                HirExprKind::BitCast(Box::new(ty), Box::new(ep))
            }
            ExprKind::DollarScope { params, body } => {
                let lowered_params: Vec<HirExpr> = params
                    .iter()
                    .map(|p| self.lower_expr(p).expect("Failed to lower identifier"))
                    .collect();

                let mut stmts = self.lower_block(body)?;
                let mut result = None;

                if let Some(last_stmt) = stmts.last() {
                    if let HirStmtKind::HirExpr(expr) = &last_stmt.kind {
                        result = Some(expr.clone());
                    }
                }

                if result.is_some() {
                    stmts.pop();
                }

                HirExprKind::DollarScope {
                    params: lowered_params,
                    result,
                    body: stmts,
                }
            }
        };

        Some(HirExpr::new(self.next_id(), kind, expr.span.clone()))
    }

    fn lower_access_expr(&mut self, left: &Expr, right: &Expr, span: Span) -> Option<HirExpr> {
        let hir_left = self.lower_expr(left)?;

        if let ExprKind::Literal(Literal::Float(val)) = &right.kind {
            if let Some(chained) = self.split_and_lower_float_access(
                hir_left.clone(),
                *val,
                right.span.clone(),
                span.clone(),
            ) {
                return Some(chained);
            }
        }

        let hir_right = self.lower_expr(right)?;

        Some(HirExpr {
            hir_id: self.next_id(),
            kind: HirExprKind::Binary(Box::new(hir_left), HirBinaryOp::Access, Box::new(hir_right)),
            span,
        })
    }

    fn split_and_lower_float_access(
        &mut self,
        hir_base: HirExpr,
        float_val: f64,
        right_span: Span,
        total_span: Span,
    ) -> Option<HirExpr> {
        // Format explicitly to guarantee a decimal point exists even for whole float numbers like 0.0 or 1.0
        let float_str = format!("{:.16}", float_val);
        let parts: Vec<&str> = float_str.split('.').collect();

        if parts.len() != 2 {
            return None;
        }

        let first_idx: isize = parts[0].parse().ok()?;

        // Extract the raw digit immediately after the dot
        let second_digit_char = parts[1].chars().next()?;
        let second_idx: isize = second_digit_char.to_digit(10)? as isize;

        // Recover the span each index digit occupies within the float token.
        // For "0.1" the first index is "0" at (start, start+1) and the second
        // index "1" sits one char past the dot: (start+2, start+3).
        let first_span = Span {
            start: right_span.start,
            end: right_span.start + parts[0].len(),
        };
        let second_span = Span {
            start: right_span.start + parts[0].len() + 1,
            end: right_span.start + parts[0].len() + 2,
        };

        let inner_access = HirExpr {
            hir_id: self.next_id(),
            kind: HirExprKind::Binary(
                Box::new(hir_base.clone()),
                HirBinaryOp::Access,
                Box::new(HirExpr {
                    hir_id: self.next_id(),
                    kind: HirExprKind::Literal(HirLiteral::Int(first_idx)),
                    span: first_span,
                }),
            ),
            span: hir_base.span,
        };

        Some(HirExpr {
            hir_id: self.next_id(),
            kind: HirExprKind::Binary(
                Box::new(inner_access),
                HirBinaryOp::Access,
                Box::new(HirExpr {
                    hir_id: self.next_id(),
                    kind: HirExprKind::Literal(HirLiteral::Int(second_idx)),
                    span: second_span,
                }),
            ),
            span: total_span,
        })
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
                // Scope should never reach here, paths resolved earlier
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
            UnaryOp::BitNot => HirUnaryOp::BitNot,
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

    fn is_generic(&mut self, type_node: &Type) -> bool {
        let current_name = match &type_node.kind {
            TypeKind::CustomType(expr) => self.extract_name_string(expr).unwrap_or_default(),
            _ => return false,
        };

        for ty in self.current_generic_params.clone() {
            let param_name = match &ty.kind {
                TypeKind::CustomType(expr) => self.extract_name_string(expr).unwrap_or_default(),
                _ => continue,
            };

            if current_name == param_name {
                return true;
            }
        }
        false
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
            TypeKind::Str => HirType::Str,
            TypeKind::Char8 => HirType::Char8,
            TypeKind::Char16 => HirType::Char16,
            TypeKind::Char32 => HirType::Char32,
            TypeKind::Bool => HirType::Bool,
            TypeKind::Unit => HirType::Unit,

            TypeKind::Ptr(inner) => {
                let inner_hir = self.lower_type(inner)?;
                HirType::Ptr(Box::new(inner_hir))
            }
            TypeKind::Ref(inner) => {
                let inner_hir = self.lower_type(inner)?;
                HirType::Ref(Box::new(inner_hir))
            }
            TypeKind::Nullable(inner) => {
                let inner_hir = self.lower_type(inner)?;
                // `(i32)?` parses the parenthesized type as a one-element tuple;
                // unwrap it so the nullable wraps the actual type, not a spurious singleton tuple.
                let unwrapped = match &inner_hir.kind {
                    HirType::Tuple(types) if types.len() == 1 => types[0].clone(),
                    _ => inner_hir,
                };
                HirType::Nullable(Box::new(unwrapped))
            }
            TypeKind::Failable(ok, err) => {
                let ok_hir = self.lower_type(ok)?;
                let err_hir = self.lower_type(err)?;
                HirType::Failable(Box::new(ok_hir), Box::new(err_hir))
            }
            TypeKind::Array(inner, size_expr) => {
                let inner_hir = self.lower_type(inner)?;
                let size = match size_expr {
                    Some(expr) => self.eval_const_size(expr),
                    None => None,
                };
                HirType::Array(Box::new(inner_hir), size)
            }
            TypeKind::Func(params, return_type) => {
                let hir_params = params
                    .iter()
                    .map(|p| self.lower_type(p))
                    .collect::<Option<Vec<_>>>()?;
                let hir_return = match return_type.as_ref() {
                    Some(r) => self.lower_type(r)?,
                    None => HirTypeNode::unit(self.next_id(), type_node.span.clone()),
                };
                HirType::Func(hir_params, Box::new(hir_return))
            }
            TypeKind::CustomType(expr) => {
                let name = self.extract_name_string(expr)?;
                if self.is_generic(type_node) {
                    HirType::GenericPlaceHolder(name)
                } else {
                    HirType::CustomType(name)
                }
            }
            TypeKind::GenericType { name, type_params } => {
                let name_str = self.extract_name_string(name)?;
                let hir_params = type_params
                    .iter()
                    .map(|p| self.lower_type(p))
                    .collect::<Option<Vec<_>>>()?;
                HirType::GenericType {
                    name: name_str,
                    type_params: hir_params,
                }
            }
            TypeKind::Tuple(types) => {
                let hir_types = types
                    .iter()
                    .map(|t| self.lower_type(t))
                    .collect::<Option<Vec<_>>>()?;

                HirType::Tuple(hir_types)
            }
            TypeKind::None => {
                return None;
            }
        };

        Some(HirTypeNode::new(
            self.next_id(),
            kind,
            type_node.span.clone(),
        ))
    }

    fn eval_const_size(&self, expr: &Expr) -> Option<u64> {
        match &expr.kind {
            ExprKind::Literal(Literal::Int(n)) => Some(*n as u64),
            ExprKind::Literal(Literal::Uint64(n)) => Some(*n),
            ExprKind::Literal(Literal::Uint32(n)) => Some(*n as u64),
            _ => None,
        }
    }

    pub fn eval_const_int(&self, expr: &Expr) -> Option<isize> {
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

    // Flattens a `::` chain into its constituent expressions. The general expression parser
    // produces `Binary(.., BinaryOp::Scope, ..)` trees for `::`, so resolving a scope chain
    // means walking those bins left-to-right the same way extract_name_string walks Path nodes.
    fn scope_chain<'e>(&self, expr: &'e Expr, acc: &mut Vec<&'e Expr>) {
        if let ExprKind::Binary(left, BinaryOp::Scope, right) = &expr.kind {
            self.scope_chain(left, acc);
            self.scope_chain(right, acc);
        } else {
            acc.push(expr);
        }
    }

    // Name string for a single chain element, plus its type params if it carries any
    // (i.e. it is a generic instantiation, which must survive resolution/monomorphization).
    fn scope_parts(&mut self, expr: &Expr) -> Option<(String, Option<Vec<HirTypeNode>>)> {
        match &expr.kind {
            ExprKind::Identifier(name) => Some((name.clone(), None)),
            ExprKind::Path(left, right) => {
                let (left_str, left_tp) = self.scope_parts(left)?;
                let (right_str, right_tp) = self.scope_parts(right)?;
                Some((format!("{}_{}", left_str, right_str), left_tp.or(right_tp)))
            }
            ExprKind::GenericInstantion { name, type_params } => {
                let (name_str, _) = self.scope_parts(name)?;
                let hir_params = type_params
                    .iter()
                    .map(|p| self.lower_type(p))
                    .collect::<Option<Vec<_>>>()?;
                Some((name_str, Some(hir_params)))
            }
            _ => None,
        }
    }

    // Resolve a `::` chain into a flat mangled identifier. A trailing call names the function
    // being invoked: A::B::make() -> Call(Identifier("A_B_make"), args). If the chain contains
    // a generic instantiation, the type params are carried into a GenericInstantion callee so
    // the resolver/monomorphizer can specialize it.
    fn lower_scope_expr(&mut self, left: &Expr, right: &Expr, span: Span) -> Option<HirExpr> {
        let mut chain = Vec::new();
        self.scope_chain(left, &mut chain);
        self.scope_chain(right, &mut chain);

        let last_is_call = matches!(chain.last().map(|e| &e.kind), Some(ExprKind::Call(..)));

        let name_count = if last_is_call {
            chain.len() - 1
        } else {
            chain.len()
        };

        let mut names = Vec::new();
        let mut type_params: Option<Vec<HirTypeNode>> = None;
        for part in &chain[..name_count] {
            let (part_name, part_tp) = self.scope_parts(part)?;
            names.push(part_name);
            if part_tp.is_some() {
                type_params = part_tp;
            }
        }
        let joined = names.join("_");

        if last_is_call {
            let ExprKind::Call(callee, args) = &chain.last()?.kind else {
                return None;
            };
            let (callee_name, _) = self.scope_parts(callee)?;
            let full = format!("{}_{}", joined, callee_name);
            let hir_args = args
                .iter()
                .map(|a| self.lower_expr(a))
                .collect::<Option<Vec<_>>>()?;
            let callee_kind = match type_params {
                Some(tp) => HirExprKind::GenericInstantion {
                    name: full,
                    type_params: tp,
                },
                None => HirExprKind::Identifier(full),
            };
            let callee_expr = HirExpr::new(self.next_id(), callee_kind, span.clone());
            Some(HirExpr::new(
                self.next_id(),
                HirExprKind::Call(Box::new(callee_expr), hir_args),
                span,
            ))
        } else {
            let hir_kind = match type_params {
                Some(tp) => HirExprKind::GenericInstantion {
                    name: joined,
                    type_params: tp,
                },
                None => HirExprKind::Identifier(joined),
            };
            Some(HirExpr::new(self.next_id(), hir_kind, span))
        }
    }

    pub fn map_qualifiers(&mut self, qualifiers: &[Qualifier]) -> QualifierMap {
        let mut map = QualifierMap::new();
        map.mutable = qualifiers.iter().any(|q| q.kind == QualifierKind::Mut);
        map.constant = qualifiers.iter().any(|q| q.kind == QualifierKind::Const);
        map.dollar_read = qualifiers
            .iter()
            .any(|q| q.kind == QualifierKind::DollarRead);
        map.expose = qualifiers.iter().any(|q| q.kind == QualifierKind::Exposed);

        map.extern_conv = qualifiers.iter().find_map(|q| {
            if let QualifierKind::Extern(abi) = &q.kind {
                let abi_str = match *abi.clone() {
                    Some(e) => match e.kind {
                        ExprKind::Identifier(name) => name,
                        _ => "C".to_string(),
                    },
                    _ => "C".to_string(), // fallback
                };

                if abi_str == "C".to_string() || abi_str == "c".to_string() {
                    Some(Conv::C)
                } else {
                    self.report(
                        format!("Invalid extern convention '{}'", abi_str),
                        Some(q.span.clone()),
                    );
                    None
                }
            } else {
                None
            }
        });

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
            Literal::Str(s) => Some(HirLiteral::Str(s.clone())),
            Literal::Char8(c) => Some(HirLiteral::Char8(*c)),
            Literal::Char16(c) => Some(HirLiteral::Char16(*c)),
            Literal::Char32(c) => Some(HirLiteral::Char32(*c)),

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
