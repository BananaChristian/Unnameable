use crate::{
    diagnostics::Span,
    hir::{
        HirBinaryOp, HirExpr, HirExprKind, HirInstParam, HirLiteral, HirPattern, HirPostfixOp,
        HirStmt, HirStmtKind, HirUnaryOp,
    },
    lowering::NodeId,
    semantics::{
        semantics::{InstanceKey, ResolvedTypeKind, TypeInfo},
        type_checker::checker::TypeChecker,
    },
};

impl<'a> TypeChecker<'a> {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        self.expr_type(expr);
    }

    pub fn expr_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let Some(ty) = self.ctxt.types.types.get(&expr.hir_id) {
            return ty.clone();
        }

        let ty = match &expr.kind {
            HirExprKind::SizeOf(_) => self.primitive(ResolvedTypeKind::USize, expr.span.clone()),
            HirExprKind::Identifier(_) => {
                self.look_up_declared_type(expr.hir_id, expr.span.clone())
            }
            HirExprKind::Call(_, _) => self.call_type(expr),
            HirExprKind::Literal(_) => self.literal_type(expr),
            HirExprKind::Binary(_, _, _) => self.binary_type(expr),
            HirExprKind::StaticCast(_, _) => self.cast_type(expr),
            HirExprKind::BitCast(_, _) => self.bitcast_type(expr),
            HirExprKind::Index { .. } => self.index_type(expr),
            HirExprKind::Unary(_, _) => self.unary_type(expr),
            HirExprKind::Postfix(_, _) => self.postfix_type(expr),
            HirExprKind::GenericInstantion { .. } => self.gen_inst_type(expr),
            HirExprKind::Instantiation { .. } => self.struct_init_type(expr),
            HirExprKind::TupleInst { .. } => self.tuple_init_type(expr),
            HirExprKind::Match { .. } => self.match_type(expr),
            HirExprKind::Block(body) => self.block_type(body, expr.span.clone()),
            HirExprKind::DollarScope {
                params,
                body,
                result,
            } => {
                for p in params {
                    self.check_expr(p);
                }
                for st in body {
                    self.check_stmt(st);
                }
                //A dollar scope takes the type of the last expression if not its of
                //unit type
                match result {
                    Some(res) => self.expr_type(res),
                    None => self.unit(expr.span.clone()),
                }
            }
            _ => self.unknown(expr.span.clone()),
        };
        self.insert(expr.hir_id, ty.clone());
        ty
    }

    fn handle_init_params(&mut self, struct_ty: &TypeInfo, init_p: &HirInstParam) {
        let ResolvedTypeKind::Struct { members, .. } = &struct_ty.kind else {
            self.report(
                format!(
                    "Expected struct type for initialization parameter but got '{}'",
                    struct_ty.name
                ),
                None,
            );
            return;
        };

        let p_ty = members
            .iter()
            .find(|(name, _, _)| name == &init_p.name)
            .map(|(_, ty, _)| ty.clone());

        let Some(p_ty) = p_ty else {
            self.report(
                format!("Unknown field '{}' in struct initialization", init_p.name),
                Some(init_p.span.clone()),
            );
            return;
        };

        self.expr_type(&init_p.value);
        self.coerce_ty(&p_ty, &init_p.value);
        let init_ty = self.expr_type(&init_p.value);
        if !TypeInfo::types_match(&p_ty, &init_ty) {
            self.type_mismatch(&p_ty, &init_ty, init_p.span.clone());
        }

        self.insert(init_p.hir_id, p_ty);
    }

    fn struct_init_type(&mut self, expr: &HirExpr) -> TypeInfo {
        let HirExprKind::Instantiation { init_ty, body } = &expr.kind else {
            return self.unknown(expr.span.clone());
        };

        let ty = self.type_from_hir_type(init_ty);

        for field in body {
            self.handle_init_params(&ty, field);
        }

        ty
    }

    fn tuple_init_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::TupleInst { body } = &expr.kind {
            let field_tys: Vec<TypeInfo> = body.iter().map(|m| self.expr_type(m)).collect();
            let tuple_inst_ty = self.tuple(field_tys, expr.span.clone());
            self.insert(expr.hir_id, tuple_inst_ty.clone());
            tuple_inst_ty
        } else {
            self.unknown(expr.span.clone())
        }
    }

    /// Types a `match` expression: the scrutinee, each arm's pattern (binding
    /// the pattern names to the scrutinee's parts), guards (must be `bool`),
    /// and bodies (all of which must agree on a single type, which becomes the
    /// match's type). Variant and enum matches must be exhaustive unless a
    /// wildcard arm is present.
    fn match_type(&mut self, expr: &HirExpr) -> TypeInfo {
        let HirExprKind::Match { scrutinee, arms } = &expr.kind else {
            return self.unknown(expr.span.clone());
        };

        let scrutinee_ty = self.expr_type(scrutinee);

        let mut covered: Vec<String> = Vec::new();
        let mut exhaustive = false;
        let mut result_ty: Option<TypeInfo> = None;

        for arm in arms {
            match &arm.pattern {
                HirPattern::Wildcard => exhaustive = true,
                HirPattern::Path { member, .. } => covered.push(member.clone()),
                _ => {}
            }

            let pattern_span = match &arm.pattern {
                HirPattern::Literal(inner) => inner.span.clone(),
                HirPattern::Path { span, .. } => span.clone(),
                HirPattern::Binding { span, .. } => span.clone(),
                HirPattern::Tuple { span, .. } => span.clone(),
                HirPattern::StructPattern { span, .. } => span.clone(),
                HirPattern::Wildcard | HirPattern::Or(_) => arm.span.clone(),
            };

            self.check_pattern(&arm.pattern, &scrutinee_ty, pattern_span);

            if let Some(guard) = &arm.guard {
                let guard_ty = self.expr_type(guard);
                let bool_ty = self.boolean(guard.span.clone());
                if !TypeInfo::types_match(&bool_ty, &guard_ty) {
                    self.type_mismatch(&bool_ty, &guard_ty, guard.span.clone());
                }
            }

            let body_ty = self.expr_type(&arm.body);
            match &result_ty {
                None => result_ty = Some(body_ty),
                Some(prev) => {
                    if !TypeInfo::types_match(prev, &body_ty) {
                        self.type_mismatch(prev, &body_ty, arm.body.span.clone());
                    }
                }
            }
        }

        if !exhaustive {
            match &scrutinee_ty.kind {
                ResolvedTypeKind::Enum { name, members, .. } => {
                    let all: Vec<String> = members.iter().map(|m| m.0.clone()).collect();
                    let missing: Vec<String> = all
                        .iter()
                        .filter(|m| !covered.contains(m))
                        .cloned()
                        .collect();
                    if !missing.is_empty() {
                        self.report(
                            format!(
                                "Non-exhaustive match on enum '{}' missing case{}: {}",
                                name,
                                if missing.len() == 1 { "" } else { "s" },
                                missing.join(", ")
                            ),
                            Some(expr.span.clone()),
                        );
                    }
                }
                ResolvedTypeKind::Variant { name, arms, .. } => {
                    let all: Vec<String> = arms.iter().map(|a| a.0.clone()).collect();
                    let missing: Vec<String> = all
                        .iter()
                        .filter(|m| !covered.contains(m))
                        .cloned()
                        .collect();
                    if !missing.is_empty() {
                        self.report(
                            format!(
                                "Non-exhaustive match on variant '{}' missing case{}: {}",
                                name,
                                if missing.len() == 1 { "" } else { "s" },
                                missing.join(", ")
                            ),
                            Some(expr.span.clone()),
                        );
                    }
                }
                _ => {}
            }
        }

        match result_ty {
            Some(ty) => ty,
            None => self.unit(expr.span.clone()),
        }
    }

    /// A block expression's type is the type of its trailing tail expression
    /// statement, or `()` when it has none. A `;`-terminated trailing
    /// expression is an ordinary statement and yields `()`.
    fn block_type(&mut self, body: &Vec<HirStmt>, span: Span) -> TypeInfo {
        match body.last() {
            Some(HirStmt {
                kind: HirStmtKind::HirTailExpr(inner),
                ..
            }) => {
                for stmt in &body[..body.len() - 1] {
                    self.check_stmt(stmt);
                }
                self.expr_type(inner)
            }
            _ => {
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.unit(span)
            }
        }
    }

    /// Validates a pattern against the type of the value it matches against,
    /// recording the expected type for every binding it introduces so uses of
    /// the bound names resolve to the right type.
    fn check_pattern(&mut self, pattern: &HirPattern, expected: &TypeInfo, span: Span) {
        match pattern {
            HirPattern::Wildcard => {}
            HirPattern::Binding { hir_id, .. } => {
                self.insert(*hir_id, expected.clone());
            }
            HirPattern::Literal(lit_expr) => {
                self.expr_type(lit_expr);
                self.coerce_ty(expected, lit_expr);
                let coerced_lit_ty = self.expr_type(lit_expr);
                if !self.pattern_literal_matches(expected, &coerced_lit_ty) {
                    self.type_mismatch(expected, &coerced_lit_ty, span);
                }
            }
            HirPattern::Path {
                type_name,
                member,
                payloads,
                ..
            } => match &expected.kind {
                ResolvedTypeKind::Enum { name, members, .. } => {
                    if members.iter().any(|m| &m.0 == member) {
                        if !payloads.is_empty() {
                            self.report(
                                format!(
                                    "Enum member '{}.{}' does not take payload patterns",
                                    name, member
                                ),
                                Some(span.clone()),
                            );
                        }
                    } else {
                        self.unknown_member(member, name, span.clone());
                    }
                }
                ResolvedTypeKind::Variant { name, arms, .. } => {
                    if let Some(arm_tuple) = arms.iter().find(|a| &a.0 == member) {
                        let expected_payloads = &arm_tuple.3;
                        if payloads.len() != expected_payloads.len() {
                            self.report(
                                format!(
                                    "Variant arm '{}.{}' expects {} payload patterns, but got {}",
                                    name,
                                    member,
                                    expected_payloads.len(),
                                    payloads.len()
                                ),
                                Some(span.clone()),
                            );
                        } else {
                            for (payload, payload_ty) in payloads.iter().zip(expected_payloads) {
                                self.check_pattern(payload, payload_ty, span.clone());
                            }
                        }
                    } else {
                        self.unknown_member(member, name, span.clone());
                    }
                }
                _ => {
                    self.report(
                        format!(
                            "Path pattern '{}.{}' cannot match a value of type '{}'",
                            type_name, member, expected.name
                        ),
                        Some(span),
                    );
                }
            },
            HirPattern::Tuple { elements, .. } => match &expected.kind {
                ResolvedTypeKind::Tuple { fields } => {
                    if elements.len() != fields.len() {
                        self.report(
                            format!(
                                "Tuple pattern arity mismatch: expected {} elements, but got {}",
                                fields.len(),
                                elements.len()
                            ),
                            Some(span.clone()),
                        );
                    } else {
                        for (element, field_ty) in elements.iter().zip(fields) {
                            self.check_pattern(element, field_ty, span.clone());
                        }
                    }
                }
                _ => {
                    self.report(
                        format!(
                            "Tuple pattern cannot match a value of type '{}'",
                            expected.name
                        ),
                        Some(span),
                    );
                }
            },
            HirPattern::StructPattern {
                type_name, fields, ..
            } => match &expected.kind {
                ResolvedTypeKind::Struct { name, members, .. } => {
                    for field in fields {
                        match members.iter().find(|f| &f.0 == &field.name) {
                            Some((_, field_ty, _)) => {
                                self.check_pattern(&field.pattern, field_ty, field.span.clone());
                            }
                            None => self.unknown_member(&field.name, name, field.span.clone()),
                        }
                    }
                }
                _ => {
                    self.report(
                        format!(
                            "Struct pattern '.{}' cannot match a value of type '{}'",
                            type_name, expected.name
                        ),
                        Some(span),
                    );
                }
            },
            HirPattern::Or(alts) => {
                for alt in alts {
                    if matches!(alt, HirPattern::Binding { .. }) {
                        self.report(
                            "Bindings are not allowed inside 'or' patterns".to_string(),
                            Some(span.clone()),
                        );
                    }
                    self.check_pattern(alt, expected, span.clone());
                }
            }
        }
    }

    fn pattern_literal_matches(&self, expected: &TypeInfo, lit_ty: &TypeInfo) -> bool {
        TypeInfo::types_match(expected, lit_ty)
            || (self.is_numeric(expected) && self.is_numeric(lit_ty))
    }

    fn gen_inst_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::GenericInstantion { type_params, .. } = &expr.kind {
            let template_decl_id: NodeId = *self.ctxt.names.resolved.get(&expr.hir_id).expect(
                format!(
                    "Name resolver missing mapping for generic instantiation with id {:?}",
                    expr.hir_id
                )
                .as_str(),
            );

            let template_info = self.get_decl_type(&template_decl_id, expr.span.clone());

            let concrete_args: Vec<TypeInfo> = type_params
                .iter()
                .map(|param_node| self.type_from_hir_type(param_node))
                .collect();

            if !concrete_args.is_empty()
                && !concrete_args.iter().any(|a| {
                    matches!(
                        a.kind,
                        ResolvedTypeKind::GenericParam(_) | ResolvedTypeKind::Unknown
                    )
                })
            {
                let key = InstanceKey {
                    original_def_id: template_decl_id,
                    concrete_args: concrete_args.clone(),
                };
                self.ctxt.monomorph_backlog.insert(key);
            }

            let specialized_type =
                self.specialize_signature(&template_info, &concrete_args, expr.span.clone());

            self.insert(expr.hir_id, specialized_type.clone());
            specialized_type
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn index_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Index { target, index } = &expr.kind {
            let target_ty = self.expr_type(target);
            let index_ty = self.expr_type(index);

            if !self.is_integer(&index_ty.kind) {
                self.report(
                    format!(
                        "Invalid index type '{}' array indexes must be integers",
                        index_ty.name
                    ),
                    Some(expr.span.clone()),
                );
                return self.unknown(expr.span.clone());
            }

            match &target_ty.kind {
                ResolvedTypeKind::Array { inner, .. } => *inner.clone(),
                ResolvedTypeKind::Pointer { inner } => match &inner.kind {
                    ResolvedTypeKind::Array {
                        inner: arr_elem, ..
                    } => *arr_elem.clone(),
                    _ => {
                        self.report(
                                format!("Cannot index into pointer to non-array type '{}'  use dereference or pointer arithmetic instead", inner.name),
                                Some(expr.span.clone()),
                            );
                        self.unknown(expr.span.clone())
                    }
                },
                _ => {
                    self.report(
                        format!(
                            "Cannot index into a non indexable type '{}'",
                            target_ty.name
                        ),
                        Some(expr.span.clone()),
                    );
                    self.unknown(expr.span.clone())
                }
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn unary_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Unary(op, target) = &expr.kind {
            let target_ty = self.expr_type(target);
            match op {
                HirUnaryOp::Dereference => self.deref_type(&target_ty),
                HirUnaryOp::AddressOf => self.address_of_type(&target_ty),
                HirUnaryOp::Increment | HirUnaryOp::Decrement => self.inc_dec_type(&target_ty),
                HirUnaryOp::Neg => self.neg_type(&target_ty),
                HirUnaryOp::Not => self.logical_not_type(&target_ty),
                HirUnaryOp::BitNot => {
                    if self.is_integer(&target_ty.kind) {
                        // The type doesn't change
                        target_ty
                    } else {
                        self.report(
                            format!(
                                "Bitwise operators require integer operands but got {}",
                                target_ty.name
                            ),
                            Some(target_ty.span.clone()),
                        );
                        self.unknown(target_ty.span.clone())
                    }
                }
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn neg_type(&mut self, ty: &TypeInfo) -> TypeInfo {
        if self.is_signed_numeric(ty) {
            ty.clone()
        } else {
            self.report(
                format!("Cannot apply '-' to type '{}'", ty.name),
                Some(ty.span.clone()),
            );
            self.unknown(ty.span.clone())
        }
    }

    fn logical_not_type(&mut self, ty: &TypeInfo) -> TypeInfo {
        match ty.kind {
            ResolvedTypeKind::Bool => ty.clone(),
            _ => {
                self.report(
                    "Operator '!' can only be applied to 'bool'".to_string(),
                    Some(ty.span.clone()),
                );
                self.unknown(ty.span.clone())
            }
        }
    }

    fn inc_dec_type(&mut self, target_ty: &TypeInfo) -> TypeInfo {
        if !self.is_numeric(&target_ty) {
            self.report(
                format!(
                    "Cannot apply operator to non-numeric type '{}'",
                    target_ty.name
                ),
                Some(target_ty.span.clone()),
            );
            self.unknown(target_ty.span.clone())
        } else {
            target_ty.clone()
        }
    }

    fn deref_type(&mut self, src_ty: &TypeInfo) -> TypeInfo {
        match &src_ty.kind {
            ResolvedTypeKind::Pointer { inner } => {
                if inner.kind == ResolvedTypeKind::Unit {
                    self.report(
                        format!("Cannot dereference a pointer of type '{}'", inner.name),
                        Some(inner.span.clone()),
                    );
                }
                *inner.clone()
            }
            _ => {
                self.report(
                    format!("Cannot dereference type '{}'", src_ty.name),
                    Some(src_ty.span.clone()),
                );
                self.unknown(src_ty.span.clone())
            }
        }
    }

    fn address_of_type(&mut self, src_ty: &TypeInfo) -> TypeInfo {
        let ptr_kind = ResolvedTypeKind::Pointer {
            inner: Box::new(src_ty.clone()),
        };
        let ptr_id = self.registry.issue_id(ptr_kind.clone());
        let ptr_layout = self.get_layout(&ptr_kind, ptr_id.clone(), src_ty.span.clone());
        TypeInfo {
            type_id: ptr_id,
            name: TypeInfo::name(ptr_kind.clone()),
            kind: ptr_kind,
            layout: ptr_layout,
            span: src_ty.span.clone(),
        }
    }

    fn cast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::StaticCast(target, src_expr) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);
            let src_ty = self.expr_type(src_expr);

            let allowed = match (&src_ty.kind, &target_ty.kind) {
                (_, _) if self.is_numeric(&src_ty) && self.is_numeric(&target_ty) => true,

                (ResolvedTypeKind::Enum { .. }, t) if self.is_integer(t) => true,

                // Pointer to Pointer address reassignment
                (ResolvedTypeKind::Pointer { .. }, ResolvedTypeKind::Pointer { .. }) => true,

                // Any Integer to Pointer (Handles isize, usize, i64, ...)
                (src, ResolvedTypeKind::Pointer { .. }) if self.is_integer(src) => true,
                (ResolvedTypeKind::Pointer { .. }, tgt) if self.is_integer(tgt) => true, // Pointer value conversions to raw address tracking limits

                _ => false,
            };

            if !allowed {
                self.report(
                    format!(
                        "Invalid cast cannot convert '{}' to '{}'",
                        src_ty.name, target_ty.name
                    ),
                    Some(expr.span.clone()),
                );
                self.unknown(expr.span.clone())
            } else {
                target_ty
            }

            //Will have to apply some casting rules here
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn bitcast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::BitCast(target, src_expr) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);
            let src_ty = self.expr_type(src_expr);

            if src_ty.layout.size != target_ty.layout.size {
                self.report(
                    format!("bitcast size mismatch, cannot reinterpret '{}' ({} bytes) as '{}' ({} bytes), ensure sizes match",
                    src_ty.name,
                    src_ty.layout.size,
                    target_ty.name,
                    target_ty.layout.size),
                    Some(expr.span.clone()));
                self.unknown(expr.span.clone())
            } else {
                target_ty
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn postfix_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Postfix(inner, op) = &expr.kind {
            let inner_ty = self.expr_type(inner);
            match op {
                HirPostfixOp::Increment | HirPostfixOp::Decrement => self.inc_dec_type(&inner_ty),
                _ => self.unknown(expr.span.clone()),
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn binary_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Binary(left, op, right) = &expr.kind {
            let left_ty = self.expr_type(left);
            self.expr_type(right);

            self.coerce_ty(&left_ty, right);
            let coerced_right_ty = self.expr_type(right);

            match op {
                HirBinaryOp::Add
                | HirBinaryOp::Sub
                | HirBinaryOp::Div
                | HirBinaryOp::Mul
                | HirBinaryOp::Mod => {
                    if left_ty.is_pointer() || coerced_right_ty.is_pointer() {
                        return self.pointer_arithmetic_type(
                            &left_ty,
                            op,
                            &coerced_right_ty,
                            expr.span.clone(),
                        );
                    }
                    self.arithmetic_type(left_ty, coerced_right_ty, expr.span.clone())
                }
                HirBinaryOp::Eq
                | HirBinaryOp::Neq
                | HirBinaryOp::Lt
                | HirBinaryOp::Gt
                | HirBinaryOp::Geq
                | HirBinaryOp::Leq => {
                    self.comparison_binary_type(&left_ty, &coerced_right_ty, expr.span.clone())
                }
                HirBinaryOp::And | HirBinaryOp::Or => {
                    self.logical_binary_type(&left_ty, &coerced_right_ty, expr.span.clone())
                }
                HirBinaryOp::Assign
                | HirBinaryOp::AddAssign
                | HirBinaryOp::SubAssign
                | HirBinaryOp::MulAssign
                | HirBinaryOp::ModAssign
                | HirBinaryOp::DivAssign => {
                    if let HirExprKind::Binary(_, inner_op, _) = &right.kind {
                        if matches!(
                            inner_op,
                            HirBinaryOp::Assign
                                | HirBinaryOp::AddAssign
                                | HirBinaryOp::SubAssign
                                | HirBinaryOp::MulAssign
                                | HirBinaryOp::ModAssign
                                | HirBinaryOp::DivAssign
                        ) && coerced_right_ty.is_array()
                        {
                            self.report(
                                "Cannot chain assignment through an array-typed assignment"
                                    .to_string(),
                                Some(expr.span.clone()),
                            );
                            return self.unknown(expr.span.clone());
                        }
                    }
                    self.assignment_type(&left_ty, &coerced_right_ty, expr.span.clone())
                }
                HirBinaryOp::Access => self.access_type(&left_ty, right),
                HirBinaryOp::Shr
                | HirBinaryOp::Shl
                | HirBinaryOp::BitAnd
                | HirBinaryOp::BitOr
                | HirBinaryOp::Xor => {
                    self.bitwise_type(&left_ty, &coerced_right_ty, expr.span.clone())
                }
                _ => self.unknown(expr.span.clone()),
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn access_type(&mut self, left_ty: &TypeInfo, field_expr: &HirExpr) -> TypeInfo {
        match &left_ty.kind {
            ResolvedTypeKind::Struct { name, members, .. }
            | ResolvedTypeKind::Enum { name, members, .. } => {
                let field_name = match &field_expr.kind {
                    HirExprKind::Identifier(n) => n,
                    _ => {
                        self.report(
                            "Right-hand side of struct/enum access must be an identifier".into(),
                            Some(field_expr.span.clone()),
                        );
                        return self.unknown(field_expr.span.clone());
                    }
                };

                if let Some(member_tuple) = members.iter().find(|m| m.0 == *field_name) {
                    self.ctxt
                        .names
                        .resolved
                        .insert(field_expr.hir_id, member_tuple.2);
                    // The field identifier itself carries no error and is a plain reference
                    // expression: type it as Unit rather than leaving it Unknown.
                    let field_ty = self.unit(field_expr.span.clone());
                    self.insert(field_expr.hir_id, field_ty);
                    member_tuple.1.clone() // Returns field's type
                } else {
                    self.unknown_member(field_name, name, field_expr.span.clone());
                    self.unknown(field_expr.span.clone())
                }
            }

            ResolvedTypeKind::Variant { name, arms, .. } => {
                let (variant_name, provided_args) = match &field_expr.kind {
                    HirExprKind::Identifier(n) => (n.as_str(), None),
                    HirExprKind::Call(callee, args) => match &callee.kind {
                        HirExprKind::Identifier(n) => (n.as_str(), Some(args)),
                        _ => {
                            self.report(
                                "Expected variant constructor identifier".into(),
                                Some(field_expr.span.clone()),
                            );
                            return self.unknown(field_expr.span.clone());
                        }
                    },
                    _ => {
                        self.report(
                            "Right-hand side of variant access must be a constructor or identifier"
                                .into(),
                            Some(field_expr.span.clone()),
                        );
                        return self.unknown(field_expr.span.clone());
                    }
                };

                if let Some(arm_tuple) = arms.iter().find(|m| m.0 == variant_name) {
                    self.ctxt
                        .names
                        .resolved
                        .insert(field_expr.hir_id, arm_tuple.2);

                    self.ctxt
                        .types
                        .types
                        .insert(field_expr.hir_id, left_ty.clone());

                    if let HirExprKind::Call(callee, _) = &field_expr.kind {
                        self.ctxt.types.types.insert(callee.hir_id, left_ty.clone());
                    }

                    let expected_arg_tys = &arm_tuple.3;

                    match (provided_args, expected_arg_tys.is_empty()) {
                        (None, true) => {}

                        (Some(_), true) => {
                            self.report(
                                format!(
                                    "Variant '{}.{}' does not take payload arguments",
                                    name, variant_name
                                ),
                                Some(field_expr.span.clone()),
                            );
                        }

                        (None, false) => {
                            self.report(
                                format!(
                                    "Variant '{}.{}' requires payload arguments ({})",
                                    name,
                                    variant_name,
                                    expected_arg_tys.len()
                                ),
                                Some(field_expr.span.clone()),
                            );
                        }

                        (Some(args), false) => {
                            if args.len() != expected_arg_tys.len() {
                                self.report(
                                    format!(
                                        "Variant '{}.{}' expects {} arguments, but got {}",
                                        name,
                                        variant_name,
                                        expected_arg_tys.len(),
                                        args.len()
                                    ),
                                    Some(field_expr.span.clone()),
                                );
                            } else {
                                for (arg_expr, expected_ty) in args.iter().zip(expected_arg_tys) {
                                    self.expr_type(arg_expr);
                                    self.coerce_ty(expected_ty, arg_expr);
                                    let arg_ty = self.expr_type(arg_expr);

                                    if !TypeInfo::types_match(expected_ty, &arg_ty) {
                                        self.type_mismatch(
                                            expected_ty,
                                            &arg_ty,
                                            arg_expr.span.clone(),
                                        );
                                    }
                                }
                            }
                        }
                    }

                    left_ty.clone()
                } else {
                    self.unknown_member(&variant_name.to_string(), name, field_expr.span.clone());
                    self.unknown(field_expr.span.clone())
                }
            }
            ResolvedTypeKind::Tuple { fields } => {
                let index = match &field_expr.kind {
                    HirExprKind::Literal(HirLiteral::Int(idx)) => *idx as usize,
                    _ => {
                        self.report(
                            "Right-hand side of tuple access must be an integer literal".into(),
                            Some(field_expr.span.clone()),
                        );
                        return self.unknown(field_expr.span.clone());
                    }
                };

                if index < fields.len() {
                    fields[index].clone()
                } else {
                    self.report(
                        format!(
                            "Tuple index {} out of bounds for tuple of length {}",
                            index,
                            fields.len()
                        ),
                        Some(field_expr.span.clone()),
                    );
                    self.unknown(field_expr.span.clone())
                }
            }

            _ => {
                if left_ty.kind != ResolvedTypeKind::Unknown {
                    self.report(
                        format!(
                            "Cannot carry out an access operation on type '{}'",
                            left_ty.name
                        ),
                        Some(field_expr.span.clone()),
                    );
                }
                self.unknown(field_expr.span.clone())
            }
        }
    }

    fn bitwise_type(&mut self, left_ty: &TypeInfo, right_ty: &TypeInfo, span: Span) -> TypeInfo {
        if !self.is_integer(&left_ty.kind) || !self.is_integer(&right_ty.kind) {
            self.report(
                format!(
                    "Bitwise operators require integer operands but got {} and {}",
                    left_ty.name, right_ty.name
                ),
                Some(span.clone()),
            );
            return self.unknown(span);
        }

        if !TypeInfo::types_match(left_ty, right_ty) {
            self.type_mismatch(left_ty, right_ty, span.clone());
            return self.unknown(span);
        }

        left_ty.clone()
    }

    fn assignment_type(&mut self, left_ty: &TypeInfo, right_ty: &TypeInfo, span: Span) -> TypeInfo {
        if !TypeInfo::types_match(left_ty, right_ty) {
            self.type_mismatch(left_ty, right_ty, span.clone());
            self.unknown(span)
        } else {
            left_ty.clone()
        }
    }

    fn comparison_binary_type(
        &mut self,
        left_ty: &TypeInfo,
        right_ty: &TypeInfo,
        span: Span,
    ) -> TypeInfo {
        if !TypeInfo::types_match(left_ty, right_ty) {
            self.type_mismatch(left_ty, right_ty, span.clone());
            self.unknown(span.clone())
        } else {
            self.boolean(span)
        }
    }

    fn logical_binary_type(
        &mut self,
        left_ty: &TypeInfo,
        right_ty: &TypeInfo,
        span: Span,
    ) -> TypeInfo {
        if right_ty.kind != ResolvedTypeKind::Bool || left_ty.kind != ResolvedTypeKind::Bool {
            self.report(
                format!(
                    "logical binary operator cannot be applied to types '{}' and '{}'",
                    left_ty.name, right_ty.name
                ),
                Some(span.clone()),
            );
            self.unknown(span.clone())
        } else {
            self.boolean(span)
        }
    }

    fn call_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Call(name, args) = &expr.kind {
            let overall_ty = self.expr_type(name);
            match &overall_ty.kind {
                ResolvedTypeKind::Func {
                    params,
                    ret_type,
                    param_defaults,
                    ..
                } => {
                    let min_args = params
                        .len()
                        .saturating_sub(param_defaults.iter().filter(|d| **d).count());

                    if args.len() < min_args || args.len() > params.len() {
                        let expected = if min_args == params.len() {
                            params.len().to_string()
                        } else {
                            format!("{min_args}-{}", params.len())
                        };
                        self.report(
                            format!(
                                "Invalid argument count expected '{}' but got '{}'",
                                expected,
                                args.len()
                            ),
                            Some(expr.span.clone()),
                        );
                    }

                    for (arg, param_ty) in args.iter().zip(params.iter()) {
                        self.expr_type(arg);
                        self.coerce_ty(param_ty, arg);
                        let arg_ty = self.expr_type(arg);

                        if !TypeInfo::types_match(&arg_ty, param_ty) {
                            self.type_mismatch(&arg_ty, param_ty, expr.span.clone());
                        }
                    }

                    *ret_type.clone()
                }
                _ => self.unknown(expr.span.clone()),
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn arithmetic_type(&mut self, left_ty: TypeInfo, right_ty: TypeInfo, span: Span) -> TypeInfo {
        // both must be numeric
        if !self.is_numeric(&left_ty) {
            self.report(
                "Left operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return self.unknown(span.clone());
        }

        if !self.is_numeric(&right_ty) {
            self.report(
                "Right operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return self.unknown(span.clone());
        }

        if !TypeInfo::types_match(&left_ty, &right_ty) {
            self.type_mismatch(&left_ty, &right_ty, span.clone());
            return self.unknown(span.clone());
        }

        left_ty
    }

    fn pointer_arithmetic_type(
        &mut self,
        left_ty: &TypeInfo,
        op: &HirBinaryOp,
        right_ty: &TypeInfo,
        span: Span,
    ) -> TypeInfo {
        match (op, &left_ty.kind, &right_ty.kind) {
            // Ptr + Int = Ptr
            (HirBinaryOp::Add, ResolvedTypeKind::Pointer { .. }, _)
                if self.is_integer(&right_ty.kind) =>
            {
                left_ty.clone()
            }

            // Int + Ptr = Ptr
            (HirBinaryOp::Add, _, ResolvedTypeKind::Pointer { .. })
                if self.is_integer(&left_ty.kind) =>
            {
                right_ty.clone()
            }

            // Ptr - Int = Ptr
            (HirBinaryOp::Sub, ResolvedTypeKind::Pointer { .. }, _)
                if self.is_integer(&right_ty.kind) =>
            {
                left_ty.clone()
            }

            // Ptr - Ptr -> USize
            (
                HirBinaryOp::Sub,
                ResolvedTypeKind::Pointer { inner: t1 },
                ResolvedTypeKind::Pointer { inner: t2 },
            ) => {
                if !TypeInfo::types_match(t1, t2) {
                    self.report(
                        format!(
                            "Cannot subtract pointers to different types `{}` and `{}`",
                            t1.name, t2.name
                        ),
                        Some(span.clone()),
                    );
                    return self.unknown(span);
                }
                self.primitive(ResolvedTypeKind::USize, span)
            }

            // Invalid Ops (like Ptr * Int, Ptr / Ptr, Ptr + Float)
            _ => {
                self.report(
                    format!(
                        "Invalid pointer arithmetic operation: `{}` {:?} `{}`",
                        left_ty.name, op, right_ty.name
                    ),
                    Some(span.clone()),
                );
                self.unknown(span)
            }
        }
    }

    fn literal_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Literal(lit) = &expr.kind {
            match lit {
                HirLiteral::Int8(_) => self.primitive(ResolvedTypeKind::I8, expr.span.clone()),
                HirLiteral::Uint8(_) => self.primitive(ResolvedTypeKind::U8, expr.span.clone()),
                HirLiteral::Int16(_) => self.primitive(ResolvedTypeKind::I16, expr.span.clone()),
                HirLiteral::Uint16(_) => self.primitive(ResolvedTypeKind::U16, expr.span.clone()),
                HirLiteral::Int32(_) => self.primitive(ResolvedTypeKind::I32, expr.span.clone()),
                HirLiteral::Uint32(_) => self.primitive(ResolvedTypeKind::U32, expr.span.clone()),
                HirLiteral::Int64(_) => self.primitive(ResolvedTypeKind::I64, expr.span.clone()),
                HirLiteral::Uint64(_) => self.primitive(ResolvedTypeKind::U64, expr.span.clone()),
                HirLiteral::Int128(_) => self.primitive(ResolvedTypeKind::I128, expr.span.clone()),
                HirLiteral::Uint128(_) => self.primitive(ResolvedTypeKind::U128, expr.span.clone()),
                HirLiteral::IntSize(_) => {
                    self.primitive(ResolvedTypeKind::ISize, expr.span.clone())
                }
                HirLiteral::UintSize(_) => {
                    self.primitive(ResolvedTypeKind::USize, expr.span.clone())
                }
                HirLiteral::Int(_) => self.primitive(ResolvedTypeKind::ISize, expr.span.clone()),
                HirLiteral::Float(_) => self.primitive(ResolvedTypeKind::F64, expr.span.clone()),
                HirLiteral::F32(_) => self.primitive(ResolvedTypeKind::F32, expr.span.clone()),
                HirLiteral::F64(_) => self.primitive(ResolvedTypeKind::F64, expr.span.clone()),
                HirLiteral::Str(_) => self.primitive(ResolvedTypeKind::Str, expr.span.clone()),
                HirLiteral::Char8(_) => self.primitive(ResolvedTypeKind::Char8, expr.span.clone()),
                HirLiteral::Char16(_) => {
                    self.primitive(ResolvedTypeKind::Char16, expr.span.clone())
                }
                HirLiteral::Char32(_) => {
                    self.primitive(ResolvedTypeKind::Char32, expr.span.clone())
                }
                HirLiteral::Bool(_) => self.primitive(ResolvedTypeKind::Bool, expr.span.clone()),
                HirLiteral::Null => {
                    let inner_ty = self.unknown(expr.span.clone());
                    self.primitive(
                        ResolvedTypeKind::Nullable {
                            ty: Box::new(inner_ty),
                        },
                        expr.span.clone(),
                    )
                }
                HirLiteral::ArrayLiteral(elements) => {
                    self.check_array_literal(elements, expr.span.clone())
                }
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn check_array_literal(&mut self, elements: &Vec<HirExpr>, span: Span) -> TypeInfo {
        if elements.is_empty() {
            self.unknown(span)
        } else {
            let first_ty = self.expr_type(&elements[0]);
            let mut all_okay = true;
            for element in elements.iter().skip(1) {
                let element_ty = self.expr_type(element);
                if !TypeInfo::types_match(&first_ty, &element_ty) {
                    self.report(
                        format!("array elements must all have the same type"),
                        Some(span.clone()),
                    );
                    all_okay = false;
                }
            }

            if all_okay {
                self.array(first_ty, Some(elements.len() as u64), span)
            } else {
                self.unknown(span)
            }
        }
    }
}
