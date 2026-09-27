use crate::{
    const_and_mut_validator::validator::{BindingKind, Validator, WritePath},
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirPattern, HirPostfixOp, HirUnaryOp},
};

impl Validator {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Binary(l, op, r) => self.check_binary(op, r, l),
            HirExprKind::Postfix(operand, op) => match op {
                HirPostfixOp::Increment => self.check_mutation_target(operand, "increment"),
                HirPostfixOp::Decrement => self.check_mutation_target(operand, "decrement"),
                // Propagate carries the operand; walk it so embedded mutations
                // don't slip past the validator.
                HirPostfixOp::Propagate => self.check_expr(operand),
            },
            HirExprKind::Unary(op, operand) => match op {
                HirUnaryOp::Increment => self.check_mutation_target(operand, "increment"),
                HirUnaryOp::Decrement => self.check_mutation_target(operand, "decrement"),
                _ => self.check_expr(operand),
            },
            HirExprKind::Marked(inner) => self.check_expr(inner),
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
                if let Some(res) = result {
                    self.check_expr(res);
                }
            }
            HirExprKind::Index { target, index } => {
                self.check_expr(target);
                self.check_expr(index);
            }
            HirExprKind::Call(callee, args) => {
                self.check_expr(callee);
                for arg in args {
                    self.check_expr(arg);
                }
            }
            HirExprKind::StaticCast(_, inner) => self.check_expr(inner),
            HirExprKind::BitCast(_, inner) => self.check_expr(inner),
            HirExprKind::TupleInst { body } => {
                for elem in body {
                    self.check_expr(elem);
                }
            }
            HirExprKind::Instantiation { body, .. } => {
                for param in body {
                    self.check_expr(&param.value);
                }
            }
            HirExprKind::Unwrap(inner) => self.check_expr(inner),
            HirExprKind::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            HirExprKind::Block(body) => {
                for stmt in body {
                    self.check_stmt(stmt);
                }
            }
            _ => (),
        }
    }

    fn check_pattern(&mut self, pattern: &HirPattern) {
        match pattern {
            HirPattern::Wildcard | HirPattern::Binding { .. } => {}
            HirPattern::Literal(expr) => self.check_expr(expr),
            HirPattern::Path { payloads, .. } => {
                for payload in payloads {
                    self.check_pattern(payload);
                }
            }
            HirPattern::Tuple { elements, .. } => {
                for element in elements {
                    self.check_pattern(element);
                }
            }
            HirPattern::StructPattern { fields, .. } => {
                for field in fields {
                    self.check_pattern(&field.pattern);
                }
            }
            HirPattern::Or(alts) => {
                for alt in alts {
                    self.check_pattern(alt);
                }
            }
        }
    }

    fn check_binary(&mut self, op: &HirBinaryOp, right: &HirExpr, left: &HirExpr) {
        match op {
            HirBinaryOp::Assign => self.check_assignement(right, left),
            HirBinaryOp::AddAssign
            | HirBinaryOp::SubAssign
            | HirBinaryOp::MulAssign
            | HirBinaryOp::DivAssign
            | HirBinaryOp::ModAssign => self.check_opassign(op, right, left),
            // Not a mutation op, but the operands may still contain embedded
            // mutations (e.g. `1 + (y = 3)`); walk both sides.
            _ => {
                self.check_expr(right);
                self.check_expr(left);
            }
        }
    }

    fn check_opassign(&mut self, op: &HirBinaryOp, right: &HirExpr, left: &HirExpr) {
        self.check_expr(right);
        match op {
            HirBinaryOp::AddAssign => self.check_mutation_target(left, "add and assign to"),
            HirBinaryOp::SubAssign => self.check_mutation_target(left, "subtract and assign to"),
            HirBinaryOp::MulAssign => self.check_mutation_target(left, "multiply and assign to"),
            HirBinaryOp::ModAssign => self.check_mutation_target(left, "modulo and assign to"),
            HirBinaryOp::DivAssign => self.check_mutation_target(left, "divide and assign to"),
            _ => (),
        }
    }

    fn check_assignement(&mut self, right: &HirExpr, left: &HirExpr) {
        self.check_expr(right);
        self.check_mutation_target(left, "assign to");
    }

    fn check_identifier_binding(&mut self, expr: &HirExpr) -> BindingKind {
        if let HirExprKind::Identifier(name) = &expr.kind {
            self.look_up(name).unwrap_or_else(BindingKind::deny_all)
        } else {
            BindingKind::deny_all()
        }
    }

    fn check_mutation_target(&mut self, expr: &HirExpr, action_description: &str) {
        self.check_mutation_target_path(expr, action_description, WritePath::direct());
    }

    /// `path` records how the write reaches the binding. Anything other than
    /// `Direct` goes *through* the binding's type and so requires `mut T`;
    /// `mut var` on its own only permits reassigning the binding, because it
    /// says nothing about the type.
    fn check_mutation_target_path(
        &mut self,
        expr: &HirExpr,
        action_description: &str,
        path: WritePath,
    ) {
        match &expr.kind {
            HirExprKind::Identifier(name) => {
                let binding = self.check_identifier_binding(expr);
                let allowed = if path.is_through() {
                    binding.allows_write_through(path.deref_depth())
                } else {
                    binding.allows_direct_write()
                };
                if allowed {
                    return;
                }
                // The action reads as "assign to" / "add and assign to" /
                // "increment" / ..., so drop the trailing " to" when it has to
                // fit into "... through ...".
                let op = action_description
                    .strip_suffix(" to")
                    .unwrap_or(action_description);
                if binding.is_const {
                    self.report(
                        format!("Cannot {} constant variable '{}'", action_description, name),
                        Some(expr.span.clone()),
                    );
                } else if path.is_deref() {
                    // The binding is fine; it is the type it points at that is
                    // immutable, so name that rather than the pointer.
                    self.report(
                        format!("Cannot {} through pointer to immutable type '{}'", op, name),
                        Some(expr.span.clone()),
                    );
                } else if path.is_through() && binding.reassignable {
                    // `mut var` but an immutable type: the binding may be
                    // reassigned, but nothing may be written through it.
                    self.report(
                        format!(
                            "Cannot {} through immutable type of variable '{}'",
                            op, name
                        ),
                        Some(expr.span.clone()),
                    );
                } else {
                    self.report(
                        format!(
                            "Cannot {} immutable variable '{}'",
                            action_description, name
                        ),
                        Some(expr.span.clone()),
                    );
                }
            }
            // Field access: walk the field side for embedded mutations, then
            // recurse into the base expression. Going through a field is a
            // write through the base's type.
            HirExprKind::Binary(left, HirBinaryOp::Access, right) => {
                self.check_expr(right);
                // A field access reaches into the same type rather than moving
                // to another pointer depth, so `mut T` at the current depth is
                // what authorizes it. Any derefs an enclosing arm already applied
                // are carried forward.
                self.check_mutation_target_path(left, action_description, path.through_aggregate());
            }
            // Element access: walk the index for embedded mutations, then
            // recurse into the target expression.
            HirExprKind::Index { target, index } => {
                self.check_expr(index);
                self.check_mutation_target_path(
                    target,
                    action_description,
                    path.through_aggregate(),
                );
            }
            // A dereference writes through whatever the pointer points at, so it
            // is always a write through a type: `^p = 1` needs `ptr<mut T>`.
            HirExprKind::Unary(HirUnaryOp::Dereference, operand) => {
                self.check_mutation_target_path(operand, action_description, path.through_deref());
            }
            // Non-chain targets: walk for embedded mutations only.
            _ => {
                self.check_expr(expr);
            }
        }
    }
}
