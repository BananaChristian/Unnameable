use crate::{
    const_and_mut_validator::validator::{BindingKind, Validator},
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
            match self.look_up(name) {
                Some(bind) => bind,
                None => BindingKind::Immutable,
            }
        } else {
            BindingKind::Immutable
        }
    }

    fn check_mutation_target(&mut self, expr: &HirExpr, action_description: &str) {
        match &expr.kind {
            HirExprKind::Identifier(name) => {
                let binding = self.check_identifier_binding(expr);
                match binding {
                    BindingKind::Const => {
                        self.report(
                            format!("Cannot {} constant variable '{}'", action_description, name),
                            Some(expr.span.clone()),
                        );
                    }
                    BindingKind::Immutable => {
                        self.report(
                            format!(
                                "Cannot {} immutable variable '{}'",
                                action_description, name
                            ),
                            Some(expr.span.clone()),
                        );
                    }
                    BindingKind::Mutable => {
                        // All clear! Mutating a mutable variable is perfectly valid.
                    }
                }
            }
            // Field access: walk the field side for embedded mutations, then
            // recurse into the base expression.
            HirExprKind::Binary(left, HirBinaryOp::Access, right) => {
                self.check_expr(right);
                self.check_mutation_target(left, action_description);
            }
            // Element access: walk the index for embedded mutations, then
            // recurse into the target expression.
            HirExprKind::Index { target, index } => {
                self.check_expr(index);
                self.check_mutation_target(target, action_description);
            }
            // Non-chain targets: walk for embedded mutations only.
            _ => {
                self.check_expr(expr);
            }
        }
    }
}
