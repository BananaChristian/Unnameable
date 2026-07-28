use crate::{
    const_and_mut_validator::validator::{BindingKind, Validator},
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirPostfixOp, HirUnaryOp},
};

impl Validator {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Binary(l, op, r) => self.check_binary(op, r, l),
            HirExprKind::Postfix(operand, op) => match op {
                HirPostfixOp::Increment => self.check_mutation_target(operand, "increment"),
                HirPostfixOp::Decrement => self.check_mutation_target(operand, "decrement"),
                _ => (),
            },
            HirExprKind::Unary(op, operand) => match op {
                HirUnaryOp::Increment => self.check_mutation_target(operand, "increment"),
                HirUnaryOp::Decrement => self.check_mutation_target(operand, "decrement"),
                _ => (),
            },
            HirExprKind::DollarScope { body, result } => {
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
            _ => (),
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
            _ => (),
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
            // If dereferencing or field accessing, you might want to recurse down to verify
            // base objects, but for basic local variables, validating identifier targets covers 95%.
            _ => {
                // If the target expression isn't an identifier (e.g. assigning to a literal `5 = x`),
                // your parser likely caught this, but we recurse just in case.
                self.check_expr(expr);
            }
        }
    }
}
