use crate::{
    const_and_mut_validator::validator::{BindingKind, Validator},
    hir::{HirBinaryOp, HirExpr, HirExprKind},
};

impl<'a> Validator<'a> {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Binary(l, op, r) => self.check_binary(op, r, l),
            _ => (),
        }
    }

    fn check_binary(&mut self, op: &HirBinaryOp, right: &HirExpr, left: &HirExpr) {
        match op {
            HirBinaryOp::Assign => self.check_assignement(right, left),
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
                            format!("Cannot {} constant '{}'", action_description, name),
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
