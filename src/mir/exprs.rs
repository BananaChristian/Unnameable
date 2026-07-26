use crate::{
    diagnostics::Span,
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral, HirPostfixOp, HirUnaryOp},
    mir::{
        builder::MIRBuilder,
        instructions::{ConstantValue, MIROps, MIRTy, MIRValue},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_expr(&mut self, expr: &HirExpr) {
        let ty = self.get_type(&expr.hir_id);
        let span = Some(expr.span.clone());
        match &expr.kind {
            HirExprKind::Literal(_) => {
                let src = self.expr_value(expr);
                self.build_assign(src, ty, span)
            }
            HirExprKind::Identifier(_) => {
                self.expr_value(expr);
            }
            HirExprKind::Unary(op, operand) => self.build_unary(op, ty, operand),
            HirExprKind::Postfix(operand, op) => self.build_postfix(op, operand, ty),
            HirExprKind::Binary(lhs, operat, rhs) => self.build_bin(operat, lhs, rhs, span, ty),
            _ => todo!("Will add other expressions later"),
        }
    }

    fn build_bin(
        &mut self,
        op: &HirBinaryOp,
        lhs: &HirExpr,
        rhs: &HirExpr,
        span: Option<Span>,
        ty: MIRTy,
    ) {
        let lhs_value = self.expr_value(lhs);
        let rhs_value = self.expr_value(rhs);
        match op {
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Mod => {
                let op = self.map_arithmetic_op(op, &lhs_value);
                self.build_binary(op, lhs_value, rhs_value, ty, span)
            }
            HirBinaryOp::AddAssign
            | HirBinaryOp::SubAssign
            | HirBinaryOp::MulAssign
            | HirBinaryOp::DivAssign
            | HirBinaryOp::ModAssign => {
                // lhs_value is the loaded value of x
                // rhs_value is the right side
                let base_op = match op {
                    HirBinaryOp::AddAssign => MIROps::Add,
                    HirBinaryOp::SubAssign => MIROps::Sub,
                    HirBinaryOp::MulAssign => MIROps::Mul,
                    HirBinaryOp::ModAssign => MIROps::Mod,
                    HirBinaryOp::DivAssign => {
                        if self.is_signed(&lhs_value) {
                            MIROps::Sdiv
                        } else {
                            MIROps::Udiv
                        }
                    }
                    _ => unreachable!(),
                };
                // emit the operation
                self.build_binary(base_op, lhs_value, rhs_value, ty, span.clone());

                let Some(result) = self.last_value.as_ref().cloned() else {
                    self.report_ice("Failed to get last MIRValue".to_string(), span.clone());
                    return;
                };

                // store back into lhs's slot
                let ptr = self.lookup_ptr(lhs);
                self.build_store(ptr, result, span);
            }
            HirBinaryOp::Eq
            | HirBinaryOp::Neq
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::Leq
            | HirBinaryOp::Geq => {
                let cmp_op = self.map_cmp_op(op, &lhs_value);
                self.build_cmp(cmp_op, lhs_value, rhs_value, span)
            }
            HirBinaryOp::Assign => {
                let ptr = self.lookup_ptr(lhs);
                self.build_store(ptr, rhs_value, span);
            }
            _ => todo!(),
        }
    }

    fn build_postfix(&mut self, op: &HirPostfixOp, operand: &HirExpr, ty: MIRTy) {
        let span = Some(operand.span.clone());
        let old_val = self.expr_value(operand);

        let base_op = match op {
            HirPostfixOp::Increment => MIROps::Add,
            HirPostfixOp::Decrement => MIROps::Sub,
            _ => todo!("Add propagate later"),
        };

        let one_val = MIRValue::Constant(ConstantValue::Int(1));
        self.build_binary(base_op, old_val.clone(), one_val, ty, span.clone());

        let Some(new_val) = self.last_value.as_ref().cloned() else {
            self.report_ice("Failed to get last MIRValue".to_string(), span);
            return;
        };

        let ptr = self.lookup_ptr(operand);
        self.build_store(ptr, new_val, span);

        self.last_value = Some(old_val)
    }

    fn build_unary(&mut self, op: &HirUnaryOp, ty: MIRTy, operand: &HirExpr) {
        let span = Some(operand.span.clone());
        let operand_val = self.expr_value(operand);

        match op {
            HirUnaryOp::Not => {
                let true_val = MIRValue::Constant(ConstantValue::Bool(true));
                self.build_binary(MIROps::Xor, operand_val, true_val, ty, span);
            }
            HirUnaryOp::Neg => {
                let zero_val = MIRValue::Constant(ConstantValue::Int(1));
                self.build_binary(MIROps::Sub, operand_val, zero_val, ty, span);
            }
            HirUnaryOp::Increment | HirUnaryOp::Decrement => {
                let base_op = if matches!(op, HirUnaryOp::Increment) {
                    MIROps::Add
                } else {
                    MIROps::Sub
                };

                let one_val = MIRValue::Constant(ConstantValue::Int(1));

                self.build_binary(base_op, operand_val, one_val, ty, span.clone());

                let Some(new_val) = self.last_value.as_ref().cloned() else {
                    self.report_ice("Failed to get last MIRValue".to_string(), span);
                    return;
                };

                let ptr = self.lookup_ptr(operand);
                self.build_store(ptr, new_val.clone(), span);
                self.last_value = Some(new_val)
            }
            _ => (),
        }
    }

    pub fn expr_value(&mut self, expr: &HirExpr) -> MIRValue {
        let span = Some(expr.span.clone());
        match &expr.kind {
            HirExprKind::Literal(lit) => self.literal_value(&lit),
            HirExprKind::Identifier(name) => {
                let ptr = self.lookup_var(name).cloned().expect("Variable not found");
                let ty = self.get_type(&expr.hir_id);
                let dest = self.new_register(ty.clone());
                self.build_load(dest.clone(), ptr, ty, span);
                dest
            }

            _ => todo!("Will add other expressions later"),
        }
    }

    fn literal_value(&mut self, lit: &HirLiteral) -> MIRValue {
        match *lit {
            HirLiteral::Int8(val) => MIRValue::Constant(ConstantValue::I8(val)),
            HirLiteral::Uint8(val) => MIRValue::Constant(ConstantValue::U8(val)),
            HirLiteral::Int16(val) => MIRValue::Constant(ConstantValue::I16(val)),
            HirLiteral::Uint16(val) => MIRValue::Constant(ConstantValue::U16(val)),
            HirLiteral::Int32(val) => MIRValue::Constant(ConstantValue::I32(val)),
            HirLiteral::Uint32(val) => MIRValue::Constant(ConstantValue::U32(val)),
            HirLiteral::Int64(val) => MIRValue::Constant(ConstantValue::I64(val)),
            HirLiteral::Uint64(val) => MIRValue::Constant(ConstantValue::U64(val)),
            HirLiteral::Int(val) => MIRValue::Constant(ConstantValue::I64(val)),
            HirLiteral::IntSize(val) => MIRValue::Constant(ConstantValue::Int(val)),
            HirLiteral::UintSize(val) => MIRValue::Constant(ConstantValue::UInt(val)),
            HirLiteral::Int128(val) => MIRValue::Constant(ConstantValue::I128(val)),
            HirLiteral::Uint128(val) => MIRValue::Constant(ConstantValue::U128(val)),
            HirLiteral::F32(val) => MIRValue::Constant(ConstantValue::F32(val)),
            HirLiteral::F64(val) => MIRValue::Constant(ConstantValue::F64(val)),
            HirLiteral::Float(val) => MIRValue::Constant(ConstantValue::F64(val)),
            HirLiteral::Bool(val) => MIRValue::Constant(ConstantValue::Bool(val)),
            _ => todo!("Handle the other constants"),
        }
    }
}
