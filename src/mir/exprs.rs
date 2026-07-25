use crate::{
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral},
    mir::{
        builder::MIRBuilder,
        instructions::{ConstantValue, MIRValue},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_expr(&mut self, expr: &HirExpr) {
        let ty = self.get_type(&expr.hir_id);
        match &expr.kind {
            HirExprKind::Literal(_) => {
                let src = self.expr_value(expr);
                self.build_assign(src, ty)
            }
            HirExprKind::Identifier(_) => {
                self.expr_value(expr);
            }
            HirExprKind::Binary(lhs, operat, rhs) => {
                let lhs_value = self.expr_value(lhs);
                let rhs_value = self.expr_value(rhs);
                match operat {
                    HirBinaryOp::Add
                    | HirBinaryOp::Sub
                    | HirBinaryOp::Mul
                    | HirBinaryOp::Div
                    | HirBinaryOp::Mod => {
                        let op = self.map_arithmetic_op(operat);
                        self.build_binary(op, lhs_value, rhs_value, ty)
                    }
                    HirBinaryOp::Eq
                    | HirBinaryOp::Neq
                    | HirBinaryOp::Lt
                    | HirBinaryOp::Gt
                    | HirBinaryOp::Leq
                    | HirBinaryOp::Geq => {
                        let cmp_op = self.map_cmp_op(operat, &lhs_value);
                        self.build_cmp(cmp_op, lhs_value, rhs_value)
                    }
                    _ => todo!(),
                }
            }
            _ => todo!("Will add other expressions later"),
        }
    }

    pub fn expr_value(&mut self, expr: &HirExpr) -> MIRValue {
        match &expr.kind {
            HirExprKind::Literal(lit) => self.literal_value(&lit),
            HirExprKind::Identifier(name) => {
                let ptr = self.lookup_var(name).cloned().expect("Variable not found");
                let ty = self.get_type(&expr.hir_id);
                let dest = self.new_register(ty.clone());
                self.build_load(dest.clone(), ptr, ty);
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
