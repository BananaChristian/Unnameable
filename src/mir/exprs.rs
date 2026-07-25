use crate::{
    hir::{HirExpr, HirExprKind, HirLiteral},
    mir::{
        builder::MIRBuilder,
        instructions::{ConstantValue, MIRInstruction, MIRValue},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_expr(&mut self, expr: &HirExpr) -> MIRInstruction {
        let ty= self.get_type(&expr.hir_id);
        match &expr.kind {
            HirExprKind::Literal(_) => {
                let src = self.expr_value(expr);
                self.build_assign(src, ty)
            }
            HirExprKind::Binary(lhs, operat, rhs) => {
                let lhs_value = self.expr_value(lhs);
                let rhs_value = self.expr_value(rhs);
                let op = self.map_binary_operator(operat);
                self.build_binary(op, lhs_value, rhs_value,ty)
            }
            _ => todo!("Will add other expressions later"),
        }
    }

    pub fn expr_value(&mut self, expr: &HirExpr) -> MIRValue {
        match &expr.kind {
            HirExprKind::Literal(lit) => self.literal_value(&lit),
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
