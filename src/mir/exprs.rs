use crate::{
    hir::{HirExpr, HirExprKind, HirLiteral},
    mir::{
        builder::MIRBuilder,
        instructions::{ConstantValue, MIRInstruction, MIRValue},
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_expr(&mut self, expr: &HirExpr) -> MIRInstruction {
        match &expr.kind {
            HirExprKind::Literal(lit) => self.build_literals(&lit),
            _ => todo!("Will add other expressions later"),
        }
    }

    fn build_literals(&mut self, lit: &HirLiteral) -> MIRInstruction {
        let src = match *lit {
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
        };

        self.build_assign(src)
    }
}
