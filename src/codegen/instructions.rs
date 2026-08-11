use std::collections::HashMap;

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};

use crate::{
    codegen::Codegen,
    mir::{BlockId, CmpOp, MIRInstruction, MIROps, MIRValue},
};

impl<'ctx> Codegen<'ctx> {
    fn bind_dest(&mut self, dest: &MIRValue, val: BasicValueEnum<'ctx>) {
        if let MIRValue::Register { vreg, .. } = dest {
            self.vreg_map.insert(vreg.clone(), val);
        }
    }

    fn lower_binary_op(
        &mut self,
        op: &MIROps,
        lhs_val: BasicValueEnum<'ctx>,
        rhs_val: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match (lhs_val, rhs_val) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => match op {
                MIROps::Add => self
                    .builder
                    .build_int_add(lhs, rhs, "addtmp")
                    .unwrap()
                    .into(),
                MIROps::Sub => self
                    .builder
                    .build_int_sub(lhs, rhs, "subtmp")
                    .unwrap()
                    .into(),
                MIROps::Mul => self
                    .builder
                    .build_int_mul(lhs, rhs, "multmp")
                    .unwrap()
                    .into(),
                MIROps::Sdiv => self
                    .builder
                    .build_int_signed_div(lhs, rhs, "sdivtmp")
                    .unwrap()
                    .into(),
                MIROps::Udiv => self
                    .builder
                    .build_int_unsigned_div(lhs, rhs, "udivtmp")
                    .unwrap()
                    .into(),
                MIROps::Mod => self
                    .builder
                    .build_int_signed_rem(lhs, rhs, "modtmp")
                    .unwrap()
                    .into(),
                MIROps::Xor => self.builder.build_xor(lhs, rhs, "xortmp").unwrap().into(),
                MIROps::And => self.builder.build_and(lhs, rhs, "andtmp").unwrap().into(),
                MIROps::Or => self.builder.build_or(lhs, rhs, "ortmp").unwrap().into(),
                MIROps::Shl =>self
                    .builder
                    .build_left_shift(lhs, rhs, "shrtmp")
                    .unwrap()
                    .into(),
                MIROps::Shr => self
                    .builder
                    .build_right_shift(lhs, rhs, false, "shrtmp")
                    .unwrap()
                    .into(),
                MIROps::Ashr => self
                    .builder
                    .build_right_shift(lhs, rhs, true, "ashrtmp")
                    .unwrap()
                    .into(),
            },
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => match op {
                MIROps::Add => self
                    .builder
                    .build_float_add(lhs, rhs, "faddtmp")
                    .unwrap()
                    .into(),
                MIROps::Sub => self
                    .builder
                    .build_float_sub(lhs, rhs, "fsubtmp")
                    .unwrap()
                    .into(),
                MIROps::Mul => self
                    .builder
                    .build_float_mul(lhs, rhs, "fmultmp")
                    .unwrap()
                    .into(),
                MIROps::Sdiv | MIROps::Udiv => self
                    .builder
                    .build_float_div(lhs, rhs, "fdivtmp")
                    .unwrap()
                    .into(),
                MIROps::Mod => self
                    .builder
                    .build_float_rem(lhs, rhs, "fmodtmp")
                    .unwrap()
                    .into(),
                _ => panic!("Unsupported float operation: {:?}", op),
            },
            _ => panic!("Type mismatch in binary operation operands"),
        }
    }

    pub fn lower_instruction(
        &mut self,
        inst: &MIRInstruction,
        bb_map: &HashMap<BlockId, BasicBlock<'ctx>>,
    ) {
        match inst {
            MIRInstruction::BinaryOperation { dest, op, lhs, rhs } => {
                let lhs_val = self.lower_value(lhs);
                let rhs_val = self.lower_value(rhs);
                let result = self.lower_binary_op(op, lhs_val, rhs_val);
                self.bind_dest(dest, result);
            }
            MIRInstruction::Assign { dest, src } => {
                let val = self.lower_value(src);
                self.bind_dest(dest, val);
            }
            MIRInstruction::Alloca {
                dest, ty, align, ..
            } => {
                let llvm_ty = self.get_llvmty(ty);
                let ptr = self.builder.build_alloca(llvm_ty, "allocatmp").unwrap();
                ptr.as_instruction()
                    .unwrap()
                    .set_alignment(*align as u32)
                    .unwrap();
                self.bind_dest(dest, ptr.into());
            }
            MIRInstruction::Load {
                dest,
                ptr,
                ty,
                align,
            } => {
                let ptr_val = self.lower_value(ptr).into_pointer_value();
                let llvm_ty = self.get_llvmty(ty);
                let val = self
                    .builder
                    .build_load(llvm_ty, ptr_val, "loadtmp")
                    .unwrap();
                val.as_instruction_value()
                    .unwrap()
                    .set_alignment(*align as u32)
                    .unwrap();
                self.bind_dest(dest, val);
            }
            MIRInstruction::Store { ptr, val, align } => {
                let ptr_val = self.lower_value(ptr).into_pointer_value();
                let val_val = self.lower_value(val);
                let store_inst = self.builder.build_store(ptr_val, val_val).unwrap();
                store_inst.set_alignment(*align as u32).unwrap();
            }
            MIRInstruction::Call { dest, callee, args } => {
                let func = self
                    .module
                    .get_function(callee)
                    .unwrap_or_else(|| panic!("Calle @{} not found in module", callee));

                let arg_vals: Vec<BasicMetadataValueEnum<'ctx>> =
                    args.iter().map(|a| self.lower_value(a).into()).collect();

                let call_site = self.builder.build_call(func, &arg_vals, callee).unwrap();
                if let Some(res_val) = call_site.try_as_basic_value().left() {
                    self.bind_dest(dest, res_val);
                }
            }
            MIRInstruction::Compare { dest, op, lhs, rhs } => {
                let lhs_val = self.lower_value(lhs);
                let rhs_val = self.lower_value(rhs);

                let res = match op {
                    // Floating point comparisons
                    CmpOp::Flt | CmpOp::Fgt | CmpOp::Fle | CmpOp::Fge => {
                        let float_pred = match op {
                            CmpOp::Flt => FloatPredicate::OLT,
                            CmpOp::Fgt => FloatPredicate::OGT,
                            CmpOp::Fle => FloatPredicate::OLE,
                            CmpOp::Fge => FloatPredicate::OGE,
                            _ => unreachable!(),
                        };
                        self.builder
                            .build_float_compare(
                                float_pred,
                                lhs_val.into_float_value(),
                                rhs_val.into_float_value(),
                                "cmptmp",
                            )
                            .unwrap()
                    }
                    // Integer (and Pointer/Bool) comparisons
                    _ => {
                        let int_pred = match op {
                            CmpOp::Eq => IntPredicate::EQ,
                            CmpOp::Neq => IntPredicate::NE,
                            CmpOp::Slt => IntPredicate::SLT,
                            CmpOp::Sgt => IntPredicate::SGT,
                            CmpOp::Sle => IntPredicate::SLE,
                            CmpOp::Sge => IntPredicate::SGE,
                            CmpOp::Ult => IntPredicate::ULT,
                            CmpOp::Ugt => IntPredicate::UGT,
                            CmpOp::Ule => IntPredicate::ULE,
                            CmpOp::Uge => IntPredicate::UGE,
                            _ => unreachable!(),
                        };
                        self.builder
                            .build_int_compare(
                                int_pred,
                                lhs_val.into_int_value(),
                                rhs_val.into_int_value(),
                                "cmptmp",
                            )
                            .unwrap()
                    }
                };

                self.bind_dest(dest, res.into());
            }
            _ => (),
        }
    }
}
