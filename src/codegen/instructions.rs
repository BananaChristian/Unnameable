use std::collections::HashMap;

use inkwell::{
    FloatPredicate, IntPredicate,
    basic_block::BasicBlock,
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum},
};

use crate::{
    codegen::Codegen,
    mir::{BlockId, CmpOp, MIRInstruction, MIROps, MIRTykind, MIRValue},
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
                MIROps::Shl => self
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
        _bb_map: &HashMap<BlockId, BasicBlock<'ctx>>,
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
            MIRInstruction::AddrOf { dest, src } => {
                let src_ptr_val = self.lower_value(src);
                self.bind_dest(dest, src_ptr_val);
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
            MIRInstruction::GetElementPtr {
                dest,
                ptr,
                index,
                elem_ty,
            } => {
                let llvm_elem_ty = self.get_llvmty(elem_ty);
                let ptr_val = self.lower_value(ptr).into_pointer_value();
                let index_val = self.lower_value(index).into_int_value();

                let gep_ptr = unsafe {
                    self.builder
                        .build_in_bounds_gep(llvm_elem_ty, ptr_val, &[index_val], "geptmp")
                        .unwrap()
                };

                self.bind_dest(dest, gep_ptr.into());
            }
            MIRInstruction::Cast {
                dest,
                src,
                from_ty,
                to_ty,
            } => {
                let src_val = self.lower_value(src);
                let target_ty = self.get_llvmty(to_ty);

                let res = match (&from_ty.kind, &to_ty.kind) {
                    //  Integer -> Pointer (inttoptr)
                    (_, MIRTykind::Ptr) if from_ty.is_integer() => self
                        .builder
                        .build_int_to_ptr(
                            src_val.into_int_value(),
                            target_ty.into_pointer_type(),
                            "inttoptr",
                        )
                        .unwrap()
                        .into(),

                    //  Pointer -> Integer (ptrtoint)
                    (MIRTykind::Ptr, _) if to_ty.is_integer() => self
                        .builder
                        .build_ptr_to_int(
                            src_val.into_pointer_value(),
                            target_ty.into_int_type(),
                            "ptrtoint",
                        )
                        .unwrap()
                        .into(),

                    // Integer <-> Integer (Truncate, Sign Extend, Zero Extend)
                    _ if from_ty.is_integer() && to_ty.is_integer() => {
                        let from_bits = from_ty.bit_width();
                        let to_bits = to_ty.bit_width();
                        let int_val = src_val.into_int_value();
                        let target_int_ty = target_ty.into_int_type();

                        if to_bits < from_bits {
                            self.builder
                                .build_int_truncate(int_val, target_int_ty, "trunc")
                                .unwrap()
                                .into()
                        } else if to_bits > from_bits {
                            if from_ty.is_signed() {
                                self.builder
                                    .build_int_s_extend(int_val, target_int_ty, "sext")
                                    .unwrap()
                                    .into()
                            } else {
                                self.builder
                                    .build_int_z_extend(int_val, target_int_ty, "zext")
                                    .unwrap()
                                    .into()
                            }
                        } else {
                            src_val
                        }
                    }

                    // Float -> Integer (fptosi / fptoui)
                    _ if from_ty.is_float() && to_ty.is_integer() => {
                        let float_val = src_val.into_float_value();
                        let target_int_ty = target_ty.into_int_type();

                        if to_ty.is_signed() {
                            self.builder
                                .build_float_to_signed_int(float_val, target_int_ty, "fptosi")
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_float_to_unsigned_int(float_val, target_int_ty, "fptoui")
                                .unwrap()
                                .into()
                        }
                    }

                    // Integer -> Float (sitofp / uitofp)
                    _ if from_ty.is_integer() && to_ty.is_float() => {
                        let int_val = src_val.into_int_value();
                        let target_float_ty = target_ty.into_float_type();

                        if from_ty.is_signed() {
                            self.builder
                                .build_signed_int_to_float(int_val, target_float_ty, "sitofp")
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_unsigned_int_to_float(int_val, target_float_ty, "uitofp")
                                .unwrap()
                                .into()
                        }
                    }

                    // Float -> Float (fpext / fptrunc)
                    _ if from_ty.is_float() && to_ty.is_float() => {
                        let from_bits = from_ty.bit_width();
                        let to_bits = to_ty.bit_width();
                        let float_val = src_val.into_float_value();
                        let target_float_ty = target_ty.into_float_type();

                        if to_bits > from_bits {
                            self.builder
                                .build_float_ext(float_val, target_float_ty, "fpext")
                                .unwrap()
                                .into()
                        } else if to_bits < from_bits {
                            self.builder
                                .build_float_trunc(float_val, target_float_ty, "fptrunc")
                                .unwrap()
                                .into()
                        } else {
                            src_val
                        }
                    }

                    _ => src_val,
                };

                self.bind_dest(dest, res);
            }
            MIRInstruction::BitCast { dest, src, to_ty } => {
                let src_val = self.lower_value(src);
                let target_ty = self.get_llvmty(to_ty);
                let res = self
                    .builder
                    .build_bit_cast(src_val, target_ty, "bitcast")
                    .unwrap();
                self.bind_dest(dest, res);
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
            _ => {
                self.report_ice(format!("Unhandled MIR instruction: {}", inst), None);
            }
        }
    }
}
