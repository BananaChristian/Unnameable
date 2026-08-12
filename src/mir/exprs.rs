use std::collections::HashMap;

use crate::{
    diagnostics::Span,
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral, HirPostfixOp, HirUnaryOp},
    mir::{
        MIRInstruction,
        builder::MIRBuilder,
        instructions::{
            ConstantValue, MIRDollarMode, MIRFn, MIRLinkage, MIROps, MIRParam, MIRTy, MIRTykind,
            MIRValue, Terminator,
        },
    },
};

impl<'a> MIRBuilder<'a> {
    pub fn build_expr(&mut self, expr: &HirExpr) {
        let ty = self.get_type(&expr.hir_id);
        let span = Some(expr.span.clone());
        match &expr.kind {
            HirExprKind::Literal(_) => {
                let src = self.expr_value(expr);
                self.build_assign(src, ty, span, None)
            }
            HirExprKind::Identifier(_) => {
                self.expr_value(expr);
            }
            HirExprKind::Unary(op, operand) => self.build_unary(op, ty, operand),
            HirExprKind::Postfix(operand, op) => self.build_postfix(op, operand, ty),
            HirExprKind::Binary(lhs, operat, rhs) => self.build_bin(operat, lhs, rhs, span, ty),
            HirExprKind::DollarScope { .. } => self.build_dollar_scope(expr),
            HirExprKind::Call(_, _) => self.build_call(expr),
            HirExprKind::StaticCast(_, _) => self.build_cast(expr),
            HirExprKind::BitCast(_, _) => self.build_bitcast(expr),
            _ => todo!(
                "Encountered an expression whose handler is yet to be added {:?}",
                expr
            ),
        }
    }

    fn build_dollar_scope(&mut self, expr: &HirExpr) {
        if let HirExprKind::DollarScope {
            params,
            body,
            result,
        } = &expr.kind
        {
            let span = Some(expr.span.clone());
            let ty = self.get_type(&expr.hir_id);

            let scope_id = self.dollar_scope_counter;
            self.dollar_scope_counter += 1;
            let scope_fn_name = format!("$$scope_{}", scope_id);

            // Evaluate capture arguments in the Parent context before context switch
            let mut eval_args = Vec::new();
            let mut scope_fn_params = Vec::new();

            // Metadata saved for parameter binding inside the inner scope
            let mut param_bindings = Vec::new();

            for param_expr in params {
                let param_ty = self.get_type(&param_expr.hir_id);
                let name = match &param_expr.kind {
                    HirExprKind::Identifier(ident) => ident.clone(),
                    _ => panic!("Expected identifier in dollar scope capture list"),
                };

                // Evaluate the argument in the parent context (loads the external value)
                let arg_val = self.expr_value(param_expr);
                eval_args.push(arg_val);

                // Construct the parameter representation for the target scope function
                scope_fn_params.push(MIRParam {
                    name: name.clone(),
                    ty: param_ty.clone(),
                    dollar_mode: MIRDollarMode::Full,
                });

                param_bindings.push((name, param_ty));
            }

            // Save parent context
            let parent_func = self.current_func;
            let parent_block = self.current_block_id;
            let parent_dollar_mode = self.current_dollar_mode;

            let fn_id = self.alloc_fn_id();
            let entry_block = self.create_basic_block();
            let entry_block_id = entry_block.id;

            let mut blocks = HashMap::new();
            blocks.insert(entry_block_id, entry_block);

            let ret_ty = match result {
                Some(res) => self.get_type(&res.hir_id),
                None => MIRTy {
                    kind: MIRTykind::Unit,
                    size: 0,
                    align: 0,
                },
            };

            let scope_fn = MIRFn {
                fn_id: fn_id.clone(),
                name: scope_fn_name.clone(),
                params: scope_fn_params.clone(), // Pass the populated parameter list
                dollar_mode: MIRDollarMode::Full,
                linkage: MIRLinkage::Private,
                entry_block: entry_block_id,
                blocks,
                ret_ty,
            };

            self.module.functions.insert(fn_id, scope_fn);
            self.current_func = Some(fn_id);
            self.current_block_id = Some(entry_block_id);
            self.current_dollar_mode = MIRDollarMode::Full;
            self.current_dollar_name = Some(scope_fn_name.clone());

            // Set up the inner scope and allocate local memory for parameters
            self.push_scope();

            for (name, param_ty) in param_bindings {
                let param_val = self.new_register(param_ty.clone(), Some(name.as_str()));

                // Allocate local storage for captured arg in $$scope_N frame
                let alloc_dest = self.new_register(
                    MIRTy {
                        kind: MIRTykind::Bool,
                        size: 1,
                        align: 1,
                    },
                    None,
                );
                self.build_alloca(alloc_dest.clone(), param_ty, span.clone());
                self.build_store(alloc_dest.clone(), param_val, span.clone());

                // Bind name in scope map to the local stack pointer (%name.addr)
                self.declare_var(name, alloc_dest);
            }

            for st in body {
                self.build_stmt(st);
            }

            if let Some(final_res) = result {
                let res_val = self.expr_value(final_res);
                self.set_terminator(Terminator::Return(Some(res_val)), span.clone());
            } else {
                self.set_terminator(Terminator::Return(None), span.clone());
            }

            self.pop_scope();

            //  Restore context back to parent
            self.current_func = parent_func;
            self.current_block_id = parent_block;
            self.current_dollar_mode = parent_dollar_mode;

            //  Emit dollar eval with captured arguments
            let dest = self.new_register(ty, None);
            self.build_dollar_eval(dest.clone(), scope_fn_name, eval_args, span);
            self.last_value = Some(dest);
        }
    }

    fn build_call(&mut self, expr: &HirExpr) {
        if let HirExprKind::Call(callee, args) = &expr.kind {
            let name = match &callee.kind {
                HirExprKind::Identifier(s) => s.clone(),
                _ => self.report_ice(
                    "Failed to get callee name as it is not an identifier".to_string(),
                    Some(expr.span.clone()),
                ),
            };

            let ty = self.get_type(&expr.hir_id);

            let mir_args: Vec<MIRValue> = args.iter().map(|a| self.expr_value(a)).collect();
            let dest = self.new_register(ty, Some(name.as_str()));

            let call = MIRInstruction::Call {
                dest: dest.clone(),
                callee: name,
                args: mir_args,
            };
            self.add_instruction(call, Some(expr.span.clone()));

            self.last_value = Some(dest);
        }
    }

    fn build_cast(&mut self, expr: &HirExpr) {
        if let HirExprKind::StaticCast(_, src) = &expr.kind {
            let to_ty = self.get_type(&expr.hir_id);
            let from_ty = self.get_type(&src.hir_id);

            let dest = self.new_register(to_ty.clone(), Some("cast"));
            let src_val = self.expr_value(src);

            let cast_instr = MIRInstruction::Cast {
                dest: dest.clone(),
                src: src_val,
                from_ty,
                to_ty,
            };
            self.add_instruction(cast_instr, Some(expr.span.clone()));
            self.last_value = Some(dest);
        }
    }

    fn build_bitcast(&mut self, expr: &HirExpr) {
        if let HirExprKind::BitCast(_, src) = &expr.kind {
            let to_ty = self.get_type(&expr.hir_id);
            let src_val = self.expr_value(src);
            let dest = self.new_register(to_ty.clone(), Some("bitcast"));
            let bitcast_instr = MIRInstruction::BitCast {
                dest: dest.clone(),
                src: src_val,
                to_ty,
            };
            self.add_instruction(bitcast_instr, Some(expr.span.clone()));
            self.last_value = Some(dest);
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
        if matches!(op, HirBinaryOp::Assign) {
            let rhs_value = self.expr_value(rhs);
            let ptr = self.lookup_ptr(lhs);
            self.build_store(ptr, rhs_value, span);
            return;
        }

        let lhs_value = self.expr_value(lhs);
        let rhs_value = self.expr_value(rhs);

        match op {
            HirBinaryOp::Xor
            | HirBinaryOp::BitAnd
            | HirBinaryOp::BitOr
            | HirBinaryOp::Shl
            | HirBinaryOp::Shr => {
                let op = self.map_bitwise_op(op, &lhs_value);
                self.build_binary(op, lhs_value, rhs_value, ty, span);
            }
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

                self.build_binary(base_op, lhs_value, rhs_value, ty, span.clone());

                let Some(result) = self.last_value.as_ref().cloned() else {
                    self.report_ice("Failed to get last MIRValue".to_string(), span.clone());
                };

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
                };

                let ptr = self.lookup_ptr(operand);
                self.build_store(ptr, new_val.clone(), span);
                self.last_value = Some(new_val)
            }
            HirUnaryOp::BitNot => {
                let neg_val = self.get_corresponding_neg_val(&ty);
                self.build_binary(MIROps::Xor, operand_val, neg_val, ty, span);
            }
            _ => (),
        }
    }

    pub fn expr_value(&mut self, expr: &HirExpr) -> MIRValue {
        let span = Some(expr.span.clone());
        match &expr.kind {
            HirExprKind::Literal(lit) => self.literal_value(lit),

            HirExprKind::Identifier(name) => {
                let Some(ptr) = self.lookup_var(name).cloned() else {
                    self.report_ice(
                        format!(
                            "Variable '{}' not found in symbol table during evaluation",
                            name
                        ),
                        span.clone(),
                    );
                };

                let ty = self.get_type(&expr.hir_id);
                let dest = self.new_register(ty.clone(), None);
                self.build_load(dest.clone(), ptr, ty, span);
                dest
            }

            HirExprKind::StaticCast(_, _) => {
                self.build_cast(expr);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice("Could not get cast value".to_string(), span);
                }
            }
            HirExprKind::BitCast(_, _) => {
                self.build_bitcast(expr);
                if let Some(val)= self.last_value.clone(){
                    val
                }else{
                    self.report_ice("Could not get bitcast value".to_string(), span)
                }
            }

            HirExprKind::Call(_, _) => {
                self.build_call(expr);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice("Could not get call value".to_string(), span);
                }
            }

            HirExprKind::DollarScope { .. } => {
                self.build_dollar_scope(expr);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice(
                        "Dollar scope evaluation failed to produce a result register".to_string(),
                        span,
                    );
                }
            }

            HirExprKind::Binary(lhs, op, rhs) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_bin(op, lhs, rhs, span.clone(), ty);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice(
                        format!(
                            "Binary operation '{:?}' failed to produce a result register",
                            op
                        ),
                        span,
                    );
                }
            }

            // Unary operations (e.g., -x or !x)
            HirExprKind::Unary(op, operand) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_unary(op, ty, operand);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice(
                        format!(
                            "Unary operation '{:?}' failed to produce a result register",
                            op
                        ),
                        span,
                    );
                }
            }

            // Postfix operations (e.g., x++)
            HirExprKind::Postfix(operand, op) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_postfix(op, operand, ty);
                if let Some(val) = self.last_value.clone() {
                    val
                } else {
                    self.report_ice(
                        format!(
                            "Postfix operation '{:?}' failed to produce a result register",
                            op
                        ),
                        span,
                    );
                }
            }

            _ => {
                self.report_ice(
                    format!(
                        "Unsupported expression kind '{:?}' encountered in expr_value",
                        expr.kind
                    ),
                    span,
                );
            }
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
