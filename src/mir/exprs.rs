use std::collections::HashMap;

use crate::{
    diagnostics::Span,
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral, HirPostfixOp, HirUnaryOp},
    mir::{
        MIRGlobal, MIRInstruction,
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
            HirExprKind::Literal(inner) => {
                if let HirLiteral::ArrayLiteral(_) = inner {
                    self.build_array_literal(expr);
                    return;
                }
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
            HirExprKind::Instantiation { .. } => {
                self.build_struct_init(expr);
                return;
            }
            HirExprKind::BitCast(_, _) => self.build_bitcast(expr),
            HirExprKind::Index { .. } => self.build_index_access(expr),
            _ => todo!(
                "Encountered an expression whose handler is yet to be added {:?}",
                expr
            ),
        }
    }

    fn build_index_access(&mut self, expr: &HirExpr) {
        let expr_ty = self.get_type(&expr.hir_id);
        let elem_ptr = self.lookup_ptr(expr);

        if matches!(expr_ty.kind, MIRTykind::Array(_, _)) {
            self.last_value = Some(elem_ptr);
        } else {
            let dest = self.new_register(expr_ty.clone(), None);
            self.build_load(dest.clone(), elem_ptr, expr_ty, Some(expr.span.clone()));
            self.last_value = Some(dest);
        }
    }

    pub fn build_into(&mut self, expr: &HirExpr, dest_ptr: MIRValue) -> bool {
        match expr.kind {
            HirExprKind::Literal(HirLiteral::ArrayLiteral(_)) => {
                self.build_array_literal_into(expr, dest_ptr);
                true
            }
            HirExprKind::Instantiation { .. } => {
                self.build_struct_init_into(expr, dest_ptr);
                true
            }
            _ => false,
        }
    }

    fn build_array_literal(&mut self, expr: &HirExpr) -> MIRValue {
        let array_ty = self.get_type(&expr.hir_id);
        let arr_ptr = self.new_register(array_ty.clone(), None);
        self.build_alloca(arr_ptr.clone(), array_ty, Some(expr.span.clone()));
        self.fill_array_literal(expr, arr_ptr.clone());
        arr_ptr
    }

    pub fn build_array_literal_into(&mut self, expr: &HirExpr, dest_ptr: MIRValue) {
        self.fill_array_literal(expr, dest_ptr);
    }

    fn fill_array_literal(&mut self, expr: &HirExpr, dest_ptr: MIRValue) {
        match &expr.kind {
            HirExprKind::Literal(HirLiteral::ArrayLiteral(elements)) => {
                let array_ty = self.get_type(&expr.hir_id);
                let elem_ty = match &array_ty.kind {
                    MIRTykind::Array(elem_ty, _) => elem_ty.as_ref().clone(),
                    _ => self.report_ice(
                        "Array literal does not have array type".to_string(),
                        Some(expr.span.clone()),
                    ),
                };

                for (index, elem_expr) in elements.iter().enumerate() {
                    let index_val = MIRValue::Constant(ConstantValue::UInt(index));
                    self.build_gep_single(
                        dest_ptr.clone(),
                        index_val,
                        elem_ty.clone(),
                        Some(expr.span.clone()),
                    );
                    let elem_ptr = self.get_last_val(Some(expr.span.clone()));

                    if matches!(
                        &elem_expr.kind,
                        HirExprKind::Literal(HirLiteral::ArrayLiteral(_))
                    ) {
                        self.fill_array_literal(elem_expr, elem_ptr);
                    } else {
                        let elem_val = self.expr_value(elem_expr);
                        self.build_store(
                            elem_ptr,
                            elem_val,
                            elem_ty.align,
                            Some(expr.span.clone()),
                        );
                    }
                }
            }
            _ => self.report_ice("Not an array literal".to_string(), Some(expr.span.clone())),
        }
    }

    pub fn build_struct_init(&mut self, expr: &HirExpr) -> MIRValue {
        let struct_ty = self.get_type(&expr.hir_id);
        let struct_ptr = self.new_register(struct_ty.clone(), None);
        self.build_alloca(struct_ptr.clone(), struct_ty, Some(expr.span.clone()));
        self.fill_struct_init(expr, struct_ptr.clone());
        struct_ptr
    }

    pub fn build_struct_init_into(&mut self, expr: &HirExpr, dest_ptr: MIRValue) {
        self.fill_struct_init(expr, dest_ptr);
    }

    fn fill_struct_init(&mut self, expr: &HirExpr, dest_ptr: MIRValue) {
        if let HirExprKind::Instantiation { body, .. } = &expr.kind {
            let struct_ty = self.get_type(&expr.hir_id);
            let (struct_id, struct_name) = match &struct_ty.kind {
                MIRTykind::Struct(id, name) => (*id, name.clone()),
                _ => {
                    self.report_ice(
                        "Struct instantiation does not have struct type".to_string(),
                        Some(expr.span.clone()),
                    );
                }
            };

            let struct_decl = match self.module.structs.get(&struct_id) {
                Some(decl) => decl.clone(),
                None => {
                    self.report_ice(
                        format!("Failed to get struct declaration for ID: {:?}", struct_id),
                        Some(expr.span.clone()),
                    );
                }
            };

            for field in body {
                let (field_idx, (_, field_ty)) = match struct_decl
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name == &field.name)
                {
                    Some(res) => res,
                    None => {
                        self.report_ice(
                            format!("Unknown field '{}' in struct {}", field.name, struct_name),
                            Some(expr.span.clone()),
                        );
                    }
                };

                // Struct GEP requires 2 indices: [0 (pointer deref), field_idx (field select)]
                let zero_idx = MIRValue::Constant(ConstantValue::UInt(0));
                let field_idx_val = MIRValue::Constant(ConstantValue::UInt(field_idx));

                self.build_gep(
                    dest_ptr.clone(),
                    vec![zero_idx, field_idx_val], // Pass both indices!
                    struct_ty.clone(),
                    Some(expr.span.clone()),
                );
                let field_ptr = self.get_last_val(Some(expr.span.clone()));

                // Recursively fill nested aggregates, or store scalar value directly
                match &field.value.kind {
                    HirExprKind::Instantiation { .. } => {
                        self.fill_struct_init(&field.value, field_ptr);
                    }
                    HirExprKind::Literal(HirLiteral::ArrayLiteral(_)) => {
                        self.fill_array_literal(&field.value, field_ptr);
                    }
                    _ => {
                        let field_val = self.expr_value(&field.value);
                        self.build_store(
                            field_ptr,
                            field_val,
                            field_ty.align,
                            Some(expr.span.clone()),
                        );
                    }
                }
            }
        } else {
            self.report_ice(
                "Expected struct instantiation expression".to_string(),
                Some(expr.span.clone()),
            );
        }
    }
    fn build_string_literal(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Literal(lit) => match lit {
                HirLiteral::Str(string) => {
                    let bytes = string.as_bytes();
                    let len = bytes.len() + 1; //Cater for the null literal

                    let char8_ty = MIRTy {
                        kind: MIRTykind::CHAR8,
                        size: 1,
                        align: 1,
                    };
                    let array_ty = MIRTy {
                        kind: MIRTykind::Array(Box::new(char8_ty.clone()), len),
                        size: len,
                        align: 1,
                    };
                    let global_id = self.alloc_global_id();
                    let global_name = format!(".str.{}", global_id.0);

                    let mut const_elements: Vec<ConstantValue> =
                        bytes.iter().map(|&b| ConstantValue::Char8(b)).collect();
                    const_elements.push(ConstantValue::Char8(0));
                    let global_init = MIRValue::Constant(ConstantValue::Array(const_elements));

                    let global = MIRGlobal {
                        global_id: global_id.clone(),
                        name: global_name.clone(),
                        ty: array_ty.clone(),
                        dollar_mode: self.current_dollar_mode,
                        is_const: true,
                        init: global_init,
                        linkage: MIRLinkage::Private,
                    };

                    self.module.globals.insert(global_id.clone(), global);

                    self.last_value = Some(MIRValue::Global(global_id));
                }
                _ => self.report_ice(
                    "Invalid string literal".to_string(),
                    Some(expr.span.clone()),
                ),
            },

            _ => self.report_ice(
                "Invalid string literal".to_string(),
                Some(expr.span.clone()),
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
                self.build_alloca(alloc_dest.clone(), param_ty.clone(), span.clone());
                let align = param_ty.align;

                self.build_store(alloc_dest.clone(), param_val, align, span.clone());

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
            let ptr = self.lookup_ptr(lhs);

            if matches!(&rhs.kind, HirExprKind::Literal(HirLiteral::ArrayLiteral(_))) {
                self.build_array_literal_into(rhs, ptr);
            } else {
                let rhs_value = self.expr_value(rhs);
                let align = self.get_alignment(lhs);
                self.build_store(ptr, rhs_value, align, span);
            }
            return;
        }

        let lhs_value = self.expr_value(lhs);
        let rhs_value = self.expr_value(rhs);

        let left_ty = self.get_type(&lhs.hir_id);
        let right_ty = self.get_type(&rhs.hir_id);

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
                if left_ty.is_pointer() || right_ty.is_pointer() {
                    self.build_pointer_arithmetic(
                        op,
                        lhs,
                        lhs_value.clone(),
                        rhs,
                        rhs_value.clone(),
                        span.clone(),
                    );
                    return;
                }
                let mir_op = self.map_arithmetic_op(op, &lhs_value);
                self.build_binary(mir_op, lhs_value, rhs_value, ty, span)
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
                let align = self.get_alignment(lhs);
                self.build_store(ptr, result, align, span);
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
        let align = self.get_alignment(operand);
        self.build_store(ptr, new_val, align, span);

        self.last_value = Some(old_val)
    }

    pub fn build_unary(&mut self, op: &HirUnaryOp, ty: MIRTy, operand: &HirExpr) {
        let span = Some(operand.span.clone());

        match op {
            HirUnaryOp::Not => {
                let operand_val = self.expr_value(operand);
                let true_val = MIRValue::Constant(ConstantValue::Bool(true));
                self.build_binary(MIROps::Xor, operand_val, true_val, ty, span);
            }
            HirUnaryOp::Neg => {
                let operand_val = self.expr_value(operand);
                let zero_val = MIRValue::Constant(ConstantValue::Int(0));
                self.build_binary(MIROps::Sub, zero_val, operand_val, ty, span);
            }
            HirUnaryOp::Increment | HirUnaryOp::Decrement => {
                let operand_val = self.expr_value(operand);
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
                let align = self.get_alignment(operand);
                self.build_store(ptr, new_val.clone(), align, span);
                self.last_value = Some(new_val);
            }
            HirUnaryOp::BitNot => {
                let operand_val = self.expr_value(operand);
                let neg_val = self.get_corresponding_neg_val(&ty);
                self.build_binary(MIROps::Xor, operand_val, neg_val, ty, span);
            }
            HirUnaryOp::AddressOf => {
                let src_ptr = self.lookup_ptr(operand);
                let dest = self.new_register(ty, None);
                let addr_of_instruction = MIRInstruction::AddrOf {
                    dest: dest.clone(),
                    src: src_ptr,
                };
                self.add_instruction(addr_of_instruction, span);
                self.last_value = Some(dest);
            }
            HirUnaryOp::Dereference => {
                let ptr_val = self.expr_value(operand);
                let dest = self.new_register(ty.clone(), None);
                self.build_load(dest.clone(), ptr_val, ty, span);
                self.last_value = Some(dest);
            }
        }
    }

    pub fn expr_value(&mut self, expr: &HirExpr) -> MIRValue {
        let span = Some(expr.span.clone());
        match &expr.kind {
            HirExprKind::Literal(lit) => {
                if let HirLiteral::ArrayLiteral(_) = lit {
                    return self.build_array_literal(expr);
                }
                self.literal_value(expr)
            }

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
                self.get_last_val(Some(expr.span.clone()))
            }

            HirExprKind::Call(_, _) => {
                self.build_call(expr);
                self.get_last_val(Some(expr.span.clone()))
            }

            HirExprKind::DollarScope { .. } => {
                self.build_dollar_scope(expr);
                self.get_last_val(Some(expr.span.clone()))
            }

            HirExprKind::Binary(lhs, op, rhs) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_bin(op, lhs, rhs, span.clone(), ty);
                self.get_last_val(Some(expr.span.clone()))
            }

            // Unary operations (e.g., -x or !x)
            HirExprKind::Unary(op, operand) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_unary(op, ty, operand);
                self.get_last_val(Some(expr.span.clone()))
            }

            // Postfix operations (e.g., x++)
            HirExprKind::Postfix(operand, op) => {
                let ty = self.get_type(&expr.hir_id);
                self.build_postfix(op, operand, ty);
                self.get_last_val(Some(expr.span.clone()))
            }

            HirExprKind::Index { .. } => {
                self.build_index_access(expr);
                self.get_last_val(Some(expr.span.clone()))
            }

            HirExprKind::Instantiation { .. } => self.build_struct_init(expr),

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

    fn literal_value(&mut self, expr: &HirExpr) -> MIRValue {
        let mir_ty = self.get_type(&expr.hir_id);
        let span = Some(expr.span.clone());

        match &expr.kind {
            HirExprKind::Literal(lit) => match lit {
                HirLiteral::Int8(v) => MIRValue::Constant(ConstantValue::I8(*v)),
                HirLiteral::Int16(v) => MIRValue::Constant(ConstantValue::I16(*v)),
                HirLiteral::Int32(v) => MIRValue::Constant(ConstantValue::I32(*v)),
                HirLiteral::Int64(v) => MIRValue::Constant(ConstantValue::I64(*v)),
                HirLiteral::IntSize(v) => MIRValue::Constant(ConstantValue::Int(*v)),
                HirLiteral::Int128(v) => MIRValue::Constant(ConstantValue::I128(*v)),

                HirLiteral::Uint8(v) => MIRValue::Constant(ConstantValue::U8(*v)),
                HirLiteral::Uint16(v) => MIRValue::Constant(ConstantValue::U16(*v)),
                HirLiteral::Uint32(v) => MIRValue::Constant(ConstantValue::U32(*v)),
                HirLiteral::Uint64(v) => MIRValue::Constant(ConstantValue::U64(*v)),
                HirLiteral::UintSize(v) => MIRValue::Constant(ConstantValue::UInt(*v)),
                HirLiteral::Uint128(v) => MIRValue::Constant(ConstantValue::U128(*v)),

                HirLiteral::F32(v) => MIRValue::Constant(ConstantValue::F32(*v)),
                HirLiteral::F64(v) => MIRValue::Constant(ConstantValue::F64(*v)),
                HirLiteral::Bool(v) => MIRValue::Constant(ConstantValue::Bool(*v)),

                HirLiteral::Int(v) => match &mir_ty.kind {
                    MIRTykind::I8 => MIRValue::Constant(ConstantValue::I8(*v as i8)),
                    MIRTykind::I16 => MIRValue::Constant(ConstantValue::I16(*v as i16)),
                    MIRTykind::I32 => MIRValue::Constant(ConstantValue::I32(*v as i32)),
                    MIRTykind::I64 => MIRValue::Constant(ConstantValue::I64(*v as i64)),
                    MIRTykind::I128 => MIRValue::Constant(ConstantValue::I128(*v as i128)),
                    MIRTykind::ISIZE => MIRValue::Constant(ConstantValue::Int(*v as isize)),

                    MIRTykind::U8 => MIRValue::Constant(ConstantValue::U8(*v as u8)),
                    MIRTykind::U16 => MIRValue::Constant(ConstantValue::U16(*v as u16)),
                    MIRTykind::U32 => MIRValue::Constant(ConstantValue::U32(*v as u32)),
                    MIRTykind::U64 => MIRValue::Constant(ConstantValue::U64(*v as u64)),
                    MIRTykind::U128 => MIRValue::Constant(ConstantValue::U128(*v as u128)),
                    MIRTykind::USIZE => MIRValue::Constant(ConstantValue::UInt(*v as usize)),

                    _ => {
                        self.report_ice(
                            format!(
                                "Integer literal {:?} was assigned non-integer type {:?}",
                                lit, mir_ty.kind
                            ),
                            span,
                        );
                    }
                },

                // Generic Unsuffixed Float Literals
                HirLiteral::Float(v) => match &mir_ty.kind {
                    MIRTykind::F32 => MIRValue::Constant(ConstantValue::F32(*v as f32)),
                    MIRTykind::F64 => MIRValue::Constant(ConstantValue::F64(*v)),
                    _ => {
                        self.report_ice(
                            format!(
                                "Float literal {:?} was assigned non-float type {:?}",
                                lit, mir_ty.kind
                            ),
                            span,
                        );
                    }
                },
                HirLiteral::Char8(c) => MIRValue::Constant(ConstantValue::Char8(*c)),
                HirLiteral::Char16(c) => MIRValue::Constant(ConstantValue::Char16(*c)),
                HirLiteral::Char32(c) => MIRValue::Constant(ConstantValue::Char32(*c as u32)),

                HirLiteral::Str(_) => {
                    self.build_string_literal(expr);
                    self.get_last_val(span)
                }

                _ => todo!("Handle other constants"),
            },

            _ => {
                self.report_ice(
                    format!(
                        "Non-literal expression passed to literal_value: {:?}",
                        expr.kind
                    ),
                    span,
                );
            }
        }
    }
}
