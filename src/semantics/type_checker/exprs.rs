use crate::{
    diagnostics::Span, hir::{HirBinaryOp, HirExpr, HirExprKind, HirLiteral, HirPostfixOp, HirUnaryOp}, lowering::NodeId, semantics::{
        semantics::{InstanceKey, ResolvedTypeKind, TypeInfo},
        type_checker::checker::TypeChecker,
    }
};

impl<'a> TypeChecker<'a> {
    pub fn check_expr(&mut self, expr: &HirExpr) {
        self.expr_type(expr);
    }

    pub fn expr_type(&mut self, expr: &HirExpr) -> TypeInfo {
        let ty = match &expr.kind {
            HirExprKind::SizeOf(_) => self.primitive(ResolvedTypeKind::USize, expr.span.clone()),
            HirExprKind::Identifier(_) => {
                self.look_up_declared_type(expr.hir_id, expr.span.clone())
            }
            HirExprKind::Call(_, _) => self.call_type(expr),
            HirExprKind::Literal(_) => self.literal_type(expr),
            HirExprKind::Binary(_, _, _) => self.binary_type(expr),
            HirExprKind::StaticCast(_, _) => self.cast_type(expr),
            HirExprKind::BitCast(_, _) => self.bitcast_type(expr),
            HirExprKind::Index { ..} => self.index_type(expr),
            HirExprKind::Unary(_,_ ) => self.unary_type(expr),
            HirExprKind::Postfix(_,_ ) => self.postfix_type(expr),
            HirExprKind::GenericInstantion { .. }=> self.gen_inst_type(expr),
            _ => self.unknown(expr.span.clone()),
        };
        self.insert(expr.hir_id, ty.clone());
        ty
    }

    fn gen_inst_type(&mut self, expr: &HirExpr)-> TypeInfo{
        if let  HirExprKind::GenericInstantion {type_params ,..} =  &expr.kind{
            let template_decl_id: NodeId = *self.ctxt.names.resolved.get(&expr.hir_id)
            .expect("Name resolver missing mapping for generic instantiation");

            let template_info = self.get_decl_type(&template_decl_id, expr.span.clone());

            let concrete_args: Vec<TypeInfo> = type_params
            .iter()
            .map(|param_node| self.type_from_hir_type(param_node))
            .collect();

            if !concrete_args.is_empty() {
                let key = InstanceKey {
                    original_def_id: template_decl_id,
                    concrete_args: concrete_args.clone(),
                };
                self.ctxt.monomorph_backlog.insert(key);
            }

            let specialized_type = self.specialize_signature(&template_info, &concrete_args, expr.span.clone());
    
            self.insert(expr.hir_id, specialized_type.clone());
            specialized_type
        }else{
            self.unknown(expr.span.clone())
        }
    }

    fn index_type(&mut self,expr: &HirExpr) -> TypeInfo{
        if let HirExprKind::Index { target, index } =  &expr.kind{
            let target_ty= self.expr_type(target);
            let index_ty= self.expr_type(index);

            if !self.is_numeric(&index_ty) {
                self.report(format!("Invalid index type '{}' array indexes must be integers",index_ty.name), Some(expr.span.clone()));
                return self.unknown(expr.span.clone());
            }

            match &target_ty.kind{
                ResolvedTypeKind::Array { inner, .. } => {
                    *inner.clone()
                }
                ResolvedTypeKind::Pointer { inner } => {
                    *inner.clone()
                }
                _ => {
                    self.report(format!("Cannot index into a non indexable type '{}'",target_ty.name), Some(expr.span.clone()));
                    self.unknown(expr.span.clone())
                }
            }

        }else{
            self.unknown(expr.span.clone())
        }
    }


    fn unary_type(&mut self,expr: &HirExpr) -> TypeInfo{
        if let HirExprKind::Unary(op, target) = &expr.kind{
            let target_ty= self.expr_type(target);
            match op{
                HirUnaryOp::Dereference => self.deref_type(&target_ty),
                HirUnaryOp::AddressOf => self.address_of_type(&target_ty),
                HirUnaryOp::Increment|HirUnaryOp::Decrement => self.inc_dec_type(&target_ty), 
                HirUnaryOp::Neg => self.neg_type(&target_ty),
                HirUnaryOp::Not => self.logical_not_type(&target_ty),
            }
        }else{
            self.unknown(expr.span.clone())
        }

    }

    fn neg_type(&mut self, ty: &TypeInfo) -> TypeInfo{
        if self.is_signed_numeric(ty){
            ty.clone()
        }else{
            self.report(format!("Cannot apply '-' to type '{}'",ty.name), Some(ty.span.clone()));
            self.unknown(ty.span.clone())
        }
    }

    fn logical_not_type(&mut self,ty: &TypeInfo) -> TypeInfo{
        match ty.kind{
            ResolvedTypeKind::Bool  => ty.clone(),
            _ => {
                self.report("Operator '!' can only be applied to 'bool'".to_string(), Some(ty.span.clone()));
                self.unknown(ty.span.clone())
            }
        }
    }

    fn inc_dec_type(&mut self, target_ty: &TypeInfo) ->  TypeInfo {
        if !self.is_numeric(&target_ty){
         self.report(format!("Cannot apply operator to numeric type '{}'",target_ty.name), Some(target_ty.span.clone()));
          self.unknown(target_ty.span.clone())
        }else{
         target_ty.clone()
        }
    }

    fn deref_type(&mut self,src_ty: &TypeInfo) -> TypeInfo{
        match &src_ty.kind{
            ResolvedTypeKind::Pointer { inner } => {
                *inner.clone()
            }
            _ => self.unknown(src_ty.span.clone())
        }
    }

    fn address_of_type(&mut self, src_ty: &TypeInfo) -> TypeInfo{
        let ptr_kind=ResolvedTypeKind::Pointer { inner: Box::new(src_ty.clone()) };
        let ptr_id= self.registry.issue_id(ptr_kind.clone());
        let ptr_layout= self.layout_engine.layout_of(&ptr_kind, ptr_id.clone(), src_ty.span.clone());
        TypeInfo{
            type_id: ptr_id,
            name: TypeInfo::name(ptr_kind.clone()),
            kind: ptr_kind,
            layout:ptr_layout,
            span: src_ty.span.clone(),
        }

    }

    fn cast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::StaticCast(target, src_expr) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);
            let src_ty=self.expr_type(src_expr);

            let allowed = match (&src_ty.kind, &target_ty.kind) {
            (_,_) if self.is_numeric(&src_ty) && self.is_numeric(&target_ty) => true,

            (ResolvedTypeKind::Enum { .. }, t) if self.is_integer(t) => true,

            // Pointer to Pointer address reassignment
            (ResolvedTypeKind::Pointer{..}, ResolvedTypeKind::Pointer { .. }) => true,

            // Pointer value conversions to raw address tracking limits
            (ResolvedTypeKind::Pointer{..}, ResolvedTypeKind::USize) => true,
            (ResolvedTypeKind::USize, ResolvedTypeKind::Pointer{..}) => true,

            _ => false,
        };

            if !allowed{
                self.report(
                    format!("Invalid cast cannot convert '{}' to '{}'",
                    src_ty.name,
                    target_ty.name),
                    Some(expr.span.clone()));
                self.unknown(expr.span.clone())
            }else{

            target_ty

            }

            //Will have to apply some casting rules here
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn bitcast_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::BitCast(target, src_expr) = &expr.kind {
            let target_ty = self.type_from_hir_type(target);
            let src_ty = self.expr_type(src_expr);

            if src_ty.layout.size != target_ty.layout.size {
                self.report(
                    format!("bitcast size mismatch, cannot reinterpret '{}' ({} bytes) as '{}' ({} bytes), ensure sizes match",
                    src_ty.name,
                    src_ty.layout.size,
                    target_ty.name,
                    target_ty.layout.size), 
                    Some(expr.span.clone()));
                self.unknown(expr.span.clone())
            } else {
                target_ty
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn postfix_type(&mut self,expr: &HirExpr) -> TypeInfo{
        if let HirExprKind::Postfix(inner, op) = &expr.kind{
            let inner_ty= self.expr_type(inner);
            match op{
                HirPostfixOp::Increment|HirPostfixOp::Decrement => inner_ty,
                _ => self.unknown(expr.span.clone())
            }

        }else{
            self.unknown(expr.span.clone())
        }
    }

    fn binary_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Binary(left, op, right) = &expr.kind {
            let left_ty = self.expr_type(left);
            let right_ty = self.expr_type(right);
            match op {
                HirBinaryOp::Add
                | HirBinaryOp::Sub
                | HirBinaryOp::Div
                | HirBinaryOp::Mul
                | HirBinaryOp::Mod => self.arithmetic_type(left_ty, right_ty, expr.span.clone()),
                HirBinaryOp::Eq
                | HirBinaryOp::Neq
                | HirBinaryOp::Lt
                | HirBinaryOp::Gt
                | HirBinaryOp::Geq
                | HirBinaryOp::Leq => self.comparison_binary_type(&left_ty, &right_ty, expr.span.clone()),
                HirBinaryOp::And | HirBinaryOp::Or => self.logical_binary_type(&left_ty,&right_ty,expr.span.clone()),
                HirBinaryOp::Assign => self.assignment_type(&left_ty, &right_ty, expr.span.clone()),
                HirBinaryOp::Access => self.access_type(&left_ty, right),
                _ => self.unknown(expr.span.clone()),
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn access_type(&mut self,left_ty: &TypeInfo, field_expr: &HirExpr) -> TypeInfo{
        let field_name= match &field_expr.kind{
            HirExprKind::Identifier(name) => name,
            _ => {
                self.report("The left of a field access must be an identifier".to_string(), Some(field_expr.span.clone()));
                &"".to_string()
            }
        };

        match &left_ty.kind{
            ResolvedTypeKind::Struct { name,members ,..}| ResolvedTypeKind::Enum {name, members,..} =>{
                if let Some(member_tuple)=members.iter().find(|m|m.0==*field_name){
                    member_tuple.1.clone()
                }else{
                    self.unknown_member(field_name, name, field_expr.span.clone());
                    self.unknown(field_expr.span.clone())
                }
            },
            ResolvedTypeKind::Variant { name, arms ,..} =>{
                if let Some(arms_tuple)= arms.iter().find(|m|m.0 == *field_name){
                    arms_tuple.1.clone()
                }else{
                    self.unknown_member(field_name, name, field_expr.span.clone());
                    self.unknown(field_expr.span.clone())
                }
            }
            ResolvedTypeKind::Anonymous { fields } => {
                if let Some(fields_tuple)= fields.iter().find(|m|m.0 == *field_name){
                    fields_tuple.1.clone()
                }else{
                    self.report(format!("Unknown member '{}' in anonymous struct",field_name), Some(field_expr.span.clone()));
                    self.unknown(field_expr.span.clone())
                }

            }
            _ => {
                self.report(format!("Cannot carryout an access operation on type '{}'",left_ty.name), Some(field_expr.span.clone()));
                self.unknown(field_expr.span.clone())
            }

        }
    }

    fn assignment_type(&mut self, left_ty: &TypeInfo, right_ty: &TypeInfo,span:Span) -> TypeInfo{
        if  !TypeInfo::types_match(left_ty, right_ty){
            self.type_mismatch(left_ty, right_ty, span.clone());
            self.unknown(span)
        }else{
            left_ty.clone()
        }

    }

    fn comparison_binary_type(&mut self, left_ty: &TypeInfo,right_ty: &TypeInfo,span:Span)-> TypeInfo{
        if !TypeInfo::types_match(left_ty, right_ty){
            self.type_mismatch(left_ty, right_ty, span.clone());
            self.unknown(span.clone())
        }else{
            self.boolean(span)
        }


    }

    fn logical_binary_type(&mut self, left_ty: &TypeInfo,right_ty: &TypeInfo, span: Span)-> TypeInfo{
        if right_ty.kind != ResolvedTypeKind::Bool || left_ty.kind != ResolvedTypeKind::Bool{
            self.report(
                format!("logical binary operator cannot be applied to types '{}' and '{}'",
                    left_ty.name, 
                    right_ty.name), 
                Some(span.clone()));
            self.unknown(span.clone())
        }else{
            self.boolean(span)
        }
    }

    fn call_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Call(name, args) = &expr.kind {
            let overall_ty = self.expr_type(name);
            match &overall_ty.kind {
                ResolvedTypeKind::Func { params, ret_type ,..} => {
                    if args.len() != params.len() {
                        self.report(
                            format!(
                                "Invalid argument count expected '{}' but got '{}'",
                                params.len(),
                                args.len()
                            ),
                            Some(expr.span.clone()),
                        );
                    }

                    for (arg, param_ty) in args.iter().zip(params.iter()) {
                        let arg_ty = self.expr_type(arg);
                        if !TypeInfo::types_match(&arg_ty, param_ty) {
                            self.type_mismatch(&arg_ty, param_ty, expr.span.clone());
                        }
                    }

                    *ret_type.clone()
                }
                _ => self.unknown(expr.span.clone()),
            }
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn arithmetic_type(&mut self, left_ty: TypeInfo, right_ty: TypeInfo, span: Span) -> TypeInfo {
        // both must be numeric
        if !self.is_numeric(&left_ty) {
            self.report(
                "Left operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return self.unknown(span.clone());
        }

        if !self.is_numeric(&right_ty) {
            self.report(
                "Right operand of arithmetic operation must be numeric".to_string(),
                Some(span.clone()),
            );
            return self.unknown(span.clone());
        }

        if !TypeInfo::types_match(&left_ty, &right_ty) {
            self.type_mismatch(&left_ty, &right_ty, span.clone());
            return self.unknown(span.clone());
        }

        left_ty
    }

    fn literal_type(&mut self, expr: &HirExpr) -> TypeInfo {
        if let HirExprKind::Literal(lit) = &expr.kind {
            let ty = match lit {
                HirLiteral::Int8(_) => self.primitive(ResolvedTypeKind::I8, expr.span.clone()),
                HirLiteral::Uint8(_) => self.primitive(ResolvedTypeKind::U8, expr.span.clone()),
                HirLiteral::Int16(_) => self.primitive(ResolvedTypeKind::I16, expr.span.clone()),
                HirLiteral::Uint16(_) => self.primitive(ResolvedTypeKind::U16, expr.span.clone()),
                HirLiteral::Int32(_) => self.primitive(ResolvedTypeKind::I32, expr.span.clone()),
                HirLiteral::Uint32(_) => self.primitive(ResolvedTypeKind::U32, expr.span.clone()),
                HirLiteral::Int64(_) => self.primitive(ResolvedTypeKind::I64, expr.span.clone()),
                HirLiteral::Uint64(_) => self.primitive(ResolvedTypeKind::U64, expr.span.clone()),
                HirLiteral::Int128(_) => self.primitive(ResolvedTypeKind::I128, expr.span.clone()),
                HirLiteral::Uint128(_) => self.primitive(ResolvedTypeKind::U128, expr.span.clone()),
                HirLiteral::IntSize(_) => {
                    self.primitive(ResolvedTypeKind::ISize, expr.span.clone())
                }
                HirLiteral::UintSize(_) => {
                    self.primitive(ResolvedTypeKind::USize, expr.span.clone())
                }
                HirLiteral::Int(_) => self.primitive(ResolvedTypeKind::USize, expr.span.clone()),
                HirLiteral::Float(_) => self.primitive(ResolvedTypeKind::F32, expr.span.clone()),
                HirLiteral::F32(_) => self.primitive(ResolvedTypeKind::F32, expr.span.clone()),
                HirLiteral::F64(_) => self.primitive(ResolvedTypeKind::F64, expr.span.clone()),
                HirLiteral::Bool(_) => self.primitive(ResolvedTypeKind::Bool, expr.span.clone()),
                HirLiteral::Null => {
                    let inner_ty = self.unknown(expr.span.clone());
                    self.primitive(
                        ResolvedTypeKind::Nullable {
                            ty: Box::new(inner_ty),
                        },
                        expr.span.clone(),
                    )
                }
                HirLiteral::ArrayLiteral(elements) => {
                    self.check_array_literal(elements, expr.span.clone())
                }
            };

            ty
        } else {
            self.unknown(expr.span.clone())
        }
    }

    fn check_array_literal(&mut self, elements: &Vec<HirExpr>, span: Span) -> TypeInfo {
        if elements.is_empty() {
            self.unknown(span)
        } else {
            let first_ty = self.expr_type(&elements[0]);
            let mut all_okay = true;
            for element in elements.iter().skip(1) {
                let element_ty = self.expr_type(element);
                if !TypeInfo::types_match(&first_ty, &element_ty) {
                    self.report(
                        format!("array elements must all have the same type"),
                        Some(span.clone()),
                    );
                    all_okay = false;
                }
            }

            if all_okay {
                self.array(first_ty, Some(elements.len() as u64), span)
            } else {
                self.unknown(span)
            }
        }
    }
}
