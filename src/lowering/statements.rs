use crate::{
    ast::{EnumMember, Stmt, StmtKind, VariantMember},
    diagnostics::Span,
    hir::{
        HirBinaryOp, HirEnumMember, HirExpr, HirExprKind, HirLiteral, HirParam, HirStmt,
        HirStmtKind, HirType, HirTypeNode, HirVariantMember,
    },
    lowering::lowering::Lowering,
};

impl Lowering{
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match stmt.kind {
            StmtKind::VarDecl { .. } => self.lower_var(stmt),
            StmtKind::FunctionDef { .. } => self.lower_func_def(stmt),
            StmtKind::FunctionDecl { .. } => self.lower_func_decl(stmt),
            StmtKind::StructDecl { .. } => self.lower_struct(stmt),
            StmtKind::EnumStmt { .. } => self.lower_enum(stmt),
            StmtKind::VariantStmt { .. } => self.lower_variant(stmt),
            StmtKind::WhileStmt { .. } => self.lower_while(stmt),
            StmtKind::ContractBlock { .. } => self.lower_contract(stmt),
            StmtKind::Return(..) => self.lower_return(stmt),
            StmtKind::Break | StmtKind::Continue => self.lower_break_or_cont(stmt),
            StmtKind::IfStmt { .. } => self.lower_if(stmt),
            StmtKind::AliasStmt { .. } => self.lower_alias(stmt),
            StmtKind::ImportStmt { .. } => self.lower_import(stmt),
            _ => self.lower_expr_stmt(stmt),
        }
    }

    fn lower_expr_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::Expr(expr) = &stmt.kind {
            let lowered_expr = self.lower_expr(expr)?;
            let hir_stmt = HirStmt {
                hir_id: lowered_expr.hir_id,
                kind: HirStmtKind::HirExpr(lowered_expr),
                span: stmt.span.clone(),
            };

            Some(hir_stmt)
        } else {
            self.report("Unknown statement".to_string(), Some(stmt.span.clone()));
            None
        }
    }

    pub fn make_var(
        &mut self,
        name: String,
        mutable: bool,
        init: Option<HirExpr>,
        span: Span,
    ) -> HirStmt {
        HirStmt {
            hir_id: self.next_id(),
            kind: HirStmtKind::HirVarDecl {
                name,
                mutable,
                constant: false,
                heap: false,
                exposed: false,
                ty: None,
                init,
            },
            span,
        }
    }

    fn lower_var(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::VarDecl {
            qualifiers,
            type_annotation,
            name,
            init,
        } = &stmt.kind
        {
            // Extract name string from Expr::Identifier
            let name_str = self.extract_name_string(name)?;

            // Extract qualifiers into a booleans map
            let qualifier_map = self.map_qualifiers(qualifiers);

            // Lower the type annotation if present
            let ty = match type_annotation {
                Some(t) => Some(self.lower_type(t)?),
                None => None,
            };

            // Lower the init expression if present
            let init = match init {
                Some(e) => Some(self.lower_expr(e)?),
                None => None,
            };

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirVarDecl {
                    name: name_str,
                    mutable: qualifier_map.mutable,
                    constant: qualifier_map.constant,
                    heap: qualifier_map.heap,
                    exposed: qualifier_map.expose,
                    ty,
                    init,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_func_def(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::FunctionDef {
            qualifiers,
            name,
            params,
            type_annotation,
            body,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);

            // Extract and mangle name
            let name_str = self.extract_name_string(name)?;

            // Lower params
            let hir_params = params
                .iter()
                .map(|p| self.lower_param(p))
                .collect::<Option<Vec<_>>>()?;

            let return_type = match type_annotation {
                Some(t) => self.lower_type(t)?,
                None => HirTypeNode::unit(self.next_id(), stmt.span.clone()),
            };

            let hir_body = self.lower_block(body)?;

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirFunctionDef {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    generic_type_params: Vec::new(),
                    exposed: map.expose,
                    body: hir_body,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_func_decl(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::FunctionDecl {
            qualifiers,
            name,
            params,
            type_annotation,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);

            let name_str = self.extract_name_string(name)?;

            let hir_params = params
                .iter()
                .map(|p| self.lower_param(p))
                .collect::<Option<Vec<_>>>()?;

            let return_type = match type_annotation {
                Some(t) => self.lower_type(t)?,
                None => HirTypeNode::unit(self.next_id(), stmt.span.clone()),
            };

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirFunctionDecl {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    generic_type_params: Vec::new(),
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_param(&mut self, stmt: &Stmt) -> Option<HirParam> {
        if let StmtKind::ParamDecl {
            qualifiers,
            name,
            type_annotation,
            def,
        } = &stmt.kind
        {
            let name_str = self.extract_name_string(name)?;
            let qualifier_map = self.map_qualifiers(qualifiers);
            let ty = self.lower_type(type_annotation)?;
            let default = match def {
                Some(e) => Some(self.lower_expr(e)?),
                None => None,
            };
            Some(HirParam {
                hir_id: self.next_id(),
                name: name_str,
                ty,
                mutable: qualifier_map.mutable,
                default,
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_struct(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::StructDecl {
            qualifiers,
            name,
            contracts,
            contents,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);
            let name_str = self.extract_name_string(name)?;
            let mut extracted_contracts = Vec::new();
            for contract in contracts {
                let cont = self.lower_type(contract)?;
                extracted_contracts.push(cont);
            }
            let fields = if let StmtKind::Block { content } = &contents.kind {
                content
                    .iter()
                    .map(|p| self.lower_param(p))
                    .collect::<Option<Vec<_>>>()?
            } else {
                return None;
            };

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirStructDecl {
                    name: name_str,
                    contracts: extracted_contracts,
                    generic_type_params: Vec::new(),
                    fields,
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_variant(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::VariantStmt {
            qualifiers,
            name,
            contracts,
            body,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);
            let variant_name = self.extract_name_string(name)?;

            let mut conts = Vec::new();
            for contract in contracts {
                let cont_name = self.lower_type(contract)?;
                conts.push(cont_name);
            }

            let mems = self.lower_variant_member(body)?;

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirVariantDecl {
                    name: variant_name,
                    contracts: conts,
                    members: mems,
                    generic_type_params: Vec::new(),
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_enum(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::EnumStmt {
            qualifiers,
            name,
            underlying,
            content,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);
            let enum_name = self.extract_name_string(name)?;

            let underly = match underlying {
                Some(ty) => self.lower_type(ty)?,
                None => HirTypeNode::new(self.next_id(), HirType::U32, stmt.span.clone()),
            };

            let members = self.lower_enum_member(content)?;

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirEnumDecl {
                    name: enum_name,
                    underlying: underly,
                    members,
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_enum_member(&mut self, members: &Vec<EnumMember>) -> Option<Vec<HirEnumMember>> {
        let mut enum_mems = Vec::new();
        let mut current_value = 0;

        for member in members {
            let mem_name = self.extract_name_string(&member.name)?;
            let val = match &member.value {
                Some(expr) => self.eval_const_int(expr)?,
                None => current_value,
            };

            let conv_mem = HirEnumMember {
                hir_id: self.next_id(),
                name: mem_name,
                value: val,
                span: member.span.clone(),
            };

            enum_mems.push(conv_mem);
            current_value = val + 1;
        }
        Some(enum_mems)
    }

    fn lower_variant_member(
        &mut self,
        members: &Vec<VariantMember>,
    ) -> Option<Vec<HirVariantMember>> {
        let mut variants = Vec::new();
        let mut tag = 0;
        for member in members {
            let name_str = self.extract_name_string(&member.name)?;
            let mut types = Vec::new();
            for ty in &member.member_types {
                let convert_ty = self.lower_type(&ty)?;
                types.push(convert_ty);
            }

            let variant_m = HirVariantMember {
                hir_id: self.next_id(),
                name: name_str,
                member_types: types,
                tag,
                span: member.span.clone(),
            };

            variants.push(variant_m);
            tag += 1;
        }
        Some(variants)
    }

    fn lower_while(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::WhileStmt { condition, body } = &stmt.kind {
            let condition = self.lower_expr(condition)?;
            let body = self.lower_block(body)?;

            let hir_while = HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirWhile { condition, body },
                span: stmt.span.clone(),
            };
            Some(hir_while)
        } else {
            None
        }
    }

    fn lower_contract(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::ContractBlock {
            qualifiers,
            name,
            body,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);
            let name_str = self.extract_name_string(name)?;
            let mut funcs = Vec::new();
            for func in body {
                let contract_decl = self.lower_func_decl(func)?;
                funcs.push(contract_decl);
            }

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirContractDecl {
                    name: name_str,
                    functions: funcs,
                    generic_type_params: Vec::new(),
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_for(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::ForStmt {
            init,
            condition,
            update,
            body,
        } = &stmt.kind
        {
            let var = self.lower_var(init)?;
            let condition = self.lower_expr(condition)?;
            let update = self.lower_expr(update)?;

            //Lower the body
            let mut body = self.lower_block(body)?;

            //Push the update into the body
            body.push(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirExpr(update),
                span: stmt.span.clone(),
            });

            let hir_while = HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirWhile { condition, body },
                span: stmt.span.clone(),
            };

            Some(vec![var, hir_while])
        } else {
            None
        }
    }

    fn lower_each(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::EachStmt {
            item,
            collection,
            body,
        } = &stmt.kind
        {
            let span = stmt.span.clone();
            let item_name = self.extract_name_string(item)?;
            let hir_collection = self.lower_expr(collection)?;

            let iter_var = format!("__iter_val_{}", self.iter_counter);
            self.iter_counter += 1;

            // var __iter_val_N := collection.next()
            let next_call =
                self.make_method_call(hir_collection.clone(), "next", vec![], span.clone());
            let init_stmt = self.make_var(iter_var.clone(), true, Some(next_call), span.clone());

            // condition: __iter_val_N != null
            let left = self.make_identifier(&iter_var, span.clone());
            let right = HirExpr::new(
                self.next_id(),
                HirExprKind::Literal(HirLiteral::Null),
                span.clone(),
            );
            let condition = self.make_binary(left, HirBinaryOp::Neq, right, span.clone());

            // var item := unwrap[__iter_val_N]
            let target = self.make_identifier(&iter_var, span.clone());
            let unwrap_trigger = HirExpr::new(
                self.next_id(),
                HirExprKind::Unwrap(Box::new(target)),
                span.clone(),
            );
            let item_decl = self.make_var(item_name, false, Some(unwrap_trigger), span.clone());

            // lower original body, prepend item decl
            let hir_body = self.lower_block(body)?;
            let mut full_body = vec![item_decl];
            full_body.extend(hir_body);

            // __iter_val_N = collection.next() — advance at end
            let advance_call =
                self.make_method_call(hir_collection.clone(), "next", vec![], span.clone());
            let left = self.make_identifier(&iter_var, span.clone());
            let advance_stmt = HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirExpr(self.make_binary(
                    left,
                    HirBinaryOp::Assign,
                    advance_call,
                    span.clone(),
                )),
                span: span.clone(),
            };
            full_body.push(advance_stmt);

            let while_stmt = HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirWhile {
                    condition,
                    body: full_body,
                },
                span,
            };

            Some(vec![init_stmt, while_stmt])
        } else {
            None
        }
    }

    fn lower_seals(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::SealStmt {
            qualifiers,
            name,
            contents,
        } = &stmt.kind
        {
            let map = self.map_qualifiers(qualifiers);
            let seal_name = self.extract_name_string(name)?;
            let mut fns = Vec::new();
            for sealed_fn in contents {
                if let Some(mut sealed) = self.lower_stmt(sealed_fn) {
                    let name_to_mangle = match &sealed.kind {
                        HirStmtKind::HirFunctionDef { name, .. } => Some(name.clone()),
                        _ => None,
                    };

                    if let Some(name) = name_to_mangle {
                        let mangled_name = self.mangle_name(seal_name.clone(), name.clone());

                        match &mut sealed.kind {
                            HirStmtKind::HirFunctionDef {
                                name: struct_name,
                                exposed,
                                ..
                            } => {
                                *struct_name = mangled_name; // Update the mangled name
                                *exposed = map.expose; // Update the boolean flag
                            }
                            _ => unreachable!(),
                        }

                        fns.push(sealed);
                    } else {
                        self.report(
                            "Only function definitions are allowed in seals".to_string(),
                            Some(stmt.span.clone()),
                        );
                    }
                }
            }
            Some(fns)
        } else {
            None
        }
    }

    fn lower_methods(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::MethodsStmt { name, contents } = &stmt.kind {
            let type_name = self.extract_name_string(name)?;
            let mut methods = Vec::new();
            for metfunc in contents {
                if let Some(mut func) = self.lower_stmt(metfunc) {
                    let name_to_mangle = match &func.kind {
                        HirStmtKind::HirFunctionDef { name, .. } => Some(name.clone()),
                        _ => None,
                    };

                    if let Some(name) = name_to_mangle {
                        let mangled_name = self.mangle_name(type_name.clone(), name.clone());

                        match &mut func.kind {
                            HirStmtKind::HirFunctionDef {
                                name: fn_name,
                                params,
                                ..
                            } => {
                                *fn_name = mangled_name;
                                let self_param = HirParam {
                                    hir_id: self.next_id(),
                                    name: "self".to_string(),
                                    ty: HirTypeNode::new(
                                        self.next_id(),
                                        HirType::Ref(Box::new(HirTypeNode::custom(
                                            self.next_id(),
                                            type_name.to_string(),
                                            func.span.clone(),
                                        ))),
                                        func.span.clone(),
                                    ),
                                    mutable: false,
                                    default: None,
                                    span: func.span.clone(),
                                };
                                params.insert(0, self_param);
                            }
                            _ => unreachable!(),
                        }
                        methods.push(func);
                    } else {
                        self.report(
                            "Only function definitions are allowed in methods".to_string(),
                            Some(stmt.span.clone()),
                        );
                    }
                }
            }
            Some(methods)
        } else {
            None
        }
    }

    fn lower_generics(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::GenericBlock { params, body } = &stmt.kind {
            let mut generics = Vec::new();
            let mut generic_types = Vec::new();
            self.current_generic_params = params.clone();
            for param in params {
                let ty = self.lower_type(param)?;
                generic_types.push(ty);
            }

            let body = self.lower_block(body)?;
            self.current_generic_params.clear();
            for mut content in body {
                match &mut content.kind {
                    HirStmtKind::HirStructDecl {
                        generic_type_params: t,
                        ..
                    }
                    | HirStmtKind::HirFunctionDef {
                        generic_type_params: t,
                        ..
                    }
                    | HirStmtKind::HirFunctionDecl {
                        generic_type_params: t,
                        ..
                    }
                    | HirStmtKind::HirVariantDecl {
                        generic_type_params: t,
                        ..
                    }
                    | HirStmtKind::HirContractDecl {
                        generic_type_params: t,
                        ..
                    } => {
                        t.extend(generic_types.clone());
                        generics.push(content);
                    }
                    _ => self.report(
                        "Invalid statement inside generic block".to_string(),
                        Some(content.span.clone()),
                    ),
                }
            }

            Some(generics)
        } else {
            None
        }
    }

    fn lower_return(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::Return(ret_expr) = &stmt.kind {
            let expr = match ret_expr {
                Some(ex) => self.lower_expr(&ex),
                None => None,
            };

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirReturn(expr),
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_break_or_cont(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match &stmt.kind {
            StmtKind::Break => Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirBreak,
                span: stmt.span.clone(),
            }),
            StmtKind::Continue => Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirContinue,
                span: stmt.span.clone(),
            }),
            _ => None,
        }
    }

    fn lower_if(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::IfStmt {
            condition,
            body,
            elifs,
            else_body,
        } = &stmt.kind
        {
            let hir_condition = self.lower_expr(condition)?;
            let hir_body = self.lower_block(body)?;

            // lower the final else body, if any
            let final_else = match else_body {
                Some(eb) => Some(self.lower_block(eb)?),
                None => None,
            };

            // fold elifs from the back, building nested else-if chains
            let mut nested_else = final_else;
            for elif in elifs.iter().rev() {
                let elif_condition = self.lower_expr(&elif.condition)?;
                let elif_body = self.lower_block(&elif.body)?;

                let nested_if = HirStmt {
                    hir_id: self.next_id(),
                    kind: HirStmtKind::HirIf {
                        condition: elif_condition,
                        body: elif_body,
                        else_body: nested_else,
                    },
                    span: elif.body.span.clone(),
                };

                nested_else = Some(vec![nested_if]);
            }

            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirIf {
                    condition: hir_condition,
                    body: hir_body,
                    else_body: nested_else,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    pub fn lower_constructs(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        match stmt.kind {
            StmtKind::SealStmt { .. } => self.lower_seals(stmt),
            StmtKind::MethodsStmt { .. } => self.lower_methods(stmt),
            StmtKind::GenericBlock { .. } => self.lower_generics(stmt),
            StmtKind::ForStmt { .. } => self.lower_for(stmt),
            StmtKind::EachStmt { .. } => self.lower_each(stmt),
            _ => {
                self.report("Invalid construct".to_string(), Some(stmt.span.clone()));
                None
            }
        }
    }

    fn lower_alias(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::AliasStmt { original, new } = &stmt.kind {
            let orig = self.lower_type(original)?;
            let new = self.extract_name_string(new)?;
            Some(HirStmt {
                hir_id: self.next_id(),
                kind: HirStmtKind::HirAlias {
                    original: Box::new(orig),
                    alias: new,
                },
                span: stmt.span.clone(),
            })
        } else {
            None
        }
    }

    fn lower_import(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        if let StmtKind::ImportStmt { name, alias } = &stmt.kind {
            let name = self.extract_name_string(name)?;
            if let Some(al) = alias {
                let alias = Some(self.extract_name_string(al)?);
                Some(HirStmt {
                    hir_id: self.next_id(),
                    kind: HirStmtKind::HirImport { name, alias },
                    span: stmt.span.clone(),
                })
            } else {
                Some(HirStmt {
                    hir_id: self.next_id(),
                    kind: HirStmtKind::HirImport { name, alias: None },
                    span: stmt.span.clone(),
                })
            }
        } else {
            None
        }
    }

    fn lower_block(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::Block { content } = &stmt.kind {
            let mut contents = Vec::new();
            for victim in content {
                match &victim.kind {
                    StmtKind::SealStmt { .. } | StmtKind::MethodsStmt { .. } => {
                        let construct_vec = self.lower_constructs(victim)?;
                        contents.extend(construct_vec);
                    }
                    _ => {
                        let lowered_stmt = self.lower_stmt(victim)?;
                        contents.push(lowered_stmt);
                    }
                }
            }

            Some(contents)
        } else {
            None
        }
    }
}
