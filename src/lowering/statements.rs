use crate::{
    ast::{Stmt, StmtKind},
    hir::{HirParam, HirStmt, HirStmtKind, HirType, HirTypeNode},
    lowering::lowering::Lowering,
};

impl<'a> Lowering<'a> {
    pub fn lower_stmt(&mut self, stmt: &Stmt) -> Option<HirStmt> {
        match stmt.kind {
            StmtKind::VarDecl { .. } => self.lower_var(stmt),
            StmtKind::FunctionDef { .. } => self.lower_func_def(stmt),
            StmtKind::FunctionDecl { .. } => self.lower_func_decl(stmt),
            StmtKind::StructDecl { .. } => self.lower_struct(stmt),
            _ => {
                self.report("Unknown statement".to_string(), Some(stmt.span.clone()));
                None
            }
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
                None => HirTypeNode::unit(stmt.span.clone()),
            };

            let hir_body = self.lower_block(body)?;

            Some(HirStmt {
                kind: HirStmtKind::HirFunctionDef {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    type_params: self.current_type_params.clone(),
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
                None => HirTypeNode::unit(stmt.span.clone()),
            };

            Some(HirStmt {
                kind: HirStmtKind::HirFunctionDecl {
                    name: name_str,
                    params: hir_params,
                    return_type,
                    type_params: self.current_type_params.clone(),
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
                let cont = self.extract_name_string(contract)?;
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
                kind: HirStmtKind::HirStructDecl {
                    name: name_str,
                    contracts: extracted_contracts,
                    type_params: self.current_type_params.clone(),
                    fields,
                    exposed: map.expose,
                },
                span: stmt.span.clone(),
            })
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
                                    name: "self".to_string(),
                                    ty: HirTypeNode::new(
                                        HirType::Ref(Box::new(HirType::CustomType(
                                            type_name.to_string(),
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

    pub fn lower_constructs(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        match stmt.kind {
            StmtKind::SealStmt { .. } => self.lower_seals(stmt),
            StmtKind::MethodsStmt { .. } => self.lower_methods(stmt),
            _ => {
                self.report("Invalid construct".to_string(), Some(stmt.span.clone()));
                None
            }
        }
    }

    fn lower_block(&mut self, stmt: &Stmt) -> Option<Vec<HirStmt>> {
        if let StmtKind::Block { content } = &stmt.kind {
            content
                .iter()
                .map(|s| self.lower_stmt(s))
                .collect::<Option<Vec<_>>>()
        } else {
            None
        }
    }
}
