use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirParam, HirStmt, HirStmtKind, HirTypeNode},
    indexer::NodeIndex,
    semantics::{SemanticCtxt, TypeInfo},
};

pub struct ContractVerifier<'a> {
    node_index: &'a NodeIndex,
    ctxt: &'a mut SemanticCtxt,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> ContractVerifier<'a> {
    pub fn new(
        node_index: &'a NodeIndex,
        ctxt: &'a mut SemanticCtxt,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        ContractVerifier {
            node_index,
            ctxt,
            diagnostics,
            corrupted: false,
        }
    }

    pub fn run(&mut self) {
        for (_, stmt) in &self.node_index.nodes {
            self.verify_statement(stmt);
        }
    }

    fn verify(&mut self, implementer_name: &String, contracts: &Vec<HirTypeNode>, span: Span) {
        for contract in contracts {
            let decl_id = match self.ctxt.names.resolved.get(&contract.hir_id) {
                Some(id) => id,
                None => continue,
            };

            let contract_stmt = match self.node_index.get(&decl_id) {
                Some(contract_st) => contract_st,
                None => continue,
            };

            if let HirStmtKind::HirContractDecl {
                name: contract_name,
                functions,
                ..
            } = &contract_stmt.kind
            {
                for required_fn in functions {
                    self.verify_function_impl(
                        implementer_name,
                        required_fn,
                        contract_name,
                        span.clone(),
                    );
                }
            }
        }
    }

    fn verify_statement(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirStructDecl {
                name, contracts, ..
            }
            | HirStmtKind::HirVariantDecl {
                name, contracts, ..
            } => self.verify(name, contracts, stmt.span.clone()),

            _ => (),
        }
    }

    fn verify_function_impl(
        &mut self,
        implementer_name: &String,
        required_fn: &HirStmt,
        contract_name: &String,
        span: Span,
    ) {
        if let HirStmtKind::HirFunctionDecl {
            name,
            params,
            return_type,
            ..
        } = &required_fn.kind
        {
            let expected_name = format!("{}_{}", implementer_name, name);

            let impl_fn = self.node_index.nodes.values().find(|stmt| {
                if let HirStmtKind::HirFunctionDef { name: fn_name, .. } = &stmt.kind {
                    fn_name == &expected_name
                } else {
                    false
                }
            });

            match impl_fn {
                None => {
                    self.report(
                        format!(
                            "'{}' missing implementation of '{}' required by '{}' contract",
                            implementer_name, name, contract_name
                        ),
                        Some(span),
                    );
                }
                Some(impl_stmt) => {
                    self.verify_signature(
                        impl_stmt,
                        params,
                        return_type,
                        implementer_name,
                        name,
                        contract_name,
                    );
                }
            }
        }
    }

    fn verify_signature(
        &mut self,
        impl_stmt: &HirStmt,
        required_params: &Vec<HirParam>,
        required_ret: &HirTypeNode,
        implementer_name: &str,
        fn_name: &str,
        contract_name: &str,
    ) {
        if let HirStmtKind::HirFunctionDef {
            params,
            return_type,
            ..
        } = &impl_stmt.kind
        {
            // skip self — first param on impl side
            let impl_params = &params[1..];

            // check param count
            if impl_params.len() != required_params.len() {
                self.report(
                    format!(
                        "'{}' implements '{}' from contract '{}' with wrong number of parameters",
                        implementer_name, fn_name, contract_name
                    ),
                    Some(impl_stmt.span.clone()),
                );
                return;
            }

            // check each param type
            for (impl_param, req_param) in impl_params.iter().zip(required_params.iter()) {
                let impl_ty = self.ctxt.types.types.get(&impl_param.ty.hir_id);
                let req_ty = self.ctxt.types.types.get(&req_param.ty.hir_id);

                match (impl_ty, req_ty) {
                    (Some(it), Some(rt)) => {
                        if !TypeInfo::types_match(it, rt) {
                            self.report(format!(
                                "'{}::{}' parameter type does not match contract '{}' requirement",
                                implementer_name, fn_name, contract_name
                            ),Some(impl_param.span.clone()));
                        }
                    }
                    _ => {}
                }
            }

            // check return type
            let impl_ret = self.ctxt.types.types.get(&return_type.hir_id);
            let req_ret = self.ctxt.types.types.get(&required_ret.hir_id);

            match (impl_ret, req_ret) {
                (Some(it), Some(rt)) => {
                    if !TypeInfo::types_match(it, rt) {
                        self.report(
                            format!(
                                "'{}::{}' return type does not match contract '{}' requirement",
                                implementer_name, fn_name, contract_name
                            ),
                            Some(return_type.span.clone()),
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::ContractVerifier, span));
    }
}
