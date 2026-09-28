use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirStmt, HirStmtKind, HirTypeNode},
    indexer::NodeIndex,
    semantics::{ResolvedTypeKind, SemanticCtxt, TypeInfo},
};

pub struct ContractVerifier<'a> {
    node_index: &'a NodeIndex,
    ctxt: &'a mut SemanticCtxt,
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl<'a> ContractVerifier<'a> {
    pub fn new(
        node_index: &'a NodeIndex,
        ctxt: &'a mut SemanticCtxt,
        diagnostics: SharedDiagnostics,
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
            // The verifier runs *after* monomorphization, over the unified
            // tree. Concrete instances (`_U_Pair_i32`) keep their contract
            // usages but with fresh ids the name table never recorded, so the
            // lookup below misses. Instances are skipped: a generic struct's
            // instance functions (`_U_Pair_i32_get`, …) have no language-level
            // way to exist yet -- generic methods do not monomorphize, they ICE
            // with an unsubstituted type parameter -- so there is nothing to
            // verify an instance's contracts against, and demanding one would
            // demand the user write something they cannot write.
            //
            // Skipping the *instance* is right. The *template* is a separate
            // matter and is reported during type checking, which still runs
            // before monomorphization drops the template entirely -- see
            // `TypeChecker::check_struct_decl`.
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
                name,
                contracts,
                generic_type_params,
                ..
            }
            | HirStmtKind::HirVariantDecl {
                name,
                contracts,
                generic_type_params,
                ..
            } => {
                if !contracts.is_empty() && !generic_type_params.is_empty() {
                    self.report(
                        format!(
                            "'{}' is generic and declares a contract, which is not supported yet: \
                             generic methods do not monomorphize, so the contract cannot be \
                             satisfied by any implementation you can write",
                            name
                        ),
                        Some(stmt.span.clone()),
                    );
                }
                self.verify(name, contracts, stmt.span.clone())
            }

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
        if let HirStmtKind::HirFunctionDecl { name, .. } = &required_fn.kind {
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
                        required_fn,
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
        required_fn: &HirStmt,
        implementer_name: &str,
        fn_name: &str,
        contract_name: &str,
    ) {
        let req_param_count = match &required_fn.kind {
            HirStmtKind::HirFunctionDecl { params, .. } => params.len(),
            _ => return,
        };
        let impl_param_count = match &impl_stmt.kind {
            HirStmtKind::HirFunctionDef { params, .. } => params.len(),
            _ => return,
        };

        // skip self — first param on impl side. The impl is looked up by
        // name alone (`{struct}_{fn}`), so a *free* function that happens
        // to wear that name can reach here with no receiver — report rather
        // than panicking on an empty slice.
        let impl_params_no_self = match impl_param_count.checked_sub(1) {
            Some(n) => n,
            None => {
                self.report(
                    format!(
                        "'{}' implements '{}' from contract '{}' without a 'self' receiver",
                        implementer_name, fn_name, contract_name
                    ),
                    Some(impl_stmt.span.clone()),
                );
                return;
            }
        };

        if impl_params_no_self != req_param_count {
            self.report(
                format!(
                    "'{}' implements '{}' from contract '{}' with wrong number of parameters",
                    implementer_name, fn_name, contract_name
                ),
                Some(impl_stmt.span.clone()),
            );
            return;
        }

        // ---- type checks (resolved Func-kind from the type table) ----
        // The checker records the full function type under the stmt's
        // hir_id, not under the individual return-type / param-type nodes,
        // so we must read the Func kind from there.
        let req_info = self.ctxt.types.types.get(&required_fn.hir_id);
        let impl_info = self.ctxt.types.types.get(&impl_stmt.hir_id);

        let mut type_errors = Vec::new();
        if let (
            Some(ResolvedTypeKind::Func {
                params: ip,
                ret_type: ir,
                ..
            }),
            Some(ResolvedTypeKind::Func {
                params: rp,
                ret_type: rr,
                ..
            }),
        ) = (impl_info.map(|i| &i.kind), req_info.map(|i| &i.kind))
        {
            // The receiver is the implementer's own first parameter. The contract
            // does not declare a receiver, so there is nothing in it to compare
            // against -- but the receiver still has to *be* the implementer, or the
            // promise is not being kept. `impl Point` with
            // `func get(self: Other)` was accepted before this check, and the
            // error only surfaced later and elsewhere, as a call-site type
            // mismatch. The verifier is the pass whose job is to catch a broken
            // promise, so it is where it belongs.
            if let Some(recv_ty) = ip.first() {
                let recv_named = match &recv_ty.kind {
                    ResolvedTypeKind::Struct { name, .. } => Some(name.as_str()),
                    ResolvedTypeKind::Mut { inner } => match &inner.kind {
                        ResolvedTypeKind::Struct { name, .. } => Some(name.as_str()),
                        _ => None,
                    },
                    _ => None,
                };
                match recv_named {
                    Some(name) if name == implementer_name => {}
                    Some(name) => type_errors.push(format!(
                        "'{}::{}' takes a '{}' receiver but implements contract '{}' for '{}'",
                        implementer_name, fn_name, name, contract_name, implementer_name
                    )),
                    None => type_errors.push(format!(
                        "'{}::{}' has a receiver that is not the implementing type '{}'",
                        implementer_name, fn_name, implementer_name
                    )),
                }
            }

            // impl params include self at index 0
            for (impl_ty, req_ty) in ip.iter().skip(1).zip(rp.iter()) {
                if !TypeInfo::types_match(impl_ty, req_ty) {
                    type_errors.push(format!(
                        "'{}::{}' parameter type does not match contract '{}' requirement",
                        implementer_name, fn_name, contract_name
                    ));
                }
            }
            if !TypeInfo::types_match(rr, ir) {
                type_errors.push(format!(
                    "'{}::{}' return type does not match contract '{}' requirement",
                    implementer_name, fn_name, contract_name
                ));
            }
        }

        for msg in type_errors {
            self.report(msg, Some(impl_stmt.span.clone()));
        }
    }

    fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics.borrow_mut().report(CompilerError::error(
            message,
            Phase::ContractVerifier,
            span,
        ));
    }
}
