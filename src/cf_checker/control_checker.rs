use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirStmt, HirStmtKind},
    indexer::NodeIndex,
    semantics::{ResolvedTypeKind, SemanticCtxt, TypeInfo},
};

pub struct ControlFlowChecker<'a> {
    node_index: &'a NodeIndex,
    diagnostics: SharedDiagnostics,
    ctxt: &'a SemanticCtxt,
    loop_depth: u32,
    fn_context: bool,
    current_return_type: Option<TypeInfo>,
    pub corrupted: bool,
}

impl<'a> ControlFlowChecker<'a> {
    pub fn new(
        node_index: &'a NodeIndex,
        ctxt: &'a SemanticCtxt,
        diagnostics: SharedDiagnostics,
    ) -> Self {
        ControlFlowChecker {
            node_index,
            diagnostics,
            ctxt,
            loop_depth: 0,
            fn_context: false,
            current_return_type: None,
            corrupted: false,
        }
    }

    pub fn run(&mut self) {
        for (_, stmt) in &self.node_index.nodes {
            if let HirStmtKind::HirFunctionDef { .. } = &stmt.kind {
                self.check_stmt(stmt);
            }
        }
    }

    fn check_stmt(&mut self, stmt: &HirStmt) -> bool {
        match &stmt.kind {
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirReturn(_) => self.check_return(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirBreak => self.check_break_continue(stmt, "break"),
            HirStmtKind::HirContinue => self.check_break_continue(stmt, "continue"),
            _ => false,
        }
    }

    fn check_func(&mut self, stmt: &HirStmt) -> bool {
        let mut always_returns = false;
        if let HirStmtKind::HirFunctionDef {
            return_type, body, ..
        } = &stmt.kind
        {
            self.fn_context = true; //We are in a function
            let current_fn_ret_ty = self.ctxt.types.types.get(&return_type.hir_id).expect(
                format!("Failed to get type info for id: {:?}", return_type.hir_id).as_str(),
            );

            self.current_return_type = Some(current_fn_ret_ty.clone());

            for st in body {
                if always_returns {
                    self.unreachable_code(st.span.clone());
                }
                if self.check_stmt(st) {
                    always_returns = true;
                }
            }

            if current_fn_ret_ty.kind != ResolvedTypeKind::Unit && !always_returns {
                self.report(format!("function missing  terminal return statement, expected return statement of type '{}'",current_fn_ret_ty.name), 
                    Some(stmt.span.clone()));
            }

            self.fn_context = false;
            self.current_return_type = None;
        }
        always_returns
    }

    fn check_return(&mut self, stmt: &HirStmt) -> bool {
        let mut always_returns = false;
        if let HirStmtKind::HirReturn(_) = &stmt.kind {
            if !self.fn_context {
                self.report(
                    format!("Return statements must only exist in a function body"),
                    Some(stmt.span.clone()),
                );
            }

            let return_ty = self
                .ctxt
                .types
                .types
                .get(&stmt.hir_id)
                .expect(format!("Failed to get type info for id: {:?}", &stmt.hir_id).as_str());

            if let Some(current_r) = &self.current_return_type {
                if !TypeInfo::types_match(return_ty, current_r) {
                    self.report(
                        format!(
                            "Expected type '{}' but got '{}'",
                            return_ty.name, current_r.name
                        ),
                        Some(stmt.span.clone()),
                    );
                }
            }

            always_returns = true; //Just mark it since u have encountered it
        }
        always_returns
    }

    fn check_while(&mut self, stmt: &HirStmt) -> bool {
        if let HirStmtKind::HirWhile { body, .. } = &stmt.kind {
            self.loop_depth += 1;
            let mut body_returns = false;
            for st in body {
                if body_returns {
                    self.unreachable_code(st.span.clone());
                }

                if self.check_stmt(st) {
                    body_returns = true;
                }
            }
            self.loop_depth -= 1;
        }
        false
    }

    fn check_break_continue(&mut self, stmt: &HirStmt, name: &str) -> bool {
        if self.loop_depth == 0 {
            self.report(
                format!("{} statements must only exist inside a loop body", name),
                Some(stmt.span.clone()),
            );
        }
        false
    }

    fn check_if(&mut self, stmt: &HirStmt) -> bool {
        if let HirStmtKind::HirIf {
            body, else_body, ..
        } = &stmt.kind
        {
            let mut body_returns = false;
            for st in body {
                if body_returns {
                    self.unreachable_code(st.span.clone());
                }
                if self.check_stmt(st) {
                    body_returns = true;
                }
            }

            let mut else_returns = false;
            if let Some(el_stmts) = else_body {
                for el in el_stmts {
                    if else_returns {
                        self.unreachable_code(el.span.clone());
                    }
                    if self.check_stmt(el) {
                        else_returns = true;
                    }
                }
            }

            if else_body.is_some() {
                return body_returns && else_returns;
            }
        }

        false
    }

    fn unreachable_code(&mut self, span: Span) {
        self.report(format!("Unreachable code"), Some(span.clone()));
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
