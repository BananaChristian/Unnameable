use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirExpr, HirExprKind, HirLiteral, HirPattern, HirStmt, HirStmtKind},
    indexer::NodeIndex,
    semantics::{ResolvedTypeKind, SemanticCtxt, TypeInfo},
};

/// How a statement ends the enclosing block.
#[derive(Clone, Copy, Debug, Default)]
struct Termination {
    /// Control never falls past this statement inside its own block.
    pub terminates: bool,
    /// It terminates because the current function/scope is returned from.
    pub returns: bool,
    /// A reachable path may execute a `break` that escapes this statement.
    pub breaks: bool,
}

impl Termination {
    fn falls_through() -> Self {
        Termination {
            terminates: false,
            returns: false,
            breaks: false,
        }
    }

    fn returns() -> Self {
        Termination {
            terminates: true,
            returns: true,
            breaks: false,
        }
    }

    fn breaks() -> Self {
        Termination {
            terminates: true,
            returns: false,
            breaks: true,
        }
    }

    fn continues() -> Self {
        Termination {
            terminates: true,
            returns: false,
            breaks: false,
        }
    }
}

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
        for id in &self.node_index.roots {
            if let Some(stmt) = self.node_index.get(id) {
                self.check_stmt(stmt);
            }
        }
    }

    /// Walks a block of statements, flags dead code and computes how the block
    /// terminates. Statements following the termination point are unreachable
    /// and do not participate in the analysis.
    fn walk_block(&mut self, stmts: &[HirStmt]) -> Termination {
        let mut term = Termination::falls_through();
        let mut terminated = false;
        for st in stmts {
            if terminated {
                self.unreachable_code(st.span.clone());
                continue;
            }
            let t = self.check_stmt(st);
            if t.breaks {
                term.breaks = true;
            }
            if t.terminates {
                term.terminates = true;
                term.returns = t.returns;
                terminated = true;
            }
        }
        term
    }

    fn check_stmt(&mut self, stmt: &HirStmt) -> Termination {
        match &stmt.kind {
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirReturn(_) => self.check_return(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func(stmt),
            HirStmtKind::HirContractDecl { functions, .. } => {
                for func in functions {
                    self.check_func(func);
                }
                Termination::falls_through()
            }
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirBreak => self.check_break_continue(stmt, "break", true),
            HirStmtKind::HirContinue => self.check_break_continue(stmt, "continue", false),
            _ => {
                self.walk_stmt_exprs(stmt);
                Termination::falls_through()
            }
        }
    }

    fn check_func(&mut self, stmt: &HirStmt) -> Termination {
        if let HirStmtKind::HirFunctionDef {
            return_type, body, ..
        } = &stmt.kind
        {
            self.fn_context = true; //We are in a function
            let current_fn_ret_ty = self.ctxt.types.types.get(&return_type.hir_id).cloned();
            self.current_return_type = current_fn_ret_ty.clone();

            let body_term = self.walk_block(body);

            if let Some(ty) = &current_fn_ret_ty {
                if ty.kind != ResolvedTypeKind::Unit && !body_term.returns {
                    self.report(
                        format!(
                            "function missing  terminal return statement, expected return statement of type '{}'",
                            ty.name
                        ),
                        Some(stmt.span.clone()),
                    );
                }
            }

            self.fn_context = false;
            self.current_return_type = None;
        }
        Termination::falls_through()
    }

    fn check_return(&mut self, stmt: &HirStmt) -> Termination {
        if let HirStmtKind::HirReturn(val) = &stmt.kind {
            if !self.fn_context {
                self.report(
                    format!("Return statements must only exist in a function body"),
                    Some(stmt.span.clone()),
                );
            }

            let return_ty = self.ctxt.types.types.get(&stmt.hir_id);

            if let Some(current_r) = &self.current_return_type {
                if let Some(return_ty) = return_ty {
                    let known = current_r.kind != ResolvedTypeKind::Unknown
                        && return_ty.kind != ResolvedTypeKind::Unknown;
                    if known && !TypeInfo::types_match(current_r, return_ty) {
                        self.report(
                            format!(
                                "Expected type '{}' but got '{}'",
                                current_r.name, return_ty.name
                            ),
                            Some(stmt.span.clone()),
                        );
                    }
                }
            }

            if let Some(inner) = val {
                self.walk_expr(inner);
            }
        }
        Termination::returns()
    }

    fn check_while(&mut self, stmt: &HirStmt) -> Termination {
        if let HirStmtKind::HirWhile { condition, body } = &stmt.kind {
            self.walk_expr(condition);
            self.loop_depth += 1;
            let body_term = self.walk_block(body);
            self.loop_depth -= 1;

            if is_literal_true(condition) {
                // A `while true` only ever stops if a reachable `break` escapes
                // it. When no break can run, the loop diverges.
                return Termination {
                    terminates: !body_term.breaks,
                    returns: !body_term.breaks && body_term.returns,
                    breaks: false,
                };
            }
        }
        Termination::falls_through()
    }

    fn check_break_continue(&mut self, stmt: &HirStmt, name: &str, is_break: bool) -> Termination {
        if self.loop_depth == 0 {
            self.report(
                format!("{} statements must only exist inside a loop body", name),
                Some(stmt.span.clone()),
            );
            return Termination::falls_through();
        }
        if is_break {
            Termination::breaks()
        } else {
            Termination::continues()
        }
    }

    fn check_if(&mut self, stmt: &HirStmt) -> Termination {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &stmt.kind
        {
            self.walk_expr(condition);
            let body_term = self.walk_block(body);

            let result = if let Some(els) = else_body {
                let else_term = self.walk_block(els);
                Termination {
                    terminates: body_term.terminates && else_term.terminates,
                    returns: body_term.returns && else_term.returns,
                    breaks: body_term.breaks || else_term.breaks,
                }
            } else {
                Termination {
                    terminates: false,
                    returns: false,
                    breaks: body_term.breaks,
                }
            };
            return result;
        }
        Termination::falls_through()
    }

    // A dollar scope compiles to its own function, so its statements are
    // checked in a fresh function context: `return` terminates the scope and
    // `break`/`continue` may only target loops inside the scope itself.
    fn check_dollar_scope(&mut self, expr: &HirExpr, body: &[HirStmt]) {
        let saved_fn_context = self.fn_context;
        let saved_loop_depth = self.loop_depth;
        let saved_return_type = self.current_return_type.clone();

        self.fn_context = true;
        self.loop_depth = 0;
        self.current_return_type = self.ctxt.types.types.get(&expr.hir_id).cloned();
        self.walk_block(body);

        self.fn_context = saved_fn_context;
        self.loop_depth = saved_loop_depth;
        self.current_return_type = saved_return_type;
    }

    fn walk_stmt_exprs(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirExpr(expr) => self.walk_expr(expr),
            HirStmtKind::HirVarDecl { init, .. } => self.walk_expr(init),
            _ => (),
        }
    }

    fn walk_expr(&mut self, expr: &HirExpr) {
        match &expr.kind {
            HirExprKind::Binary(left, _, right) => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            HirExprKind::Unary(_, inner) => self.walk_expr(inner),
            HirExprKind::Postfix(inner, _) => self.walk_expr(inner),
            HirExprKind::Call(callee, args) => {
                self.walk_expr(callee);
                for arg in args {
                    self.walk_expr(arg);
                }
            }
            HirExprKind::Unwrap(inner) => self.walk_expr(inner),
            HirExprKind::StaticCast(_, inner) | HirExprKind::BitCast(_, inner) => {
                self.walk_expr(inner)
            }
            HirExprKind::Instantiation { body, .. } => {
                for param in body {
                    self.walk_expr(&param.value);
                }
            }
            HirExprKind::TupleInst { body } => {
                for elem in body {
                    self.walk_expr(elem);
                }
            }
            HirExprKind::Index { target, index } => {
                self.walk_expr(target);
                self.walk_expr(index);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.walk_expr(scrutinee);
                for arm in arms {
                    self.walk_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.walk_expr(guard);
                    }
                    self.walk_expr(&arm.body);
                }
            }
            HirExprKind::Block(body) => {
                for stmt in body {
                    self.check_stmt(stmt);
                }
            }
            HirExprKind::DollarScope {
                params, body, result, ..
            } => {
                for param in params {
                    self.walk_expr(param);
                }
                self.check_dollar_scope(expr, body);
                if let Some(res) = result {
                    self.walk_expr(res);
                }
            }
            _ => (),
        }
    }

    fn unreachable_code(&mut self, span: Span) {
        self.report(format!("Unreachable code"), Some(span.clone()));
    }

    fn walk_pattern(&mut self, pattern: &HirPattern) {
        match pattern {
            HirPattern::Wildcard | HirPattern::Binding { .. } => {}
            HirPattern::Literal(expr) => self.walk_expr(expr),
            HirPattern::Path { payloads, .. } => {
                for payload in payloads {
                    self.walk_pattern(payload);
                }
            }
            HirPattern::Tuple { elements, .. } => {
                for element in elements {
                    self.walk_pattern(element);
                }
            }
            HirPattern::StructPattern { fields, .. } => {
                for field in fields {
                    self.walk_pattern(&field.pattern);
                }
            }
            HirPattern::Or(alts) => {
                for alt in alts {
                    self.walk_pattern(alt);
                }
            }
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}

fn is_literal_true(expr: &HirExpr) -> bool {
    matches!(&expr.kind, HirExprKind::Literal(HirLiteral::Bool(true)))
}