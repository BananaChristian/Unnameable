use std::collections::HashMap;

use crate::{
    diagnostics::{self, CompilerError, Diagnostics, Phase, Span},
    hir::{HirExpr, HirExprKind, HirLiteral, HirParam, HirStmt, HirStmtKind},
    indexer::NodeIndex,
    semantics::SemanticCtxt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    Const,
    Mutable,
    Immutable,
}

pub struct Validator<'a> {
    node_index: &'a NodeIndex,
    ctxt: &'a SemanticCtxt,
    diagnostics: &'a mut Diagnostics,
    scopes: Vec<HashMap<String, BindingKind>>,
    pub corrupted: bool,
}

impl<'a> Validator<'a> {
    pub fn new(
        node_index: &'a NodeIndex,
        ctxt: &'a SemanticCtxt,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Validator {
            node_index,
            ctxt,
            diagnostics,
            scopes: vec![HashMap::new()],
            corrupted: false,
        }
    }

    pub fn look_up(&self, name: &str) -> Option<BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.get(name) {
                return Some(*kind);
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn is_compile_time_literal(&self, expr: &HirExpr) -> bool {
        match &expr.kind {
            HirExprKind::Literal(inner) => match inner {
                HirLiteral::Null | HirLiteral::ArrayLiteral(_) => false, //For now these are a no
                //go
                _ => true,
            },
            _ => false,
        }
    }

    fn run(&mut self) {
        for (_, stmt) in &self.node_index.nodes {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirVarDecl { .. } => self.check_var_decl(stmt),
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirExpr(_) => self.check_expr_stmt(stmt),
            _ => (),
        }
    }

    fn check_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) = &stmt.kind {
            self.check_expr(inner);
        }
    }

    fn check_param(&mut self, param: &HirParam) {
        if param.mutable {
            self.scopes
                .last_mut()
                .unwrap()
                .insert(param.name.clone(), BindingKind::Mutable);
        } else {
            self.scopes
                .last_mut()
                .unwrap()
                .insert(param.name.clone(), BindingKind::Immutable);
        }
    }

    fn check_func_decl(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef { params, body, .. } = &stmt.kind {
            self.enter_scope();
            for param in params {
                self.check_param(param);
            }
            for st in body {
                self.check_stmt(st);
            }
            self.exit_scope();
        }
    }

    fn check_while(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirWhile { condition, body } = &stmt.kind {
            self.enter_scope();
            for st in body {
                self.check_stmt(st);
            }
            self.exit_scope();
        }
    }

    fn check_if(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &stmt.kind
        {
            self.enter_scope();
            for st in body {
                self.check_stmt(stmt);
            }
            self.exit_scope();

            if let Some(el_bod) = else_body {
                self.enter_scope();
                for el_st in el_bod {
                    self.check_stmt(el_st);
                }
                self.exit_scope();
            }
        }
    }

    fn check_var_decl(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirVarDecl {
            name,
            mutable,
            constant,
            init,
            ..
        } = &stmt.kind
        {
            let is_constant = *constant;
            let is_mutable = *mutable;

            //Mut and const are mutually exclusive
            if is_mutable && is_constant {
                self.report(
                    format!(
                        "Variable '{}' cannot be const and mutable at the same time",
                        name,
                    ),
                    Some(stmt.span.clone()),
                );
            }

            //Is it constant if so, apply come checks
            if is_constant {
                if let Some(initializer) = init {
                    if !self.is_compile_time_literal(initializer) {
                        self.report(format!("Constant variable '{}' must be initilialized with a compile time value",name), 
                            Some(stmt.span.clone()));
                    }
                } else {
                    self.report(
                        format!("Constant variable '{}' must be initialized", name),
                        Some(stmt.span.clone()),
                    );
                }

                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), BindingKind::Const);
            } else if is_mutable {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), BindingKind::Mutable);
            } else {
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert(name.clone(), BindingKind::Immutable);
            }
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
