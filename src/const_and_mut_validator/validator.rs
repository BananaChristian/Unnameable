use std::collections::HashMap;

use crate::{
    hir::{HirExpr, HirExprKind, HirStmt, HirStmtKind},
    indexer::NodeIndex,
    semantics::SemanticCtxt,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingKind {
    Const,
    Mutable,
    Immutable,
}

pub struct Validator<'a> {
    node_index: &'a NodeIndex,
    ctxt: &'a SemanticCtxt,
    scopes: Vec<HashMap<String, BindingKind>>,
    pub corrupted: bool,
}

impl<'a> Validator<'a> {
    pub fn new(node_index: &'a NodeIndex, ctxt: &'a SemanticCtxt) -> Self {
        Validator {
            node_index,
            ctxt,
            scopes: vec![HashMap::new()],
            corrupted: false,
        }
    }

    fn look_up(&self, name: &str) -> Option<BindingKind> {
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
            HirExprKind::Literal(_) => true,
            // If it's a complex expression (like an addition or function call), it's not a raw literal
            _ => false,
        }
    }

    fn check_var_decl(&self, stmt: &HirStmt) {
        if let HirStmtKind::HirVarDecl {
            name,
            mutable,
            constant,
            heap,
            exposed,
            ty,
            init,
        } = &stmt.kind
        {}
    }
}
