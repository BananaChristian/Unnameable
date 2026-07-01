use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::HirStmt,
    lowering::NodeId,
    semantics::semantics::NameTable,
};

pub struct Resolver<'a> {
    scope_stack: Vec<HashMap<String, NodeId>>,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Resolver {
            scope_stack: vec![HashMap::new()],
            diagnostics,
            corrupted: false,
        }
    }

    pub fn run(&mut self, hir: &Vec<HirStmt>, table: &mut NameTable) {
        for stmt in hir {
            self.resolve_stmt(stmt, table);
        }
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.push(HashMap::new())
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn declare(&mut self, name: String, id: NodeId, span: Span) {
        let current = self.scope_stack.last_mut().unwrap();
        if current.contains_key(&name) {
            self.report(
                format!("'{}' already exists in this scope", name),
                Some(span),
            );
        } else {
            current.insert(name, id);
        }
    }

    pub fn resolve_name(&mut self, name: &String, id: NodeId, span: Span, table: &mut NameTable) {
        for scope in self.scope_stack.iter().rev() {
            if let Some(decl_id) = scope.get(name) {
                table.resolved.insert(id, *decl_id);
                return;
            }
        }
        self.report(format!("'{}' is not declared", name), Some(span));
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
