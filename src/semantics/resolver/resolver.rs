use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::HirStmt,
    import::ImportEngine,
    lowering::NodeId,
    semantics::semantics::NameTable,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolKind {
    Declaration,
    Definition,
}

pub struct Resolver<'a> {
    pub scope_stack: Vec<HashMap<String, (NodeId, SymbolKind)>>,
    diagnostics: SharedDiagnostics,
    import: &'a ImportEngine,
    pub corrupted: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(diagnostics: SharedDiagnostics, import: &'a ImportEngine) -> Self {
        Resolver {
            scope_stack: vec![HashMap::new()],
            diagnostics,
            import,
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
        self.declare_with_kind(name, id, span, SymbolKind::Definition);
    }

    pub fn declare_with_kind(&mut self, name: String, id: NodeId, span: Span, kind: SymbolKind) {
        let current = self.scope_stack.last_mut().unwrap();

        if let Some((_, existing_kind)) = current.get(&name) {
            match (existing_kind, kind) {
                (SymbolKind::Declaration, SymbolKind::Definition) => {
                    // Upgrading a  declaration followed by definition is allowed
                    current.insert(name, (id, kind));
                    return;
                }
                (SymbolKind::Definition, SymbolKind::Declaration) => {
                    self.report(
                        format!(
                            "'{}' already has a definition, declaration is redundant",
                            name
                        ),
                        Some(span),
                    );
                    return;
                }
                (SymbolKind::Definition, SymbolKind::Definition) => {
                    self.report(
                        format!("'{}' already defined in this scope", name),
                        Some(span),
                    );
                    return;
                }
                (SymbolKind::Declaration, SymbolKind::Declaration) => {
                    self.report(
                        format!("'{}' already declared in this scope", name),
                        Some(span),
                    );
                    return;
                }
            }
        }

        current.insert(name, (id, kind));
    }

    pub fn resolve_name(&mut self, name: &String, id: NodeId, span: Span, table: &mut NameTable) {
        for scope in self.scope_stack.iter().rev() {
            if let Some((decl_id, _)) = scope.get(name) {
                table.resolved.insert(id, *decl_id);
                return;
            }
        }

        match self.import.resolve_imported_name(name) {
            Some(declid) => {
                table.resolved.insert(id, declid);
            }
            None => {
                self.report(format!("'{}' is not declared", name), Some(span));
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
