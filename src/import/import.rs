use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirStmt, HirStmtKind},
    serializer::ExportStub,
};

pub struct ImportCache {
    pub imported_stubs: HashMap<String, ExportStub>, //<alias_name, export_stub>
}

pub struct ImportEngine<'a> {
    pub resolved_imports: HashMap<String, Option<String>>, //<Original_name,Alias if any>
    pub imported_cache: ImportCache,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> ImportEngine<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        ImportEngine {
            resolved_imports: HashMap::new(),
            imported_cache: ImportCache {
                imported_stubs: HashMap::new(),
            },
            diagnostics,
            corrupted: false,
        }
    }

    pub fn test_run(&mut self, hir: &Vec<HirStmt>) {
        self.scan_for_import_stmts(hir);
    }

    fn is_alias_available(&self, alias_name: &String) -> bool {
        for (_, alias) in &self.resolved_imports {
            if let Some(existing) = alias {
                if *existing == *alias_name {
                    println!("IT IS AVAILABLE");
                    return false;
                } else {
                    println!("IT IS NOT AVAILABLE");
                    return true;
                }
            }
        }

        true
    }

    fn scan_for_import_stmts(&mut self, hir: &Vec<HirStmt>) {
        for stmt in hir {
            if let HirStmtKind::HirImport { name, alias } = &stmt.kind {
                if !self.resolved_imports.contains_key(name) {
                    if let Some(existing_alias) = alias {
                        if self.is_alias_available(existing_alias) {
                            self.resolved_imports.insert(name.clone(), alias.clone());
                        } else {
                            self.report(
                                format!("Already used alias '{}' ", existing_alias),
                                Some(stmt.span.clone()),
                            );
                        }
                    } else {
                        self.resolved_imports.insert(name.clone(), None);
                    }
                } else {
                    self.report(
                        format!("Already imported '{}'", name),
                        Some(stmt.span.clone()),
                    );
                }
            }
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        println!("ERROR Trigger");
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
