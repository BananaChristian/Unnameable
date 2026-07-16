use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirStmt, HirStmtKind},
    serializer::ExportStub,
};

#[derive(Debug)]
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

    fn load_and_populate_cache(&mut self, stub_paths: &Vec<String>) {
        if self.corrupted {
            return;
        }

        let mut loaded_stubs: HashMap<String, ExportStub> = HashMap::new();
        for path in stub_paths {
            match self.read_and_deserialize_stub(path) {
                Ok(stub) => {
                    loaded_stubs.insert(stub.module_name.clone(), stub);
                }
                Err(msg) => {
                    self.report(msg, None);
                    return;
                }
            }
        }

        let imports_to_process: Vec<(String, Option<String>)> = self
            .resolved_imports
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        for (original_name, alias) in imports_to_process {
            if let Some(stub) = loaded_stubs.remove(&original_name) {
                let handle = alias.as_ref().unwrap_or(&original_name);
                self.imported_cache
                    .imported_stubs
                    .insert(handle.clone(), stub);
            } else {
                self.report(
                    format!("Module '{}' was imported but not provided", original_name),
                    None,
                );
            }
        }
    }

    pub fn import(&mut self, hir: &Vec<HirStmt>, stub_paths: &Vec<String>) {
        self.scan_for_import_stmts(hir);
        self.load_and_populate_cache(stub_paths);
    }

    fn is_alias_available(&self, alias_name: &str) -> bool {
        !self
            .resolved_imports
            .values()
            .any(|alias| alias.as_ref().map_or(false, |val| val == alias_name))
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

    fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }

    fn read_and_deserialize_stub(&mut self, path: &str) -> Result<ExportStub, String> {
        let bytes = std::fs::read(path)
            .map_err(|e| format!("Failed to read stub file '{}': {}", path, e))?;

        let stub: ExportStub = bincode::deserialize(&bytes).map_err(|e| {
            format!(
                "Failed to parse stub file '{}'. It might be corrupted: {}",
                path, e
            )
        })?;

        Ok(stub)
    }
}
