use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirStmt, HirStmtKind},
    lowering::NodeId,
    semantics::TypeInfo,
    serializer::ExportStub,
};

#[derive(Debug)]
pub struct ImportCache {
    pub imported_stubs: HashMap<String, ExportStub>, //<alias_name, export_stub>
}

pub struct ImportEngine {
    pub resolved_imports: HashMap<String, Option<String>>, //<Original_name,Alias if any>
    pub symbol_aliases: HashMap<String, String>, //<alias, original> (MyStruct,test_whatever)
    pub symbol_declarations: HashMap<String, NodeId>,
    pub imported_cache: ImportCache,
    diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl ImportEngine {
    pub fn new(diagnostics: SharedDiagnostics) -> Self {
        ImportEngine {
            resolved_imports: HashMap::new(),
            imported_cache: ImportCache {
                imported_stubs: HashMap::new(),
            },
            symbol_aliases: HashMap::new(),
            symbol_declarations: HashMap::new(),
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

        println!("SYMBOL ALIASES{:?}", self.symbol_aliases);
        println!("SYMBOL DECLARATIONS: {:?}", self.symbol_declarations);

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

    pub fn import(&mut self, hir: &mut Vec<HirStmt>, stub_paths: &Vec<String>) {
        self.scan_for_import_stmts(hir);
        self.load_and_populate_cache(stub_paths);
    }

    fn is_alias_available(&self, alias_name: &str) -> bool {
        !self
            .resolved_imports
            .values()
            .any(|alias| alias.as_ref().map_or(false, |val| val == alias_name))
    }

    fn swap_ids(&mut self, input: &mut NodeId) {
        input.external = input.local; //Swap the external with the local held
        input.local = 0; //Make the local 0 since this isnt local
    }

    fn scan_for_import_stmts(&mut self, hir: &mut Vec<HirStmt>) {
        for stmt in hir {
            if let HirStmtKind::HirImport { name, alias } = &stmt.kind {
                if self.is_mangled(name) {
                    let demangled = self.demangle_name(name);
                    let module_candidate = demangled.0.unwrap_or(name.clone());

                    if let Some(existing_alias) = alias {
                        if self.is_alias_available(existing_alias) {
                            self.swap_ids(&mut stmt.hir_id);
                            self.resolved_imports
                                .entry(module_candidate)
                                .or_insert(None);

                            self.symbol_aliases
                                .insert(existing_alias.clone(), name.clone());
                            self.symbol_declarations
                                .insert(existing_alias.clone(), stmt.hir_id.clone());
                        } else {
                            self.report(
                                format!("Already used alias '{}' ", existing_alias),
                                Some(stmt.span.clone()),
                            );
                        }
                    } else {
                        if let Some(symbol_name) = demangled.1 {
                            self.swap_ids(&mut stmt.hir_id);
                            self.resolved_imports
                                .entry(module_candidate)
                                .or_insert(None);
                            self.symbol_aliases
                                .insert(symbol_name.clone(), name.clone());
                            self.symbol_declarations
                                .insert(symbol_name.clone(), stmt.hir_id.clone());
                        }
                    }
                } else {
                    let candidate = name.clone();
                    if !self.resolved_imports.contains_key(&candidate) {
                        if let Some(existing_alias) = alias {
                            if self.is_alias_available(existing_alias) {
                                self.swap_ids(&mut stmt.hir_id);
                                self.resolved_imports.insert(name.clone(), alias.clone());
                                self.symbol_declarations
                                    .insert(existing_alias.clone(), stmt.hir_id.clone());
                            } else {
                                self.report(
                                    format!("Already used alias '{}' ", existing_alias),
                                    Some(stmt.span.clone()),
                                );
                            }
                        } else {
                            self.swap_ids(&mut stmt.hir_id);
                            self.resolved_imports.insert(name.clone(), None);
                            self.symbol_declarations
                                .insert(name.clone(), stmt.hir_id.clone());
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
    }

    fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
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

    fn demangle_name(&self, mangled: &str) -> (Option<String>, Option<String>) {
        match mangled.find("_") {
            Some(idx) if idx > 0 && idx < mangled.len() - 1 => {
                let prefix = &mangled[..idx];
                let symbol = &mangled[idx + 1..];
                (Some(prefix.to_string()), Some(symbol.to_string()))
            }
            _ => (Some(mangled.to_string()), None),
        }
    }

    fn is_mangled(&self, name: &str) -> bool {
        match name.find("_") {
            Some(idx) => idx > 0 && idx < name.len() - 1,
            None => false,
        }
    }


    pub fn resolve_imported_name(&self, name: &str) -> Option<NodeId> {
        let target_name = match self.symbol_aliases.get(name) {
            Some(mangled_target) => mangled_target.as_str(),
            None => name,
        };

        let node_id = *self
            .symbol_declarations
            .get(name)
            .or_else(|| self.symbol_declarations.get(target_name))?;

        let prefix = self.demangle_name(target_name).0?;

        let stub = self.imported_cache.imported_stubs.get(&prefix)?;

        if stub.exposed_symbols.contains_key(target_name) {
            return Some(node_id);
        }

        None
    }

    pub fn resolve_external_ty(&self, decl_id: &NodeId) -> Option<&TypeInfo> {
        for (symbol, target_id) in &self.symbol_declarations {
            if *decl_id == *target_id {
                //We have gotten the id we want
                //Get the mangled name from the actual symbol this mangled name has the module name
                //in it
                let mangled_name = self.symbol_aliases.get(symbol)?;
                //Demangle it and get the module name
                let module_name = self.demangle_name(mangled_name).0?;
                if let Some(stub) = self.imported_cache.imported_stubs.get(&module_name) {
                    if let Some(ty_info) = stub.exposed_symbols.get(mangled_name.as_str()) {
                        return Some(ty_info);
                    }
                }
            }
        }

        None
    }
    
}
