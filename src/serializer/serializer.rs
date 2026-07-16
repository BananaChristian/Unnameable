use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    hir::{HirStmt, HirStmtKind},
    indexer::NodeIndex,
    lowering::NodeId,
    semantics::{SemanticCtxt, TypeInfo},
};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ExportStub {
    pub module_name: String,
    pub exposed_symbols: HashMap<String, TypeInfo>,
}

pub struct Serializer<'a> {
    module_name: String,
    ctxt: &'a SemanticCtxt,
    hir_index: &'a NodeIndex,
    stub: ExportStub,
}

impl<'a> Serializer<'a> {
    pub fn new(module_name: String, ctxt: &'a SemanticCtxt, hir_index: &'a NodeIndex) -> Self {
        Serializer {
            module_name: module_name.clone(),
            ctxt,
            hir_index,
            stub: ExportStub {
                module_name,
                exposed_symbols: HashMap::new(),
            },
        }
    }

    pub fn serialize(mut self) -> ExportStub {
        for (_, stmt) in &self.hir_index.nodes {
            self.serialize_stmt(stmt);
        }
        self.stub
    }

    fn mangle_name(&self, original_name: &String) -> String {
        format!("{}_{}", self.module_name, original_name)
    }

    fn get_ty_info(&self, id: &NodeId) -> &TypeInfo {
        self.ctxt
            .types
            .types
            .get(id)
            .expect("Failed to get type info for this id")
    }

    fn serialize_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirFunctionDecl { name, exposed, .. }
            | HirStmtKind::HirFunctionDef { name, exposed, .. }
            | HirStmtKind::HirStructDecl { name, exposed, .. }
            | HirStmtKind::HirVariantDecl { name, exposed, .. }
            | HirStmtKind::HirVarDecl { name, exposed, .. }
            | HirStmtKind::HirEnumDecl { name, exposed, .. } => {
                if *exposed {
                    let mangled_name = self.mangle_name(name);
                    let ty_info = self.get_ty_info(&stmt.hir_id);
                    self.stub
                        .exposed_symbols
                        .insert(mangled_name, ty_info.clone());
                }
            }
            _ => (),
        }
    }
}
