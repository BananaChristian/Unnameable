use std::collections::HashMap;

use crate::{
    hir::{HirStmt, HirStmtKind},
    lowering::NodeId,
};

#[derive(Debug)]
pub struct NodeIndex {
    pub nodes: HashMap<NodeId, HirStmt>,
}

impl NodeIndex {
    pub fn build(hir: &Vec<HirStmt>) -> Self {
        let mut index = NodeIndex {
            nodes: HashMap::new(),
        };

        for stmt in hir {
            index.index_stmt(&stmt);
        }

        index
    }

    pub fn get(&self, id: &NodeId) -> Option<&HirStmt> {
        self.nodes.get(id)
    }

    fn index_stmt(&mut self, stmt: &HirStmt) {
        self.nodes.insert(stmt.hir_id, stmt.clone());
        match &stmt.kind {
            HirStmtKind::HirIf {
                body, else_body, ..
            } => {
                for bod in body {
                    self.index_stmt(bod);
                }
                if let Some(el) = else_body {
                    for el_bod in el {
                        self.index_stmt(el_bod);
                    }
                }
            }
            HirStmtKind::HirWhile { body, .. } => {
                for b in body {
                    self.index_stmt(b);
                }
            }
            HirStmtKind::HirFunctionDef { body, .. } => {
                for child in body {
                    self.index_stmt(child);
                }
            }
            HirStmtKind::HirContractDecl { functions, .. } => {
                for func in functions {
                    self.index_stmt(func);
                }
            }
            _ => (),
        }
    }
}
