use std::collections::HashMap;

use crate::{
    diagnostics::Diagnostics,
    hir::HirStmt,
    lowering::NodeId,
    semantics::resolver::Resolver,
};

pub struct NameTable {
    pub resolved: HashMap<NodeId, NodeId>,
}

impl NameTable {
    pub fn new() -> Self {
        NameTable {
            resolved: HashMap::new(),
        }
    }
}

pub struct SemanticCtxt {
    pub names: NameTable,
}

impl SemanticCtxt {
    pub fn new() -> Self {
        SemanticCtxt {
            names: NameTable::new(),
        }
    }
}

pub struct Semantics<'a> {
    hir: Vec<HirStmt>,
    diagnostics: &'a mut Diagnostics,
    ctxt: SemanticCtxt,
    pub corrupted: bool,
}

impl<'a> Semantics<'a> {
    pub fn new(hir: Vec<HirStmt>, diagnostics: &'a mut Diagnostics) -> Self {
        Semantics {
            hir,
            diagnostics,
            ctxt: SemanticCtxt::new(),
            corrupted: false,
        }
    }

    fn run_resolver(&mut self) {
        let mut resolver = Resolver::new(self.diagnostics);
        resolver.run(&self.hir, &mut self.ctxt.names);
        if resolver.corrupted{
            self.corrupted=true;
        }
    }

    pub fn analyze(&mut self) {
        self.run_resolver();
    }
}
