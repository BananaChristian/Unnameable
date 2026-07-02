use std::collections::HashMap;

use crate::{
    diagnostics::{Diagnostics, Span},
    hir::HirStmt,
    lowering::NodeId,
    semantics::resolver::Resolver,
};

pub struct NameTable {
    pub resolved: HashMap<NodeId, NodeId>,
}

#[derive(Clone)]
pub struct TypeInfo {
    pub kind: ResolvedTypeKind,
    pub span: Span,
}

#[derive(Clone)]
pub enum ResolvedTypeKind {
    //Primitives
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    ISize,
    USize,
    F32,
    F64,
    Bool,
    Unit,
    GenericParam(String),
    //Complex
    Pointer {
        inner: Box<TypeInfo>,
    },
    Ref {
        inner: Box<TypeInfo>,
    },
    Array {
        inner: Box<TypeInfo>,
        size: Option<u64>,
    },
    Func {
        params: Vec<TypeInfo>,
        ret_type: Box<TypeInfo>,
    },
    //User defined
    Struct {
        name: String,
        gen_type_params: Vec<TypeInfo>,
    },
    Enum {
        name: String,
    },
    Variant {
        name: String,
        gen_type_params: Vec<TypeInfo>,
    },
    Contract {
        name: String,
        gen_type_params: Vec<TypeInfo>,
    },

    Failable {
        ok: Box<TypeInfo>,
        err: Box<TypeInfo>,
    },
    Nullable {
        ty: Box<TypeInfo>,
    },
    Unknown,
}

pub struct TypesTable {
    pub types: HashMap<NodeId, TypeInfo>,
}

pub struct SemanticCtxt {
    pub names: NameTable,
    pub types: TypesTable,
}

impl SemanticCtxt {
    pub fn new() -> Self {
        SemanticCtxt {
            names: NameTable {
                resolved: HashMap::new(),
            },
            types: TypesTable {
                types: HashMap::new(),
            },
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
        if resolver.corrupted {
            self.corrupted = true;
        }
    }

    pub fn analyze(&mut self) {
        self.run_resolver();
    }
}
