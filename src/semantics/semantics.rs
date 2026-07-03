use std::collections::HashMap;

use crate::{
    diagnostics::{Diagnostics, Span},
    hir::HirStmt,
    lowering::NodeId,
    semantics::{resolver::Resolver, type_checker::TypeChecker},
};

pub struct NameTable {
    pub resolved: HashMap<NodeId, NodeId>,
}

#[derive(Clone)]
pub struct TypeInfo {
    pub kind: ResolvedTypeKind,
    pub name: String,
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
    Custom {
        name: String,
        gen_type_params: Vec<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
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

impl TypeInfo {
    pub fn unknown(span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Unknown,
            name: "unknown".to_string(),
            span,
        }
    }

    pub fn primitive(kind: ResolvedTypeKind, span: Span) -> Self {
        let name = match kind {
            ResolvedTypeKind::I8 => "i8".to_string(),
            ResolvedTypeKind::U8 => "u8".to_string(),
            ResolvedTypeKind::I16 => "i16".to_string(),
            ResolvedTypeKind::U16 => "u16".to_string(),
            ResolvedTypeKind::I32 => "i32".to_string(),
            ResolvedTypeKind::U32 => "u32".to_string(),
            ResolvedTypeKind::I64 => "i64".to_string(),
            ResolvedTypeKind::U64 => "u64".to_string(),
            ResolvedTypeKind::I128 => "i128".to_string(),
            ResolvedTypeKind::U128 => "u128".to_string(),
            ResolvedTypeKind::USize => "usize".to_string(),
            ResolvedTypeKind::ISize => "isize".to_string(),
            ResolvedTypeKind::F32 => "f32".to_string(),
            ResolvedTypeKind::F64 => "f64".to_string(),
            ResolvedTypeKind::Bool => "bool".to_string(),
            _ => "unknown".to_string(),
        };
        TypeInfo { kind, name, span }
    }

    pub fn func(span: Span, params: Vec<TypeInfo>, ret: TypeInfo) -> Self {
        let _ps: Vec<String> = params
            .iter()
            .map(|param| format!("{}", param.name.clone()))
            .collect();

        TypeInfo {
            kind: ResolvedTypeKind::Func {
                params,
                ret_type: Box::new(ret.clone()),
            },
            name: format!("func():{}", ret.name),
            span,
        }
    }

    pub fn array(inner: TypeInfo, size: Option<u64>, span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Array {
                inner: Box::new(inner.clone()),
                size,
            },
            name: if let Some(s) = size {
                format!("arr[{},{}]", inner.name, s)
            } else {
                format!("arr[{}]", inner.name)
            },
            span,
        }
    }

    pub fn nullable(inner: TypeInfo, span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Nullable {
                ty: Box::new(inner.clone()),
            },
            name: format!("({})?", inner.name.clone()),
            span,
        }
    }

    pub fn failable(ok: TypeInfo, err: TypeInfo, span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Failable {
                ok: Box::new(ok.clone()),
                err: Box::new(err.clone()),
            },
            name: format!("!!({},{})", ok.name, err.name),
            span,
        }
    }

    pub fn pointer(inner: TypeInfo, span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Pointer {
                inner: Box::new(inner.clone()),
            },
            name: format!("ptr<{}>", inner.name),
            span,
        }
    }

    pub fn reference(inner: TypeInfo, span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Ref {
                inner: Box::new(inner.clone()),
            },
            name: format!("ref<{}>", inner.name.clone()),
            span,
        }
    }

    pub fn unit(span: Span) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Unit,
            name: format!("()"),
            span,
        }
    }

    pub fn boolean(span: Span) -> Self{
        TypeInfo{
            kind:ResolvedTypeKind::Bool,
            name: format!("bool"),
            span
        }
    }

    pub fn custom(
        span: Span,
        name: String,
        gen_params: Vec<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
    ) -> Self {
        TypeInfo {
            kind: ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: gen_params,
                members,
            },
            name,
            span,
        }
    }
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

    fn run_type_checker(&mut self) {
        let mut checker=TypeChecker::new(self.diagnostics, &self.hir, &mut self.ctxt.names, &mut self.ctxt.types);
        checker.check();
        if checker.corrupted{
            self.corrupted=true;
        }

    }

    pub fn analyze(&mut self) {
        self.run_resolver();
        self.run_type_checker();
    }
}
