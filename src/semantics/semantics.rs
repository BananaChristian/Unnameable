use std::collections::HashMap;

use crate::{
    diagnostics::{Diagnostics, Span},
    hir::HirStmt,
    layout::Layout,
    lowering::NodeId,
    semantics::{resolver::Resolver, type_checker::TypeChecker},
    target::TargetSpec,
};

pub struct NameTable {
    pub resolved: HashMap<NodeId, NodeId>,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct TypeId(pub usize);

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct TypeInfo {
    pub kind: ResolvedTypeKind,
    pub name: String,
    pub type_id: TypeId,
    pub layout: Layout,
    pub span: Span,
}

#[derive(Clone, Hash, Eq, PartialEq)]
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
    Tuple {
        fields: Vec<TypeInfo>,
    },
    Anonymous {
        fields: Vec<(String, TypeInfo)>,
    },
    Unknown,
}

impl TypeInfo {
    pub fn name(kind: ResolvedTypeKind) -> String {
        match kind {
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
            ResolvedTypeKind::Unit => "()".to_string(),
            ResolvedTypeKind::Array { inner, size } => {
                if let Some(s) = size {
                    format!("arr[{},{}]", inner.name, s)
                } else {
                    format!("arr[{}]", inner.name)
                }
            }
            ResolvedTypeKind::Nullable { ty } => {
                format!("({})?", ty.name)
            }
            ResolvedTypeKind::Failable { ok, err } => {
                format!("!!({},{})", ok.name, err.name)
            }
            ResolvedTypeKind::Pointer { inner } => {
                format!("ptr<{}>", inner.name)
            }
            ResolvedTypeKind::Ref { inner } => {
                format!("ref<{}>", inner.name.clone())
            }
            ResolvedTypeKind::Tuple { fields } => {
                let element_names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
                format!("({})", element_names.join(", "))
            }

            ResolvedTypeKind::Anonymous { fields } => {
                let field_strings: Vec<String> = fields
                    .iter()
                    .map(|(name, info)| format!(".{}: {}", name, info.name))
                    .collect();
                format!(".{{ {} }}", field_strings.join(", "))
            }
            _ => "unknown".to_string(),
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
    target_spec: &'a TargetSpec,
    diagnostics: &'a mut Diagnostics,
    ctxt: SemanticCtxt,
    pub corrupted: bool,
}

impl<'a> Semantics<'a> {
    pub fn new(
        hir: Vec<HirStmt>,
        target_spec: &'a TargetSpec,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        Semantics {
            hir,
            target_spec,
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
        let mut checker = TypeChecker::new(
            self.diagnostics,
            &self.hir,
            &mut self.ctxt.names,
            self.target_spec,
            &mut self.ctxt.types,
        );
        checker.check();
        if checker.corrupted {
            self.corrupted = true;
            checker.flush_layout_errors();
        }
    }

    pub fn analyze(&mut self) {
        self.run_resolver();
        self.run_type_checker();
    }
}
