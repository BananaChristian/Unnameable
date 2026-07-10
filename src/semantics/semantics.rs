use std::{
    any::Any,
    collections::{HashMap, HashSet},
};

use crate::{
    diagnostics::{Diagnostics, Span},
    hir::{HirStmt, HirType, HirTypeNode},
    layout::Layout,
    lowering::NodeId,
    monomorph::Monomorphizer,
    semantics::{resolver::Resolver, type_checker::TypeChecker},
    target::TargetSpec,
};

#[derive(Debug)]
pub struct NameTable {
    pub resolved: HashMap<NodeId, NodeId>, //usage_id, declaration_id
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeInfo {
    pub kind: ResolvedTypeKind,
    pub name: String,
    pub type_id: TypeId,
    pub layout: Layout,
    pub span: Span,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
        gen_type_params: Vec<TypeInfo>,
        ret_type: Box<TypeInfo>,
    },
    //User defined
    Struct {
        name: String,
        gen_type_params: Vec<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
    },
    Enum {
        name: String,
        underlying: Box<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
    },
    Variant {
        name: String,
        gen_type_params: Vec<TypeInfo>,
        arms: Vec<(String, TypeInfo, Vec<TypeInfo>)>,
    },
    Tuple {
        fields: Vec<TypeInfo>,
    },
    Anonymous {
        fields: Vec<(String, TypeInfo)>,
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
            ResolvedTypeKind::Unknown => "unknown".to_string(),
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
            ResolvedTypeKind::GenericParam(name) => name,

            ResolvedTypeKind::Struct {
                name,
                gen_type_params,
                ..
            } => {
                if gen_type_params.is_empty() {
                    name
                } else {
                    let params: Vec<String> =
                        gen_type_params.iter().map(|p| p.name.clone()).collect();
                    format!("{}<{}>", name, params.join(", "))
                }
            }

            ResolvedTypeKind::Enum { name, .. } => name,

            ResolvedTypeKind::Variant {
                name,
                gen_type_params,
                ..
            } => {
                if gen_type_params.is_empty() {
                    name
                } else {
                    let params: Vec<String> =
                        gen_type_params.iter().map(|p| p.name.clone()).collect();
                    format!("{}<{}>", name, params.join(", "))
                }
            }

            ResolvedTypeKind::Func {
                params, ret_type, ..
            } => {
                let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();
                format!("func({}) : {}", param_names.join(", "), ret_type.name)
            }
        }
    }
}

#[derive(Debug)]
pub struct TypesTable {
    pub types: HashMap<NodeId, TypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceKey {
    // The unique ID of the original generic function or struct definition
    pub original_def_id: NodeId,
    // The actual concrete types chosen for this specific call (e.g., [Int32])
    pub concrete_args: Vec<TypeInfo>,
}

#[derive(Debug)]
pub struct SemanticCtxt {
    pub names: NameTable,
    pub types: TypesTable,
    pub monomorph_backlog: HashSet<InstanceKey>,
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
            monomorph_backlog: HashSet::new(),
        }
    }
}

pub struct Semantics<'a> {
    hir: Vec<HirStmt>,
    target_spec: &'a TargetSpec,
    diagnostics: &'a mut Diagnostics,
    pub ctxt: SemanticCtxt,
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
            &mut self.ctxt,
            self.target_spec,
        );
        checker.check();
        if checker.corrupted {
            self.corrupted = true;
            checker.flush_layout_errors();
        }
    }

    fn run_monormophizer(&mut self) {
        let mut monomorphizer = Monomorphizer::new(&mut self.ctxt, &mut self.hir);
        let cleaned = monomorphizer.run();
        println!("{:?}", cleaned);
    }

    pub fn analyze(&mut self) {
        self.run_resolver();
        self.run_type_checker();
        self.run_monormophizer();
    }
}
