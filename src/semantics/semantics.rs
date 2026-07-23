use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    cf_checker::ControlFlowChecker,
    contract_verifier::ContractVerifier,
    diagnostics::{SharedDiagnostics, Span},
    hir::HirStmt,
    import::ImportEngine,
    indexer::NodeIndex,
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

#[derive(Serialize, Deserialize, Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeId(pub usize);

#[derive(Serialize, Deserialize, Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeInfo {
    pub kind: ResolvedTypeKind,
    pub name: String,
    pub type_id: TypeId,
    pub layout: Layout,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Debug, Clone, Hash, Eq, PartialEq)]
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

    pub fn types_match(expected: &TypeInfo, actual: &TypeInfo) -> bool {
        match (&expected.kind, &actual.kind) {
            (ResolvedTypeKind::Unknown, ResolvedTypeKind::Unknown) => true,
            (ResolvedTypeKind::I8, ResolvedTypeKind::I8) => true,
            (ResolvedTypeKind::U8, ResolvedTypeKind::U8) => true,
            (ResolvedTypeKind::I16, ResolvedTypeKind::I16) => true,
            (ResolvedTypeKind::U16, ResolvedTypeKind::U16) => true,
            (ResolvedTypeKind::I32, ResolvedTypeKind::I32) => true,
            (ResolvedTypeKind::U32, ResolvedTypeKind::U32) => true,
            (ResolvedTypeKind::I64, ResolvedTypeKind::I64) => true,
            (ResolvedTypeKind::U64, ResolvedTypeKind::U64) => true,
            (ResolvedTypeKind::I128, ResolvedTypeKind::U128) => true,
            (ResolvedTypeKind::U128, ResolvedTypeKind::U128) => true,
            (ResolvedTypeKind::ISize, ResolvedTypeKind::ISize) => true,
            (ResolvedTypeKind::USize, ResolvedTypeKind::USize) => true,
            (ResolvedTypeKind::F32, ResolvedTypeKind::F32) => true,
            (ResolvedTypeKind::F64, ResolvedTypeKind::F64) => true,
            (ResolvedTypeKind::Bool, ResolvedTypeKind::Bool) => true,
            (ResolvedTypeKind::Unit, ResolvedTypeKind::Unit) => true,
            (ResolvedTypeKind::Pointer { inner: a }, ResolvedTypeKind::Pointer { inner: b }) => {
                TypeInfo::types_match(a, b)
            }
            (ResolvedTypeKind::Ref { inner: a }, ResolvedTypeKind::Ref { inner: b }) => {
                TypeInfo::types_match(a, b)
            }
            (
                ResolvedTypeKind::Func {
                    params: params_a,
                    ret_type: ret_a,
                    ..
                },
                ResolvedTypeKind::Func {
                    params: params_b,
                    ret_type: ret_b,
                    ..
                },
            ) => {
                if params_a.len() != params_b.len() {
                    return false;
                }
                let params_match = params_a
                    .iter()
                    .zip(params_b.iter())
                    .all(|(a, b)| TypeInfo::types_match(a, b));

                params_match && TypeInfo::types_match(ret_a, ret_b)
            }
            (
                ResolvedTypeKind::Array {
                    inner: a,
                    size: size_a,
                },
                ResolvedTypeKind::Array {
                    inner: b,
                    size: size_b,
                },
            ) => {
                let inners_match = TypeInfo::types_match(a, b);
                let sizes_match = match (size_a, size_b) {
                    (Some(sa), Some(sb)) => sa == sb,
                    _ => true,
                };
                inners_match && sizes_match
            }
            (ResolvedTypeKind::Nullable { ty: a }, ResolvedTypeKind::Nullable { ty: b }) => {
                TypeInfo::types_match(a, b)
            }
            (
                ResolvedTypeKind::Failable {
                    ok: ok_a,
                    err: err_a,
                },
                ResolvedTypeKind::Failable {
                    ok: ok_b,
                    err: err_b,
                },
            ) => TypeInfo::types_match(ok_a, ok_b) && TypeInfo::types_match(err_a, err_b),
            (
                ResolvedTypeKind::Struct {
                    name: name_a,
                    gen_type_params: gens_a,
                    ..
                },
                ResolvedTypeKind::Struct {
                    name: name_b,
                    gen_type_params: gens_b,
                    ..
                },
            ) => {
                if name_a != name_b {
                    return false;
                }
                if gens_a.len() != gens_b.len() {
                    return false;
                }
                gens_a
                    .iter()
                    .zip(gens_b.iter())
                    .all(|(a, b)| TypeInfo::types_match(a, b))
            }
            (ResolvedTypeKind::GenericParam(a), ResolvedTypeKind::GenericParam(b)) => a == b,
            _ => false,
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
pub struct ContractTable {
    pub implementations: HashMap<NodeId, Vec<String>>,
}

#[derive(Debug)]
pub struct SemanticCtxt {
    pub names: NameTable,
    pub types: TypesTable,
    pub monomorph_backlog: HashSet<InstanceKey>,
    pub contracts: ContractTable,
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
            contracts: ContractTable {
                implementations: HashMap::new(),
            },
        }
    }
}

pub struct Semantics<'a> {
    hir: Vec<HirStmt>,
    target_spec: &'a TargetSpec,
    pub ctxt: SemanticCtxt,
    pub corrupted: bool,
}

impl<'a> Semantics<'a> {
    pub fn new(hir: Vec<HirStmt>, target_spec: &'a TargetSpec) -> Self {
        Semantics {
            hir,
            target_spec,
            ctxt: SemanticCtxt::new(),
            corrupted: false,
        }
    }

    fn run_resolver(&mut self, diagnostics: SharedDiagnostics, importer: &ImportEngine) {
        let mut resolver = Resolver::new(diagnostics, importer);
        resolver.run(&self.hir, &mut self.ctxt.names);
        if resolver.corrupted {
            self.corrupted = true;
        }
    }

    fn run_type_checker(&mut self, diagnostics: SharedDiagnostics, import: &ImportEngine) {
        let mut checker = TypeChecker::new(
            diagnostics,
            &self.hir,
            &mut self.ctxt,
            self.target_spec,
            import,
        );
        checker.check();
        if checker.corrupted {
            self.corrupted = true;
            checker.flush_layout_errors();
        }
    }

    pub fn generate_monormophizer_hir(&mut self) -> Vec<HirStmt> {
        let mut monomorphizer = Monomorphizer::new(&mut self.ctxt, &mut self.hir);
        monomorphizer.run()
    }

    pub fn verify_contracts(
        &mut self,
        hir_index: &NodeIndex,
        diagnostics: SharedDiagnostics,
    ) -> bool {
        let mut verifier = ContractVerifier::new(hir_index, &mut self.ctxt, diagnostics);
        verifier.run();
        verifier.corrupted
    }

    pub fn check_control_flow(
        &mut self,
        hir_index: &NodeIndex,
        diagnostics: SharedDiagnostics,
    ) -> bool {
        let mut checker = ControlFlowChecker::new(hir_index, &self.ctxt, diagnostics);
        checker.run();
        checker.corrupted
    }

    pub fn analyze(&mut self, diagnostics: SharedDiagnostics, importer: &ImportEngine) {
        self.run_resolver(Rc::clone(&diagnostics), importer);
        self.run_type_checker(Rc::clone(&diagnostics), importer);
    }
}
