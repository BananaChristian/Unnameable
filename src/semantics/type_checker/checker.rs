use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirStmt, HirStmtKind, HirType, HirTypeNode},
    lowering::NodeId,
    semantics::{
        semantics::{NameTable, ResolvedTypeKind, TypeInfo, TypesTable},
        type_checker::registry::TypeRegistry,
    },
};

pub struct TypeChecker<'a> {
    hir: &'a Vec<HirStmt>,
    pub name_table: &'a NameTable,
    pub types_table: &'a mut TypesTable,
    pub registry: TypeRegistry,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        diagnostics: &'a mut Diagnostics,
        hir: &'a Vec<HirStmt>,
        name_table: &'a NameTable,
        types_table: &'a mut TypesTable,
    ) -> Self {
        TypeChecker {
            hir,
            name_table,
            types_table,
            registry: TypeRegistry::new(),
            diagnostics,
            corrupted: false,
        }
    }

    pub fn check(&mut self) {
        for stmt in self.hir {
            self.check_stmt(stmt);
        }
    }

    pub fn unknown(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Unknown;
        let ty_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id: ty_id,
            span,
        }
    }

    pub fn primitive(&mut self, kind: ResolvedTypeKind, span: Span) -> TypeInfo {
        let type_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn func(&mut self, span: Span, params: Vec<TypeInfo>, ret: TypeInfo) -> TypeInfo {
        let _ps: Vec<String> = params
            .iter()
            .map(|param| format!("{}", param.name.clone()))
            .collect();
        let kind = ResolvedTypeKind::Func {
            params,
            ret_type: Box::new(ret),
        };
        let type_id = self.registry.issue_id(kind.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn array(&mut self, inner: TypeInfo, size: Option<u64>, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Array {
            inner: Box::new(inner.clone()),
            size,
        };
        let type_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn nullable(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Nullable {
            ty: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn failable(&mut self, ok: TypeInfo, err: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Failable {
            ok: Box::new(ok),
            err: Box::new(err),
        };
        let type_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn pointer(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Pointer {
            inner: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn reference(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Ref {
            inner: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn unit(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Unit;
        let type_id = self.registry.issue_id(kind.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn boolean(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Bool;
        let type_id = self.registry.issue_id(kind.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            span,
        }
    }

    pub fn custom(
        &mut self,
        span: Span,
        name: String,
        gen_params: Vec<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
    ) -> TypeInfo {
        let kind = ResolvedTypeKind::Custom {
            name: name.clone(),
            gen_type_params: gen_params.clone(),
            members: members.clone(),
        };
        let type_id = self.registry.issue_id(kind.clone());
        TypeInfo {
            kind: ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: gen_params,
                members,
            },
            name,
            type_id,
            span,
        }
    }

    pub fn types_match(&self, expected: &TypeInfo, actual: &TypeInfo) -> bool {
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
                self.types_match(a, b)
            }
            (ResolvedTypeKind::Ref { inner: a }, ResolvedTypeKind::Ref { inner: b }) => {
                self.types_match(a, b)
            }
            (
                ResolvedTypeKind::Func {
                    params: params_a,
                    ret_type: ret_a,
                },
                ResolvedTypeKind::Func {
                    params: params_b,
                    ret_type: ret_b,
                },
            ) => {
                if params_a.len() != params_b.len() {
                    return false;
                }
                let params_match = params_a
                    .iter()
                    .zip(params_b.iter())
                    .all(|(a, b)| self.types_match(a, b));

                params_match && self.types_match(ret_a, ret_b)
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
                let inners_match = self.types_match(a, b);
                let sizes_match = match (size_a, size_b) {
                    (Some(sa), Some(sb)) => sa == sb,
                    _ => true,
                };
                inners_match && sizes_match
            }
            (ResolvedTypeKind::Nullable { ty: a }, ResolvedTypeKind::Nullable { ty: b }) => {
                self.types_match(a, b)
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
            ) => self.types_match(ok_a, ok_b) && self.types_match(err_a, err_b),
            (
                ResolvedTypeKind::Custom {
                    name: name_a,
                    gen_type_params: gens_a,
                    ..
                },
                ResolvedTypeKind::Custom {
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
                    .all(|(a, b)| self.types_match(a, b))
            }
            (ResolvedTypeKind::GenericParam(a), ResolvedTypeKind::GenericParam(b)) => a == b,
            _ => false,
        }
    }

    pub fn type_from_hir_type(&mut self, ty: &HirTypeNode) -> TypeInfo {
        match &ty.kind {
            HirType::I8 => self.primitive(ResolvedTypeKind::I8, ty.span.clone()),
            HirType::I16 => self.primitive(ResolvedTypeKind::I16, ty.span.clone()),
            HirType::I32 => self.primitive(ResolvedTypeKind::I32, ty.span.clone()),
            HirType::I64 => self.primitive(ResolvedTypeKind::I64, ty.span.clone()),
            HirType::I128 => self.primitive(ResolvedTypeKind::I128, ty.span.clone()),
            HirType::U8 => self.primitive(ResolvedTypeKind::U8, ty.span.clone()),
            HirType::U16 => self.primitive(ResolvedTypeKind::U16, ty.span.clone()),
            HirType::U32 => self.primitive(ResolvedTypeKind::U32, ty.span.clone()),
            HirType::U64 => self.primitive(ResolvedTypeKind::U64, ty.span.clone()),
            HirType::U128 => self.primitive(ResolvedTypeKind::U128, ty.span.clone()),
            HirType::ISize => self.primitive(ResolvedTypeKind::ISize, ty.span.clone()),
            HirType::USize => self.primitive(ResolvedTypeKind::USize, ty.span.clone()),
            HirType::F32 => self.primitive(ResolvedTypeKind::F32, ty.span.clone()),
            HirType::F64 => self.primitive(ResolvedTypeKind::F64, ty.span.clone()),
            HirType::Bool => self.primitive(ResolvedTypeKind::Bool, ty.span.clone()),
            HirType::Unit => self.unit(ty.span.clone()),

            HirType::Ptr(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.pointer(inner_ty, ty.span.clone())
            }
            HirType::Ref(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.reference(inner_ty, ty.span.clone())
            }
            HirType::Nullable(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.nullable(inner_ty, ty.span.clone())
            }

            HirType::Failable(ok, err) => {
                let ok_ty = self.type_from_hir_type(ok);
                let err_ty = self.type_from_hir_type(err);
                self.failable(ok_ty, err_ty, ty.span.clone())
            }

            HirType::Array(inner, size) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.array(inner_ty, *size, ty.span.clone())
            }

            HirType::Func(params, return_type) => {
                let _ps = params
                    .iter()
                    .map(|p| self.type_from_hir_type(p).clone())
                    .collect();
                let ret_ty = self.type_from_hir_type(return_type);
                self.func(ty.span.clone(), _ps, ret_ty)
            }

            HirType::CustomType(name) => self.custom(ty.span.clone(), name.clone(), vec![], vec![]),
            HirType::GenericType { name, type_params } => {
                let _ps = type_params
                    .iter()
                    .map(|p| self.type_from_hir_type(p).clone())
                    .collect();
                self.custom(ty.span.clone(), name.clone(), _ps, vec![])
            }
        }
    }

    pub fn is_numeric(&self, ty: &TypeInfo) -> bool {
        match ty.kind {
            ResolvedTypeKind::I8
            | ResolvedTypeKind::U8
            | ResolvedTypeKind::I16
            | ResolvedTypeKind::U16
            | ResolvedTypeKind::I32
            | ResolvedTypeKind::U32
            | ResolvedTypeKind::I64
            | ResolvedTypeKind::U64
            | ResolvedTypeKind::I128
            | ResolvedTypeKind::U128
            | ResolvedTypeKind::ISize
            | ResolvedTypeKind::USize
            | ResolvedTypeKind::F32
            | ResolvedTypeKind::F64 => true,
            _ => false,
        }
    }

    pub fn declare_custom_types(&mut self, stmt: &HirStmt) {
        let ty_kind = match &stmt.kind {
            HirStmtKind::HirStructDecl {
                name,
                contracts,
                generic_type_params,
                fields,
                exposed,
            } => ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: generic_type_params
                    .iter()
                    .map(|gen_p| self.type_from_hir_type(gen_p))
                    .collect(),
                members: vec![],
            },
            _ => ResolvedTypeKind::Unknown,
        };

        let ty_id = self.registry.issue_id(ty_kind.clone());
        let ty_info = TypeInfo {
            kind: ty_kind.clone(),
            name: TypeInfo::name(ty_kind),
            type_id: ty_id,
            span: stmt.span.clone(),
        };

        self.types_table.types.insert(stmt.hir_id, ty_info);
    }

    pub fn get_decl_type(&mut self, decl_id: &NodeId) -> TypeInfo {
        match self.types_table.types.get(&decl_id) {
            Some(ty) => ty.clone(),
            None => self.unknown(Span { start: 0, end: 0 }),
        }
    }

    pub fn type_mismatch(&mut self, expected: &TypeInfo, actual: &TypeInfo, span: Span) {
        self.report(
            format!(
                "Type mismatch between '{}' and '{}'",
                expected.name, actual.name
            ),
            Some(span),
        );
    }

    pub fn insert(&mut self, id: NodeId, ty: TypeInfo) {
        self.types_table.types.insert(id, ty);
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
