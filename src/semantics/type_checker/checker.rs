use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{HirStmt, HirStmtKind, HirType, HirTypeNode},
    lowering::NodeId,
    semantics::semantics::{NameTable, ResolvedTypeKind, TypeInfo, TypesTable},
};

pub struct TypeChecker<'a> {
    hir: &'a Vec<HirStmt>,
    pub name_table: &'a NameTable,
    pub types_table: &'a mut TypesTable,
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
            diagnostics,
            corrupted: false,
        }
    }

    pub fn check(&mut self) {
        for stmt in self.hir {
            self.check_stmt(stmt);
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

    pub fn type_from_hir_type(&self, ty: &HirTypeNode) -> TypeInfo {
        match &ty.kind {
            HirType::I8 => TypeInfo::primitive(ResolvedTypeKind::I8, ty.span.clone()),
            HirType::I16 => TypeInfo::primitive(ResolvedTypeKind::I16, ty.span.clone()),
            HirType::I32 => TypeInfo::primitive(ResolvedTypeKind::I32, ty.span.clone()),
            HirType::I64 => TypeInfo::primitive(ResolvedTypeKind::I64, ty.span.clone()),
            HirType::I128 => TypeInfo::primitive(ResolvedTypeKind::I128, ty.span.clone()),
            HirType::U8 => TypeInfo::primitive(ResolvedTypeKind::U8, ty.span.clone()),
            HirType::U16 => TypeInfo::primitive(ResolvedTypeKind::U16, ty.span.clone()),
            HirType::U32 => TypeInfo::primitive(ResolvedTypeKind::U32, ty.span.clone()),
            HirType::U64 => TypeInfo::primitive(ResolvedTypeKind::U64, ty.span.clone()),
            HirType::U128 => TypeInfo::primitive(ResolvedTypeKind::U128, ty.span.clone()),
            HirType::ISize => TypeInfo::primitive(ResolvedTypeKind::ISize, ty.span.clone()),
            HirType::USize => TypeInfo::primitive(ResolvedTypeKind::USize, ty.span.clone()),
            HirType::F32 => TypeInfo::primitive(ResolvedTypeKind::F32, ty.span.clone()),
            HirType::F64 => TypeInfo::primitive(ResolvedTypeKind::F64, ty.span.clone()),
            HirType::Bool => TypeInfo::primitive(ResolvedTypeKind::Bool, ty.span.clone()),
            HirType::Unit => TypeInfo::unit(ty.span.clone()),

            HirType::Ptr(inner) => {
                TypeInfo::pointer(self.type_from_hir_type(inner), ty.span.clone())
            }
            HirType::Ref(inner) => {
                TypeInfo::reference(self.type_from_hir_type(inner), ty.span.clone())
            }
            HirType::Nullable(inner) => {
                TypeInfo::nullable(self.type_from_hir_type(inner), ty.span.clone())
            }

            HirType::Failable(ok, err) => TypeInfo::failable(
                self.type_from_hir_type(ok),
                self.type_from_hir_type(err),
                ty.span.clone(),
            ),

            HirType::Array(inner, size) => {
                TypeInfo::array(self.type_from_hir_type(inner), *size, ty.span.clone())
            }

            HirType::Func(params, return_type) => TypeInfo::func(
                ty.span.clone(),
                params.iter().map(|p| self.type_from_hir_type(p)).collect(),
                self.type_from_hir_type(return_type),
            ),

            HirType::CustomType(name) => TypeInfo {
                kind: ResolvedTypeKind::Custom {
                    name: name.clone(),
                    gen_type_params: vec![],
                    members: vec![],
                },
                name: name.clone(),
                span: ty.span.clone(),
            },

            HirType::GenericType { name, type_params } => TypeInfo {
                kind: ResolvedTypeKind::Custom {
                    name: name.clone(),
                    gen_type_params: type_params
                        .iter()
                        .map(|p| self.type_from_hir_type(p))
                        .collect(),
                    members: vec![],
                },
                name: name.clone(),
                span: ty.span.clone(),
            },
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
        let ty_info = match &stmt.kind {
            HirStmtKind::HirEnumDecl {
                name,
                underlying,
                members,
                ..
            } => TypeInfo::custom(stmt.span.clone(), name.clone(), vec![], vec![]),
            HirStmtKind::HirStructDecl {
                name,
                contracts,
                generic_type_params,
                fields,
                ..
            } => {
                let gen_ps = generic_type_params
                    .iter()
                    .map(|g| self.type_from_hir_type(g))
                    .collect();
                TypeInfo::custom(stmt.span.clone(), name.clone(), gen_ps, vec![])
            }
            HirStmtKind::HirVariantDecl {
                name,
                contracts,
                members,
                generic_type_params,
                ..
            } => {
                let gen_ps = generic_type_params
                    .iter()
                    .map(|g| self.type_from_hir_type(g))
                    .collect();
                TypeInfo::custom(stmt.span.clone(), name.clone(), gen_ps, vec![])
            }
            _ => TypeInfo::unknown(stmt.span.clone()),
        };

        self.types_table.types.insert(stmt.hir_id, ty_info);
    }

    pub fn get_decl_type(&mut self, decl_id: &NodeId) -> TypeInfo {
        match self.types_table.types.get(&decl_id) {
            Some(ty) => ty.clone(),
            None => TypeInfo::unknown(Span { start: 0, end: 0 }),
        }
    }

    pub fn type_mismatch(&mut self, expected: &TypeInfo, actual: &TypeInfo, span: Span) {
        self.report(
            format!(
                "Type mismatch expected {} but got {}",
                expected.name, actual.name
            ),
            Some(span),
        );
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
