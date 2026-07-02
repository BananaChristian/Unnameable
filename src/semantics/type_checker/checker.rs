use std::collections::HashMap;

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
    node_idx: HashMap<NodeId, &'a HirStmt>,
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
        let mut checker = TypeChecker {
            hir,
            name_table,
            types_table,
            node_idx: HashMap::new(),
            diagnostics,
            corrupted: false,
        };
        checker.build_index();
        checker
    }

    fn build_index(&mut self) {
        for stmt in self.hir {
            self.index_stmt(stmt);
        }
    }

    fn index_stmt(&mut self, stmt: &'a HirStmt) {
        self.node_idx.insert(stmt.hir_id, stmt);
        match &stmt.kind {
            HirStmtKind::HirFunctionDef { body, .. } => {
                for s in body {
                    self.index_stmt(s);
                }
            }
            HirStmtKind::HirWhile { body, .. } => {
                for s in body {
                    self.index_stmt(s);
                }
            }
            HirStmtKind::HirIf {
                body, else_body, ..
            } => {
                for s in body {
                    self.index_stmt(s);
                }
                if let Some(else_stmts) = else_body {
                    for s in else_stmts {
                        self.index_stmt(s);
                    }
                }
            }
            _ => (),
        }
    }

    pub fn check(&self) {}

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

    fn type_from_hir_type(&self, ty: &HirTypeNode) -> TypeInfo {
        let kind = match &ty.kind {
            HirType::I8 => ResolvedTypeKind::I8,
            HirType::I16 => ResolvedTypeKind::I16,
            HirType::I32 => ResolvedTypeKind::I32,
            HirType::I64 => ResolvedTypeKind::I64,
            HirType::I128 => ResolvedTypeKind::I128,
            HirType::U8 => ResolvedTypeKind::U8,
            HirType::U16 => ResolvedTypeKind::U16,
            HirType::U32 => ResolvedTypeKind::U32,
            HirType::U64 => ResolvedTypeKind::U64,
            HirType::U128 => ResolvedTypeKind::U128,
            HirType::ISize => ResolvedTypeKind::ISize,
            HirType::USize => ResolvedTypeKind::USize,
            HirType::F32 => ResolvedTypeKind::F32,
            HirType::F64 => ResolvedTypeKind::F64,
            HirType::Bool => ResolvedTypeKind::Bool,
            HirType::Unit => ResolvedTypeKind::Unit,

            HirType::Ptr(inner) => ResolvedTypeKind::Pointer {
                inner: Box::new(self.type_from_hir_type(inner)),
            },

            HirType::Ref(inner) => ResolvedTypeKind::Ref {
                inner: Box::new(self.type_from_hir_type(inner)),
            },

            HirType::Nullable(inner) => self.type_from_hir_type(inner).kind,

            HirType::Failable(ok, err) => ResolvedTypeKind::Failable {
                ok: Box::new(self.type_from_hir_type(ok)),
                err: Box::new(self.type_from_hir_type(err)),
            },

            HirType::Array(inner, size) => ResolvedTypeKind::Array {
                inner: Box::new(self.type_from_hir_type(inner)),
                size: None, //Will handle this later
            },

            HirType::Func(params, return_type) => ResolvedTypeKind::Func {
                params: params.iter().map(|p| self.type_from_hir_type(p)).collect(),
                ret_type: Box::new(self.type_from_hir_type(return_type)),
            },

            HirType::CustomType(name) => ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: vec![],
                members: vec![],
            },

            HirType::GenericType { name, type_params } => ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: type_params
                    .iter()
                    .map(|p| self.type_from_hir_type(p))
                    .collect(),
                members: vec![],
            },
        };

        TypeInfo {
            kind,
            span: ty.span.clone(),
        }
    }

    pub fn declare_type(&mut self, stmt: &HirStmt) {
        let kind = match &stmt.kind {
            HirStmtKind::HirStructDecl {
                name,
                generic_type_params,
                fields,
                ..
            } => ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: generic_type_params
                    .iter()
                    .map(|ty| self.type_from_hir_type(ty))
                    .collect(),
                members: fields
                    .iter()
                    .map(|f| (f.name.clone(), self.type_from_hir_type(&f.ty)))
                    .collect(),
            },

            HirStmtKind::HirEnumDecl {
                name,
                underlying,
                members,
                ..
            } => ResolvedTypeKind::Custom {
                name: name.clone(),
                gen_type_params: vec![],
                members: members
                    .iter()
                    .map(|m| (m.name.clone(), self.type_from_hir_type(underlying)))
                    .collect(),
            },

            HirStmtKind::HirVariantDecl {
                name,
                generic_type_params,
                members,
                ..
            } => {
                ResolvedTypeKind::Custom {
                    name: name.clone(),
                    gen_type_params: generic_type_params
                        .iter()
                        .map(|ty| self.type_from_hir_type(ty))
                        .collect(),
                    members: members
                        .iter()
                        .map(|m| {
                            let member_ty = if m.member_types.is_empty() {
                                // no data — just the variant type itself
                                TypeInfo {
                                    kind: ResolvedTypeKind::Custom {
                                        name: name.clone(),
                                        gen_type_params: vec![],
                                        members: vec![],
                                    },
                                    span: m.span.clone(),
                                }
                            } else {
                                // has data — model as a function returning the variant type
                                TypeInfo {
                                    kind: ResolvedTypeKind::Func {
                                        params: m
                                            .member_types
                                            .iter()
                                            .map(|t| self.type_from_hir_type(t))
                                            .collect(),
                                        ret_type: Box::new(TypeInfo {
                                            kind: ResolvedTypeKind::Custom {
                                                name: name.clone(),
                                                gen_type_params: vec![],
                                                members: vec![],
                                            },
                                            span: m.span.clone(),
                                        }),
                                    },
                                    span: m.span.clone(),
                                }
                            };
                            (m.name.clone(), member_ty)
                        })
                        .collect(),
                }
            }

            _ => ResolvedTypeKind::Unknown,
        };

        let ty_info = TypeInfo {
            kind,
            span: stmt.span.clone(),
        };
        self.types_table.types.insert(stmt.hir_id, ty_info);
    }

    pub fn get_decl_type(&mut self, decl_id: &NodeId) -> TypeInfo {
        match self.types_table.types.get(&decl_id) {
            Some(ty) => ty.clone(),
            None => TypeInfo {
                kind: ResolvedTypeKind::Unknown,
                span: Span::new(0, 0),
            },
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
