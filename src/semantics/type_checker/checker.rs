use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::HirStmt,
    semantics::semantics::{NameTable, ResolvedTypeKind, TypeInfo, TypesTable},
};

pub struct TypeChecker<'a> {
    hir: &'a Vec<HirStmt>,
    name_table: &'a NameTable,
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
                ResolvedTypeKind::Struct {
                    name: name_a,
                    gen_type_params: gens_a,
                },
                ResolvedTypeKind::Struct {
                    name: name_b,
                    gen_type_params: gens_b,
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
            (ResolvedTypeKind::Enum { name: a }, ResolvedTypeKind::Enum { name: b }) => a == b,
            (
                ResolvedTypeKind::Variant {
                    name: name_a,
                    gen_type_params: gens_a,
                },
                ResolvedTypeKind::Variant {
                    name: name_b,
                    gen_type_params: gens_b,
                },
            ) => {
                if name_a != name_b {
                    return false;
                }
                gens_a
                    .iter()
                    .zip(gens_b.iter())
                    .all(|(a, b)| self.types_match(a, b))
            }
            (ResolvedTypeKind::GenericParam(a), ResolvedTypeKind::GenericParam(b)) => a == b,
            (
                ResolvedTypeKind::Contract {
                    name: name_a,
                    gen_type_params: gens_a,
                },
                ResolvedTypeKind::Contract {
                    name: name_b,
                    gen_type_params: gens_b,
                },
            ) => {
                if name_a != name_b {
                    return false;
                }
                gens_a
                    .iter()
                    .zip(gens_b.iter())
                    .all(|(a, b)| self.types_match(a, b))
            }
            _ => false,
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
