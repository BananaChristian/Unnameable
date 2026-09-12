use crate::diagnostics::Span;
use crate::lowering::NodeId;
use crate::semantics::semantics::{ResolvedTypeKind, TypeId, TypeInfo};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Clone, Eq)]
pub struct StructuralTypeKey(pub ResolvedTypeKind);

impl StructuralTypeKey {
    pub fn strip_type(kind: &ResolvedTypeKind) -> ResolvedTypeKind {
        match kind {
            ResolvedTypeKind::Pointer { inner } => ResolvedTypeKind::Pointer {
                inner: Box::new(Self::clean_type_info(inner)),
            },
            ResolvedTypeKind::Ref { inner } => ResolvedTypeKind::Ref {
                inner: Box::new(Self::clean_type_info(inner)),
            },
            ResolvedTypeKind::Nullable { ty } => ResolvedTypeKind::Nullable {
                ty: Box::new(Self::clean_type_info(ty)),
            },
            ResolvedTypeKind::Array { inner, size } => ResolvedTypeKind::Array {
                inner: Box::new(Self::clean_type_info(inner)),
                size: *size,
            },

            ResolvedTypeKind::Failable { ok, err } => ResolvedTypeKind::Failable {
                ok: Box::new(Self::clean_type_info(ok)),
                err: Box::new(Self::clean_type_info(err)),
            },

            ResolvedTypeKind::Tuple { fields } => ResolvedTypeKind::Tuple {
                fields: fields.iter().map(Self::clean_type_info).collect(),
            },
            ResolvedTypeKind::Func {
                params,
                gen_type_params,
                ret_type,
                param_defaults: _,
            } => ResolvedTypeKind::Func {
                params: params.iter().map(Self::clean_type_info).collect(),
                gen_type_params: gen_type_params.iter().map(Self::clean_type_info).collect(),
                ret_type: Box::new(Self::clean_type_info(ret_type)),
                // Defaults are a call-site arity concern, not part of structural identity.
                param_defaults: Vec::new(),
            },

            ResolvedTypeKind::Struct {
                name,
                gen_type_params,
                ..
            } => ResolvedTypeKind::Struct {
                name: name.clone(),
                gen_type_params: gen_type_params.iter().map(Self::clean_type_info).collect(),
                members: Vec::new(),
            },
            ResolvedTypeKind::Enum {
                name,
                underlying,
                members,
            } => ResolvedTypeKind::Enum {
                name: name.clone(),
                underlying: Box::new(Self::clean_type_info(underlying)),
                members: members
                    .iter()
                    .map(|(name, info, _node_id)| {
                        (name.clone(), Self::clean_type_info(info), NodeId::default())
                    })
                    .collect(),
            },
            ResolvedTypeKind::Variant {
                name,
                gen_type_params,
                arms,
            } => ResolvedTypeKind::Variant {
                name: name.clone(),
                gen_type_params: gen_type_params.iter().map(Self::clean_type_info).collect(),
                arms: arms
                    .iter()
                    .map(|(arm_name, info, _node_id, payloads)| {
                        (
                            arm_name.clone(),
                            Self::clean_type_info(info),
                            NodeId::default(),
                            payloads.iter().map(Self::clean_type_info).collect(),
                        )
                    })
                    .collect(),
            },

            // Primitives & Unitary dont need this
            _ => kind.clone(),
        }
    }

    fn clean_type_info(info: &TypeInfo) -> TypeInfo {
        let mut clean = info.clone();
        clean.span = Span::default();
        clean.kind = Self::strip_type(&info.kind);
        clean
    }
}

impl PartialEq for StructuralTypeKey {
    fn eq(&self, other: &Self) -> bool {
        Self::strip_type(&self.0) == Self::strip_type(&other.0)
    }
}


impl Hash for StructuralTypeKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Self::strip_type(&self.0).hash(state);
    }
}

pub struct TypeRegistry {
    issued_types: HashMap<StructuralTypeKey, TypeId>,
    next_id: usize,
}

impl TypeRegistry {
    pub fn new() -> Self {
        TypeRegistry {
            issued_types: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn issue_id(&mut self, kind: ResolvedTypeKind) -> TypeId {
        let key = StructuralTypeKey(kind.clone());

        if let Some(existing_id) = self.issued_types.get(&key) {
            return existing_id.clone();
        }

        let new_id = TypeId(self.next_id);
        self.next_id += 1;
        self.issued_types.insert(key, new_id.clone());
        new_id
    }
}
