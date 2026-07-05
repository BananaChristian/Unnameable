use std::collections::HashMap;

use crate::{
    semantics::semantics::{ResolvedTypeKind, TypeId},
};

pub struct TypeRegistry {
    issued_types: HashMap<ResolvedTypeKind, TypeId>,
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
        if let Some(existing_id) = self.issued_types.get(&kind) {
            return existing_id.clone();
        }

        let new_id = TypeId(self.next_id);
        self.next_id += 1;
        self.issued_types.insert(kind, new_id.clone());
        new_id
    }
}
