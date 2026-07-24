mod resolver;
mod semantics;
mod type_checker;

pub use semantics::{
    InstanceKey, ResolvedTypeKind, SemanticCtxt, Semantics, TypeId, TypeInfo, TypesTable,
};
