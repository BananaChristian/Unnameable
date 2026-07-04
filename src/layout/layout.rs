use std::collections::HashMap;

use crate::{semantics::TypeId, target::TargetSpec};

pub struct Layout {
    pub size: usize,      //Total size in bytes
    pub alignment: usize, //Required alignment boundary
}

pub struct LayoutEngine {
    pub target: TargetSpec,
    pub cache: HashMap<TypeId, Layout>,
    recursion_stack: Vec<TypeId>,
}

impl LayoutEngine {
    pub fn new(target: TargetSpec) -> Self {
        LayoutEngine {
            target,
            cache: HashMap::new(),
            recursion_stack: Vec::new(),
        }
    }
}
