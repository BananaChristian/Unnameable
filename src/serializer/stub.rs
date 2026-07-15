use std::collections::HashMap;

use crate::semantics::TypeInfo;

pub struct ExportStub {
    module_name: String,
    pub exposed_symbols: HashMap<String, TypeInfo>,
}

impl ExportStub {
    pub fn new() -> Self {
        ExportStub {
            module_name: String::new(),
            exposed_symbols: HashMap::new(),
        }
    }
}
