use std::collections::{HashMap, HashSet};

use crate::{
    hir::{HirStmt, HirStmtKind, HirType, HirTypeNode},
    lowering::NodeId,
    semantics::{InstanceKey, ResolvedTypeKind, SemanticCtxt, TypeInfo},
};

pub struct Monomorphizer<'a> {
    pub ctxt: &'a mut SemanticCtxt,
    hir: &'a Vec<HirStmt>,
    pub mangled_mappings: HashMap<InstanceKey, String>,
    pub processed_instances: HashSet<InstanceKey>,
    pub generated_stmts: Vec<HirStmt>,
}

impl<'a> Monomorphizer<'a> {
    pub fn new(ctxt: &'a mut SemanticCtxt, hir: &'a Vec<HirStmt>) -> Self {
        Monomorphizer {
            ctxt,
            hir,
            mangled_mappings: HashMap::new(),
            processed_instances: HashSet::new(),
            generated_stmts: Vec::new(),
        }
    }

    pub fn get_decl(&mut self, node_id: &NodeId) -> Option<&HirStmt> {
        for stmt in self.hir {
            if *node_id == stmt.hir_id {
                return Some(stmt);
            }
        }
        None
    }

    pub fn extract_stmt_name(&mut self, node_id: &NodeId) -> String {
        let stmt = self
            .get_decl(node_id)
            .expect("Failed to get the valid statement");
        match &stmt.kind {
            HirStmtKind::HirFunctionDef { name, .. } => name.clone(),
            HirStmtKind::HirFunctionDecl { name, .. } => name.clone(),
            HirStmtKind::HirStructDecl { name, .. } => name.clone(),
            HirStmtKind::HirVariantDecl { name, .. } => name.clone(),
            _ => "unknown_generic".to_string(),
        }
    }

    pub fn mangle_name(&self, base_name: &String, concrete_args: &[TypeInfo]) -> String {
        let mut mangled = format!("_U_{}", base_name);

        for arg in concrete_args {
            let clean_ty_name = arg
                .name
                .replace('(', "_")
                .replace(')', "_")
                .replace(':', "_")
                .replace('<', "_")
                .replace('>', "_")
                .replace(' ', "");
            mangled.push_str(&format!("_{}", clean_ty_name));
        }

        mangled
    }

    pub fn build_type_node(&mut self, info: &TypeInfo, id: NodeId) -> HirTypeNode {
        let ty_kind = match &info.kind {
            ResolvedTypeKind::I8 => HirType::I8,
            ResolvedTypeKind::U8 => HirType::U8,
            ResolvedTypeKind::I16 => HirType::I16,
            ResolvedTypeKind::U16 => HirType::U16,
            ResolvedTypeKind::I32 => HirType::I32,
            ResolvedTypeKind::U32 => HirType::U32,
            ResolvedTypeKind::I64 => HirType::I64,
            ResolvedTypeKind::U64 => HirType::U64,
            ResolvedTypeKind::I128 => HirType::I128,
            ResolvedTypeKind::U128 => HirType::U128,
            ResolvedTypeKind::ISize => HirType::ISize,
            ResolvedTypeKind::USize => HirType::USize,
            ResolvedTypeKind::F32 => HirType::F32,
            ResolvedTypeKind::F64 => HirType::F64,
            ResolvedTypeKind::Bool => HirType::Bool,
            ResolvedTypeKind::Unit => HirType::Unit,
            ResolvedTypeKind::Unknown => HirType::Unit,
            ResolvedTypeKind::Ref { inner } => {
                let inner_ty = self.build_type_node(&inner.clone(), id);
                HirType::Ref(Box::new(inner_ty))
            }
            ResolvedTypeKind::Pointer { inner } => {
                let inner_ty = self.build_type_node(&inner.clone(), id);
                HirType::Ptr(Box::new(inner_ty))
            }
            _ => HirType::Unit,
        };

        HirTypeNode {
            hir_id: id,
            kind: ty_kind,
            span: info.span.clone(),
        }
    }

    pub fn substitute_type(
        &mut self,
        ty_node: &mut HirTypeNode,
        generic_params: &[HirTypeNode],
        concrete_args: &[TypeInfo],
    ) {
        if let HirType::GenericPlaceHolder(name) = &ty_node.kind {
            if let Some(index) = generic_params.iter().position(|param| match &param.kind {
                HirType::GenericPlaceHolder(param_name) => *param_name == *name,
                _ => false,
            }) {
                if let Some(concrete_ty) = concrete_args.get(index) {
                    *ty_node = self.build_type_node(concrete_ty, ty_node.hir_id);
                }
            }
        }

        match &ty_node.kind {
            HirType::Ptr(inner) | HirType::Ref(inner) => {
                self.substitute_type(&mut inner.clone(), generic_params, concrete_args)
            }
            _ => {}
        }
    }

    fn generate_unified_tree(&mut self) -> Vec<HirStmt> {
        let mut monorphized_tree = Vec::new();
        for original in self.hir.iter() {
            match &original.kind {
                HirStmtKind::HirFunctionDef {
                    generic_type_params,
                    ..
                }
                | HirStmtKind::HirStructDecl {
                    generic_type_params,
                    ..
                } => {
                    if !generic_type_params.is_empty() {
                        continue;
                    }

                    let mut production_stmt = original.clone();
                    let current_name = self.extract_stmt_name(&production_stmt.hir_id);
                    self.monormophize_stmt(
                        &mut production_stmt,
                        &[],
                        &[],
                        current_name.to_string(),
                    );
                    monorphized_tree.push(production_stmt)
                }
                _ => {
                    let mut production_stmt = original.clone();
                    let current_name = self.extract_stmt_name(&production_stmt.hir_id);
                    self.monormophize_stmt(
                        &mut production_stmt,
                        &[],
                        &[],
                        current_name.to_string(),
                    );
                    monorphized_tree.push(production_stmt)
                }
            }
        }

        let mut concrete_instances = std::mem::take(&mut self.generated_stmts);
        monorphized_tree.append(&mut concrete_instances);
        monorphized_tree
    }

    pub fn run(&mut self) -> Vec<HirStmt> {
        let backlog_items: Vec<InstanceKey> = self.ctxt.monomorph_backlog.iter().cloned().collect();
        for instance in &backlog_items {
            let base_name = self.extract_stmt_name(&instance.original_def_id);
            let mangled_name = self.mangle_name(&base_name, &instance.concrete_args);

            self.mangled_mappings.insert(instance.clone(), mangled_name);
        }

        for instance in &backlog_items {
            if self.processed_instances.contains(&instance) {
                continue;
            }

            self.processed_instances.insert(instance.clone());
            if let Some(template_stmt) = self.get_decl(&instance.original_def_id) {
                let mut concrete_clone = template_stmt.clone();
                let new_name = self
                    .mangled_mappings
                    .get(instance)
                    .cloned()
                    .expect("Missing mangled name mappings");

                let gens = match &concrete_clone.kind {
                    HirStmtKind::HirFunctionDecl {
                        generic_type_params,
                        ..
                    }
                    | HirStmtKind::HirFunctionDef {
                        generic_type_params,
                        ..
                    }
                    | HirStmtKind::HirStructDecl {
                        generic_type_params,
                        ..
                    } => generic_type_params.clone(),
                    _ => Vec::new(),
                };

                self.monormophize_stmt(
                    &mut concrete_clone,
                    &gens,
                    &instance.concrete_args,
                    new_name.clone(),
                );

                self.generated_stmts.push(concrete_clone);
            }
        }

        self.generate_unified_tree()
    }
}
