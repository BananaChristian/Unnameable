use std::collections::{HashMap, HashSet};

use crate::{
    hir::{HirExpr, HirExprKind, HirStmt, HirStmtKind, HirType, HirTypeNode},
    lowering::NodeId,
    semantics::{InstanceKey, ResolvedTypeKind, SemanticCtxt, TypeInfo},
};

pub struct Monomorphizer<'a> {
    pub ctxt: &'a mut SemanticCtxt,
    hir: &'a Vec<HirStmt>,
    pub mangled_mappings: HashMap<InstanceKey, String>,
    pub processed_instances: HashSet<InstanceKey>,
    pub generated_stmts: Vec<HirStmt>,
    // Fresh NodeId issue counter for the nodes inside generated instances.
    // Seeded above the highest node id present in the original tree so that
    // instance subtrees never alias the template's (or each other's) ids.
    next_fresh_local: usize,
    // Canonical kind-key -> TypeInfo captured from already-resolved nodes,
    // used to re-type the concrete (substituted) nodes inside instances.
    native_types: HashMap<&'static str, TypeInfo>,
    // User-defined type name (including mangled generic instances) -> TypeInfo.
    named_types: HashMap<String, TypeInfo>,
}

impl<'a> Monomorphizer<'a> {
    pub fn new(ctxt: &'a mut SemanticCtxt, hir: &'a Vec<HirStmt>) -> Self {
        let mut next_fresh_local = 0usize;
        let mut native_types: HashMap<&'static str, TypeInfo> = HashMap::new();
        let mut named_types: HashMap<String, TypeInfo> = HashMap::new();

        for stmt in hir {
            next_fresh_local = next_fresh_local.max(stmt_max_id(stmt));
            if let Some(name) = stmt_name(stmt) {
                if let Some(info) = ctxt.types.types.get(&stmt.hir_id) {
                    named_types.insert(name, info.clone());
                }
            }
            let mut type_nodes = Vec::new();
            collect_type_nodes(stmt, &mut type_nodes);
            for ty in type_nodes {
                if let Some(info) = ctxt.types.types.get(&ty.hir_id) {
                    if let Some(key) = native_type_key(&ty.kind) {
                        native_types.entry(key).or_insert_with(|| info.clone());
                    }
                }
            }
        }

        // Harvest already-specialized user type infos (e.g. `Pair<i32>` from a
        // resolved `GenericType` usage) so generated instance nodes can reuse
        // their engine-computed layouts instead of re-deriving them.
        let harvested: Vec<(String, TypeInfo)> = ctxt
            .types
            .types
            .values()
            .filter(|info| !contains_generic(info))
            .filter_map(|info| specialized_name(info).map(|n| (n, info.clone())))
            .collect();
        for (name, info) in harvested {
            named_types.entry(name).or_insert(info);
        }

        // Seed native scalar type infos from every resolved entry in the full
        // types table. Type nodes nested inside expressions (e.g. a
        // `GenericInstantion`'s type params) are not collected by the tree walk
        // above, but the checker still recorded their concrete infos here by id
        // — kind-keyed lookups need them for e.g. `identity::<f64>`.
        let primitives: Vec<(&'static str, TypeInfo)> = ctxt
            .types
            .types
            .values()
            .filter_map(|info| {
                primitive_key_of_kind(&info.kind).map(|key| (key, info.clone()))
            })
            .collect();
        for (key, info) in primitives {
            native_types.insert(key, info);
        }

        Monomorphizer {
            ctxt,
            hir,
            mangled_mappings: HashMap::new(),
            processed_instances: HashSet::new(),
            generated_stmts: Vec::new(),
            next_fresh_local: next_fresh_local + 1,
            native_types,
            named_types,
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
            let clean_ty_name = sanitize_name(&arg.name);
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
            ResolvedTypeKind::Str => HirType::Str,
            ResolvedTypeKind::Char8 => HirType::Char8,
            ResolvedTypeKind::Char16 => HirType::Char16,
            ResolvedTypeKind::Char32 => HirType::Char32,
            ResolvedTypeKind::Bool => HirType::Bool,
            ResolvedTypeKind::Unit => HirType::Unit,
            ResolvedTypeKind::Unknown => HirType::Unit,
            ResolvedTypeKind::GenericParam(name) => HirType::GenericPlaceHolder(name.clone()),
            ResolvedTypeKind::Ref { inner } => {
                let inner_ty = self.build_type_node(inner, id);
                HirType::Ref(Box::new(inner_ty))
            }
            ResolvedTypeKind::Pointer { inner } => {
                let inner_ty = self.build_type_node(inner, id);
                HirType::Ptr(Box::new(inner_ty))
            }
            ResolvedTypeKind::Array { inner, size } => {
                let inner_ty = self.build_type_node(inner, id);
                HirType::Array(Box::new(inner_ty), *size)
            }
            ResolvedTypeKind::Func {
                params,
                ret_type,
                ..
            } => {
                let ps: Vec<HirTypeNode> = params
                    .iter()
                    .map(|p| self.build_type_node(p, id))
                    .collect();
                let ret = self.build_type_node(ret_type, id);
                HirType::Func(ps, Box::new(ret))
            }
            ResolvedTypeKind::Struct { name, .. }
            | ResolvedTypeKind::Enum { name, .. }
            | ResolvedTypeKind::Variant { name, .. } => HirType::CustomType(name.clone()),
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

        let mut generic_replacement: Option<String> = None;
        match &mut ty_node.kind {
            HirType::Ptr(inner)
            | HirType::Ref(inner)
            | HirType::Nullable(inner) => {
                self.substitute_type(inner, generic_params, concrete_args)
            }
            HirType::Array(inner, _) => {
                self.substitute_type(inner, generic_params, concrete_args)
            }
            HirType::Func(params, ret) => {
                for p in params.iter_mut() {
                    self.substitute_type(p, generic_params, concrete_args);
                }
                self.substitute_type(ret, generic_params, concrete_args);
            }
            HirType::Tuple(fts) => {
                for f in fts.iter_mut() {
                    self.substitute_type(f, generic_params, concrete_args);
                }
            }
            HirType::Failable(ok, err) => {
                self.substitute_type(ok, generic_params, concrete_args);
                self.substitute_type(err, generic_params, concrete_args);
            }
            HirType::GenericType { type_params, .. } => {
                for tp in type_params.iter_mut() {
                    self.substitute_type(tp, generic_params, concrete_args);
                }
                // If every argument is now concrete, resolve the user type so
                // downstream HIR/MIR sees a mangled CustomType reference.
                let evaluated_args: Option<Vec<TypeInfo>> =
                    type_params.iter().map(|tp| self.type_info_for(tp)).collect();
                if let Some(args) = evaluated_args {
                    if let Some(original_def_id) = self.ctxt.names.resolved.get(&ty_node.hir_id).cloned()
                    {
                        let search_key = InstanceKey {
                            original_def_id,
                            concrete_args: args,
                        };
                        if let Some(mangled_name) = self.mangled_mappings.get(&search_key) {
                            generic_replacement = Some(mangled_name.clone());
                        } else {
                            generic_replacement = self.ensure_instance(&search_key);
                        }
                    }
                }
            }
            _ => {}
        }

        if let Some(mangled_name) = generic_replacement {
            ty_node.kind = HirType::CustomType(mangled_name);
        }
    }

    /// Produce a TypeInfo for a (now-concrete) type node, reusing engine-built
    /// layouts where available. Returns None for nodes that are still abstract.
    pub fn type_info_for(&mut self, node: &HirTypeNode) -> Option<TypeInfo> {
        let span = node.span.clone();
        let info = match &node.kind {
            HirType::I8 => self.native_types.get("i8")?.clone(),
            HirType::I16 => self.native_types.get("i16")?.clone(),
            HirType::I32 => self.native_types.get("i32")?.clone(),
            HirType::I64 => self.native_types.get("i64")?.clone(),
            HirType::I128 => self.native_types.get("i128")?.clone(),
            HirType::U8 => self.native_types.get("u8")?.clone(),
            HirType::U16 => self.native_types.get("u16")?.clone(),
            HirType::U32 => self.native_types.get("u32")?.clone(),
            HirType::U64 => self.native_types.get("u64")?.clone(),
            HirType::U128 => self.native_types.get("u128")?.clone(),
            HirType::ISize => self.native_types.get("isize")?.clone(),
            HirType::USize => self.native_types.get("usize")?.clone(),
            HirType::F32 => self.native_types.get("f32")?.clone(),
            HirType::F64 => self.native_types.get("f64")?.clone(),
            HirType::Str => self.native_types.get("str")?.clone(),
            HirType::Char8 => self.native_types.get("char8")?.clone(),
            HirType::Char16 => self.native_types.get("char16")?.clone(),
            HirType::Char32 => self.native_types.get("char32")?.clone(),
            HirType::Bool => self.native_types.get("bool")?.clone(),
            HirType::Unit => self.native_types.get("unit")?.clone(),
            HirType::Ptr(inner) => {
                let inner_info = self.type_info_for(inner)?;
                self.wrapped_info("ptr", ResolvedTypeKind::Pointer { inner: Box::new(inner_info) }, span.clone())
            }
            HirType::Ref(inner) => {
                let inner_info = self.type_info_for(inner)?;
                self.wrapped_info("ref", ResolvedTypeKind::Ref { inner: Box::new(inner_info) }, span.clone())
            }
            HirType::Nullable(inner) => {
                let inner_info = self.type_info_for(inner)?;
                self.wrapped_info("nullable", ResolvedTypeKind::Nullable { ty: Box::new(inner_info) }, span.clone())
            }
            HirType::Array(inner, size) => {
                let inner_info = self.type_info_for(inner)?;
                let mult = size.unwrap_or(1) as usize;
                let inner_size = inner_info.layout.size * mult;
                let inner_align = inner_info.layout.alignment;
                let kind = ResolvedTypeKind::Array { inner: Box::new(inner_info), size: *size };
                let mut info = self.native_types.get("array")?.clone();
                info.layout.size = inner_size;
                info.layout.alignment = inner_align.max(info.layout.alignment);
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            HirType::Func(params, ret) => {
                let ps: Vec<TypeInfo> =
                    params.iter().map(|p| self.type_info_for(p)).collect::<Option<_>>()?;
                let ret_info = self.type_info_for(ret)?;
                let kind = ResolvedTypeKind::Func {
                    params: ps,
                    gen_type_params: Vec::new(),
                    ret_type: Box::new(ret_info),
                    param_defaults: Vec::new(),
                };
                let mut info = self.native_types.get("func").or(self.native_types.get("ptr"))?.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            HirType::Tuple(fields) => {
                let fts = fields.iter().map(|f| self.type_info_for(f)).collect::<Option<Vec<_>>>()?;
                let kind = ResolvedTypeKind::Tuple { fields: fts };
                let mut info = self.native_types.get("tuple").or(self.native_types.get("ptr"))?.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            HirType::Failable(ok, err) => {
                let ok_i = self.type_info_for(ok)?;
                let err_i = self.type_info_for(err)?;
                let kind = ResolvedTypeKind::Failable { ok: Box::new(ok_i), err: Box::new(err_i) };
                let mut info = self.native_types.get("failable").or(self.native_types.get("ptr"))?.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            HirType::CustomType(name) => self.named_types.get(name)?.clone(),
            HirType::GenericPlaceHolder(_) | HirType::GenericType { .. } => return None,
        };
        let mut info = info;
        info.span = span;
        Some(info)
    }

    fn wrapped_info(&mut self, key: &'static str, kind: ResolvedTypeKind, span: crate::diagnostics::Span) -> TypeInfo {
        let mut info = self
            .native_types
            .get(key)
            .cloned()
            .unwrap_or_else(|| self.native_types.get("ptr").cloned().unwrap_or_else(|| TypeInfo {
                kind: ResolvedTypeKind::Unknown,
                name: "unknown".to_string(),
                type_id: crate::semantics::TypeId(0),
                layout: crate::layout::Layout::empty(),
                span: span.clone(),
            }));
        info.kind = kind.clone();
        info.name = TypeInfo::name(kind);
        info.span = span.clone();
        info
    }

    fn new_id(&mut self) -> NodeId {
        let id = NodeId {
            local: self.next_fresh_local,
            external: 0,
        };
        self.next_fresh_local += 1;
        id
    }

    fn generic_param_info(&self, node: &HirTypeNode) -> TypeInfo {
        let name = match &node.kind {
            HirType::GenericPlaceHolder(n) => n.clone(),
            _ => String::new(),
        };
        TypeInfo {
            kind: ResolvedTypeKind::GenericParam(name.clone()),
            name,
            type_id: crate::semantics::TypeId(0),
            layout: crate::layout::Layout::empty(),
            span: node.span.clone(),
        }
    }

    /// Re-record the type of a node that just received a fresh id, by taking
    /// the checker's entry under its original id and substituting this
    /// instance's concrete args. MIR reads statement/expression/parameter
    /// types by id (e.g. a var decl's type), so a fresh id with no entry
    /// would ICE in `get_type`.
    fn record_fresh_type(
        &mut self,
        original: &NodeId,
        fresh: &NodeId,
        gens: &[HirTypeNode],
        args: &[TypeInfo],
    ) {
        let Some(info) = self.ctxt.types.types.get(original).cloned() else {
            return;
        };
        let gen_infos: Vec<TypeInfo> = gens.iter().map(|g| self.generic_param_info(g)).collect();
        let substituted = if gen_infos.is_empty() && args.is_empty() {
            info
        } else {
            self.substitute_type_info(&info, &gen_infos, args)
        };
        self.ctxt.types.types.insert(fresh.clone(), substituted);
    }

    fn fresh_type_node(&mut self, ty: &mut HirTypeNode) {
        ty.hir_id = self.new_id();
        if let Some(info) = self.type_info_for(ty) {
            self.ctxt
                .types
                .types
                .insert(ty.hir_id.clone(), info);
        }
        match &mut ty.kind {
            HirType::Ptr(inner) | HirType::Ref(inner) | HirType::Nullable(inner) => {
                self.fresh_type_node(inner);
            }
            HirType::Array(inner, _) => self.fresh_type_node(inner),
            HirType::Func(params, ret) => {
                for p in params {
                    self.fresh_type_node(p);
                }
                self.fresh_type_node(ret);
            }
            HirType::Tuple(fts) => {
                for t in fts {
                    self.fresh_type_node(t);
                }
            }
            HirType::Failable(ok, err) => {
                self.fresh_type_node(ok);
                self.fresh_type_node(err);
            }
            HirType::GenericType { type_params, .. } => {
                for t in type_params {
                    self.fresh_type_node(t);
                }
            }
            _ => {}
        }
    }

    fn fresh_param(&mut self, param: &mut crate::hir::HirParam, gens: &[HirTypeNode], args: &[TypeInfo]) {
        let original_id = param.hir_id.clone();
        param.hir_id = self.new_id();
        self.record_fresh_type(&original_id, &param.hir_id, gens, args);
        self.fresh_type_node(&mut param.ty);
        if let Some(default) = &mut param.default {
            self.fresh_expr(default, gens, args);
        }
    }

    fn fresh_expr(&mut self, expr: &mut HirExpr, gens: &[HirTypeNode], args: &[TypeInfo]) {
        let original_id = expr.hir_id.clone();
        expr.hir_id = self.new_id();
        self.record_fresh_type(&original_id, &expr.hir_id, gens, args);
        match &mut expr.kind {
            HirExprKind::Literal(_) | HirExprKind::Identifier(_) => {}
            HirExprKind::Binary(left, _, right) => {
                self.fresh_expr(left, gens, args);
                self.fresh_expr(right, gens, args);
            }
            HirExprKind::Unary(_, operand)
            | HirExprKind::Unwrap(operand)
            | HirExprKind::Postfix(operand, _) => {
                self.fresh_expr(operand, gens, args);
            }
            HirExprKind::Call(callee, args2) => {
                self.fresh_expr(callee, gens, args);
                for a in args2 {
                    self.fresh_expr(a, gens, args);
                }
            }
            HirExprKind::Index { target, index } => {
                self.fresh_expr(target, gens, args);
                self.fresh_expr(index, gens, args);
            }
            HirExprKind::TupleInst { body } => {
                for e in body {
                    self.fresh_expr(e, gens, args);
                }
            }
            HirExprKind::GenericInstantion { type_params, .. } => {
                for tp in type_params {
                    self.fresh_type_node(tp);
                }
            }
            HirExprKind::StaticCast(ty, operand) | HirExprKind::BitCast(ty, operand) => {
                self.fresh_type_node(ty);
                self.fresh_expr(operand, gens, args);
            }
            HirExprKind::SizeOf(ty) => {
                self.fresh_type_node(ty);
            }
            HirExprKind::Instantiation { init_ty, body } => {
                self.fresh_type_node(init_ty);
                for field in body {
                    let field_orig = field.hir_id.clone();
                    field.hir_id = self.new_id();
                    self.record_fresh_type(&field_orig, &field.hir_id, gens, args);
                    self.fresh_expr(&mut field.value, gens, args);
                }
            }
            HirExprKind::DollarScope {
                params,
                body,
                result,
            } => {
                for p in params {
                    self.fresh_expr(p, gens, args);
                }
                for st in body {
                    self.fresh_stmt(st, gens, args);
                }
                if let Some(res) = result {
                    self.fresh_expr(res, gens, args);
                }
            }
        }
    }

    fn fresh_stmt(&mut self, stmt: &mut HirStmt, gens: &[HirTypeNode], args: &[TypeInfo]) {
        let original_id = stmt.hir_id.clone();
        stmt.hir_id = self.new_id();
        self.record_fresh_type(&original_id, &stmt.hir_id, gens, args);
        match &mut stmt.kind {
            HirStmtKind::HirReturn(Some(expr)) | HirStmtKind::HirExpr(expr) => {
                self.fresh_expr(expr, gens, args);
            }
            HirStmtKind::HirReturn(None)
            | HirStmtKind::HirBreak
            | HirStmtKind::HirContinue
            | HirStmtKind::HirImport { .. } => {}
            HirStmtKind::HirVarDecl { ty, init, .. } => {
                if let Some(t) = ty {
                    self.fresh_type_node(t);
                }
                self.fresh_expr(init, gens, args);
            }
            HirStmtKind::HirFunctionDef {
                params,
                return_type,
                generic_type_params,
                body,
                ..
            } => {
                for p in params {
                    self.fresh_param(p, gens, args);
                }
                self.fresh_type_node(return_type);
                for t in generic_type_params {
                    self.fresh_type_node(t);
                }
                for s in body {
                    self.fresh_stmt(s, gens, args);
                }
            }
            HirStmtKind::HirFunctionDecl {
                params,
                return_type,
                generic_type_params,
                ..
            } => {
                for p in params {
                    self.fresh_param(p, gens, args);
                }
                self.fresh_type_node(return_type);
                for t in generic_type_params {
                    self.fresh_type_node(t);
                }
            }
            HirStmtKind::HirStructDecl {
                contracts,
                generic_type_params,
                fields,
                ..
            } => {
                for c in contracts {
                    self.fresh_type_node(c);
                }
                for t in generic_type_params {
                    self.fresh_type_node(t);
                }
                for f in fields {
                    self.fresh_param(f, gens, args);
                }
            }
            HirStmtKind::HirEnumDecl { underlying, .. } => {
                self.fresh_type_node(underlying);
            }
            HirStmtKind::HirVariantDecl {
                contracts,
                generic_type_params,
                members,
                ..
            } => {
                for c in contracts {
                    self.fresh_type_node(c);
                }
                for t in generic_type_params {
                    self.fresh_type_node(t);
                }
                for m in members {
                    for mt in &mut m.member_types {
                        self.fresh_type_node(mt);
                    }
                }
            }
            HirStmtKind::HirIf {
                condition,
                body,
                else_body,
            } => {
                self.fresh_expr(condition, gens, args);
                for s in body {
                    self.fresh_stmt(s, gens, args);
                }
                if let Some(el) = else_body {
                    for s in el {
                        self.fresh_stmt(s, gens, args);
                    }
                }
            }
            HirStmtKind::HirWhile { condition, body } => {
                self.fresh_expr(condition, gens, args);
                for s in body {
                    self.fresh_stmt(s, gens, args);
                }
            }
            HirStmtKind::HirContractDecl {
                functions, generic_type_params, ..
            } => {
                for t in generic_type_params {
                    self.fresh_type_node(t);
                }
                for f in functions {
                    self.fresh_stmt(f, gens, args);
                }
            }
            HirStmtKind::HirAlias { original, .. } => {
                self.fresh_type_node(original);
            }
        }
    }

    /// Materialise (or fetch the already-known name of) the instance for
    /// `key`. Creates new instances lazily, so a generic call discovered while
    /// monomorphizing another instance works even when it was not visible to
    /// the type checker's backlog.
    pub fn ensure_instance(&mut self, key: &InstanceKey) -> Option<String> {
        if let Some(name) = self.mangled_mappings.get(key) {
            return Some(name.clone());
        }

        if self.processed_instances.contains(key) {
            return None;
        }

        let base_name = self.extract_stmt_name(&key.original_def_id);
        let mangled = self.mangle_name(&base_name, &key.concrete_args);

        // Claim the name and mark as processing *before* substituting the body
        // so self/nested references resolve and cycles terminate.
        self.mangled_mappings.insert(key.clone(), mangled.clone());
        self.processed_instances.insert(key.clone());

        let template = self.get_decl(&key.original_def_id)?.clone();
        let gens = generic_type_params_of(&template);
        let is_struct = matches!(template.kind, HirStmtKind::HirStructDecl { .. });

        if is_struct {
            if let Some(info) = self.specialize_struct_info(&template, key) {
                self.named_types.insert(mangled.clone(), info);
            }
        }

        let mut concrete_clone = template;
        self.monormophize_stmt(
            &mut concrete_clone,
            &gens,
            &key.concrete_args,
            Some(mangled.clone()),
        );

        self.fresh_stmt(&mut concrete_clone, &gens, &key.concrete_args);

        self.generated_stmts.push(concrete_clone);
        Some(mangled)
    }

    /// Build a concrete Struct TypeInfo for an instance by substituting the
    /// template's own generic params inside its member types.
    fn specialize_struct_info(&mut self, template: &HirStmt, key: &InstanceKey) -> Option<TypeInfo> {
        if let HirStmtKind::HirStructDecl { name, .. } = &template.kind {
            let template_info = self.ctxt.types.types.get(&template.hir_id)?.clone();
            match &template_info.kind {
                ResolvedTypeKind::Struct {
                    gen_type_params,
                    members,
                    ..
                } => {
                    let substituted_members = members
                        .iter()
                        .map(|(field_name, field_ty, id)| {
                            (
                                field_name.clone(),
                                self.substitute_type_info(field_ty, gen_type_params, &key.concrete_args),
                                *id,
                            )
                        })
                        .collect::<Vec<_>>();

                    let kind = ResolvedTypeKind::Struct {
                        name: name.clone(),
                        gen_type_params: Vec::new(),
                        members: substituted_members,
                    };

                    let mut info = template_info;
                    info.kind = kind.clone();
                    info.name = TypeInfo::name(kind);
                    Some(info)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Mirror of the checker's TypeInfo substitution, for TypeInfo rather than
    /// HIR nodes. Used to specialize struct members and call-signatures.
    pub fn substitute_type_info(
        &mut self,
        current: &TypeInfo,
        template_gen_params: &[TypeInfo],
        concrete_args: &[TypeInfo],
    ) -> TypeInfo {
        match &current.kind {
            ResolvedTypeKind::GenericParam(name) => {
                let position = template_gen_params
                    .iter()
                    .position(|param| &param.name == name);
                if let Some(idx) = position {
                    if let Some(concrete) = concrete_args.get(idx) {
                        let mut concrete_clone = concrete.clone();
                        concrete_clone.span = current.span.clone();
                        return concrete_clone;
                    }
                }
                current.clone()
            }
            ResolvedTypeKind::Struct {
                name,
                gen_type_params,
                members,
            } => {
                let substituted_members = members
                    .iter()
                    .map(|(field_name, field_ty, id)| {
                        let sub = self.substitute_type_info(field_ty, template_gen_params, concrete_args);
                        (field_name.clone(), sub, *id)
                    })
                    .collect::<Vec<_>>();
                let kind = ResolvedTypeKind::Struct {
                    name: name.clone(),
                    gen_type_params: gen_type_params.clone(),
                    members: substituted_members,
                };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            ResolvedTypeKind::Variant {
                name,
                gen_type_params,
                arms,
            } => {
                let substituted_arms = arms
                    .iter()
                    .map(|(arm_name, arm_ty, id, payload_tys)| {
                        let sub = (
                            arm_name.clone(),
                            self.substitute_type_info(arm_ty, template_gen_params, concrete_args),
                            *id,
                            payload_tys
                                .iter()
                                .map(|t| self.substitute_type_info(t, template_gen_params, concrete_args))
                                .collect::<Vec<_>>(),
                        );
                        sub
                    })
                    .collect::<Vec<_>>();
                let kind = ResolvedTypeKind::Variant {
                    name: name.clone(),
                    gen_type_params: gen_type_params.clone(),
                    arms: substituted_arms,
                };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            ResolvedTypeKind::Pointer { inner } => {
                let inner = self.substitute_type_info(inner, template_gen_params, concrete_args);
                let kind = ResolvedTypeKind::Pointer { inner: Box::new(inner) };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            ResolvedTypeKind::Ref { inner } => {
                let inner = self.substitute_type_info(inner, template_gen_params, concrete_args);
                let kind = ResolvedTypeKind::Ref { inner: Box::new(inner) };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            ResolvedTypeKind::Array { inner, size } => {
                let inner = self.substitute_type_info(inner, template_gen_params, concrete_args);
                let kind = ResolvedTypeKind::Array {
                    inner: Box::new(inner),
                    size: *size,
                };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                if let Some(n) = size {
                    info.layout.size *= *n as usize;
                }
                info.span = current.span.clone();
                info
            }
            ResolvedTypeKind::Func {
                params,
                ret_type,
                param_defaults,
                ..
            } => {
                let ps = params
                    .iter()
                    .map(|p| self.substitute_type_info(p, template_gen_params, concrete_args))
                    .collect();
                let ret = self.substitute_type_info(ret_type, template_gen_params, concrete_args);
                let kind = ResolvedTypeKind::Func {
                    params: ps,
                    gen_type_params: Vec::new(),
                    ret_type: Box::new(ret),
                    param_defaults: param_defaults.clone(),
                };
                let mut info = current.clone();
                info.kind = kind.clone();
                info.name = TypeInfo::name(kind);
                info
            }
            _ => current.clone(),
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
                        Some(current_name.to_string()),
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
                        Some(current_name.to_string()),
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
        for key in &backlog_items {
            self.ensure_instance(key);
        }

        self.reconcile_struct_names();
        self.generate_unified_tree()
    }

    /// Root functions keep their original node ids, so their type-table
    /// entries were written by the type checker *before* monomorphization
    /// and still carry the template's source name (e.g. `Pair`, not
    /// `_U_Pair_i32`). MIR looks structs up by name, so every recorded
    /// Struct/Variant value whose members exactly match a generated concrete
    /// instance is re-pointed at the mangled instance name. Member-only
    /// matching is what distinguishes `Pair<i32>` from `Pair<f64>`; the
    /// template's own decl retains its `T` members and is never matched.
    fn reconcile_struct_names(&mut self) {
        if self.named_types.is_empty() {
            return;
        }

        // `named_types` — populated by the harvest and by
        // `specialize_struct_info` — is keyed by mangled name, but its values
        // may still contain generic params (e.g. a template `Pair<T>` being
        // harvested as `_U_Pair_T`). Exclude those so the template's own
        // declaration can never match a generic instance. The fingerprint
        // ignores spans/type_ids (which differ between the checker's entries
        // and the substituted instances).
        let mut mangled_by_members: Vec<(Vec<(String, String, usize, usize)>, String)> = Vec::new();
        for (mangled, info) in &self.named_types {
            if contains_generic(info) {
                continue;
            }
            let members = match &info.kind {
                ResolvedTypeKind::Struct { members, .. } => members
                    .iter()
                    .map(|(n, t, _)| {
                        (n.clone(), format!("{:?}", std::mem::discriminant(&t.kind)), t.layout.size, t.layout.alignment)
                    })
                    .collect::<Vec<_>>(),
                ResolvedTypeKind::Variant { arms, .. } => arms
                    .iter()
                    .map(|(n, t, _, _)| {
                        (n.clone(), format!("{:?}", std::mem::discriminant(&t.kind)), t.layout.size, t.layout.alignment)
                    })
                    .collect::<Vec<_>>(),
                _ => continue,
            };
            mangled_by_members.push((members, mangled.clone()));
        }

        for info in self.ctxt.types.types.values_mut() {
            if contains_generic(info) {
                continue;
            }
            let matched_name = match &info.kind {
                ResolvedTypeKind::Struct { name, members, .. } => {
                    if name.starts_with('_') {
                        None
                    } else {
                        let fingerprint: Vec<(String, String, usize, usize)> = members
                            .iter()
                            .map(|(n, t, _)| {
                                (n.clone(), format!("{:?}", std::mem::discriminant(&t.kind)), t.layout.size, t.layout.alignment)
                            })
                            .collect();
                        mangled_by_members
                            .iter()
                            .find(|(fp, _)| *fp == fingerprint)
                            .map(|(_, mangled)| mangled.clone())
                    }
                }
                ResolvedTypeKind::Variant { name, arms, .. } => {
                    if name.starts_with('_') {
                        None
                    } else {
                        let fingerprint: Vec<(String, String, usize, usize)> = arms
                            .iter()
                            .map(|(n, t, _, _)| {
                                (n.clone(), format!("{:?}", std::mem::discriminant(&t.kind)), t.layout.size, t.layout.alignment)
                            })
                            .collect();
                        mangled_by_members
                            .iter()
                            .find(|(fp, _)| *fp == fingerprint)
                            .map(|(_, mangled)| mangled.clone())
                    }
                }
                _ => None,
            };
            if let Some(mangled) = matched_name {
                match &mut info.kind {
                    ResolvedTypeKind::Struct { name, .. } => *name = mangled.clone(),
                    ResolvedTypeKind::Variant { name, .. } => *name = mangled,
                    _ => {}
                }
            }
        }
    }
}

fn generic_type_params_of(stmt: &HirStmt) -> Vec<HirTypeNode> {
    match &stmt.kind {
        HirStmtKind::HirFunctionDecl { generic_type_params, .. }
        | HirStmtKind::HirFunctionDef { generic_type_params, .. }
        | HirStmtKind::HirStructDecl { generic_type_params, .. }
        | HirStmtKind::HirVariantDecl { generic_type_params, .. }
        | HirStmtKind::HirContractDecl { generic_type_params, .. } => generic_type_params.clone(),
        _ => Vec::new(),
    }
}

fn stmt_name(stmt: &HirStmt) -> Option<String> {
    match &stmt.kind {
        HirStmtKind::HirFunctionDef { name, .. }
        | HirStmtKind::HirFunctionDecl { name, .. }
        | HirStmtKind::HirStructDecl { name, .. }
        | HirStmtKind::HirEnumDecl { name, .. }
        | HirStmtKind::HirVariantDecl { name, .. } => Some(name.clone()),
        _ => None,
    }
}

fn sanitize_name(name: &str) -> String {
    name.replace('(', "_")
        .replace(')', "_")
        .replace(':', "_")
        .replace('<', "_")
        .replace('>', "_")
        .replace(' ', "")
}

/// For an already-specialised user type (a `Struct` with params, e.g. a
/// `Pair<i32>` resolved from a `GenericType` usage), compute the name its
/// generated instance would be mangled to (e.g. `_U_Pair_i32`).
/// True if `info` still references a generic parameter anywhere (a
/// `GenericParam`, unresolved `GenericType`, or `GenericPlaceHolder`). Used
/// to keep generic templates out of the concrete-instance tables.
fn contains_generic(info: &TypeInfo) -> bool {
    match &info.kind {
        ResolvedTypeKind::GenericParam(_) => true,
        ResolvedTypeKind::Pointer { inner }
        | ResolvedTypeKind::Ref { inner }
        | ResolvedTypeKind::Nullable { ty: inner } => contains_generic(inner),
        ResolvedTypeKind::Failable { ok: inner, err: other } => {
            contains_generic(inner) || contains_generic(other)
        }
        ResolvedTypeKind::Enum { underlying, members, .. } => {
            contains_generic(underlying)
                || members.iter().any(|(_, t, _)| contains_generic(t))
        }
        ResolvedTypeKind::Array { inner, .. } => contains_generic(inner),
        ResolvedTypeKind::Func {
            params,
            ret_type,
            gen_type_params,
            ..
        } => {
            contains_generic(ret_type)
                || params.iter().any(contains_generic)
                || gen_type_params.iter().any(contains_generic)
        }
        ResolvedTypeKind::Tuple { fields } => fields.iter().any(contains_generic),
        ResolvedTypeKind::Struct { members, .. } => members.iter().any(|(_, t, _)| contains_generic(t)),
        ResolvedTypeKind::Variant { arms, .. } => arms
            .iter()
            .any(|(_, t, _, payloads)| contains_generic(t) || payloads.iter().any(contains_generic)),
        _ => false,
    }
}

fn specialized_name(info: &TypeInfo) -> Option<String> {
    match &info.kind {
        ResolvedTypeKind::Struct {
            name,
            gen_type_params,
            ..
        }
        | ResolvedTypeKind::Variant {
            name,
            gen_type_params,
            ..
        } if !gen_type_params.is_empty() => {
            let args: Vec<String> = gen_type_params.iter().map(|p| sanitize_name(&p.name)).collect();
            Some(format!("_U_{}_{}", name, args.join("_")))
        }
        _ => None,
    }
}

fn primitive_key_of_kind(kind: &ResolvedTypeKind) -> Option<&'static str> {
    match kind {
        ResolvedTypeKind::I8 => Some("i8"),
        ResolvedTypeKind::U8 => Some("u8"),
        ResolvedTypeKind::I16 => Some("i16"),
        ResolvedTypeKind::U16 => Some("u16"),
        ResolvedTypeKind::I32 => Some("i32"),
        ResolvedTypeKind::U32 => Some("u32"),
        ResolvedTypeKind::I64 => Some("i64"),
        ResolvedTypeKind::U64 => Some("u64"),
        ResolvedTypeKind::I128 => Some("i128"),
        ResolvedTypeKind::U128 => Some("u128"),
        ResolvedTypeKind::USize => Some("usize"),
        ResolvedTypeKind::ISize => Some("isize"),
        ResolvedTypeKind::F32 => Some("f32"),
        ResolvedTypeKind::F64 => Some("f64"),
        ResolvedTypeKind::Str => Some("str"),
        ResolvedTypeKind::Char8 => Some("char8"),
        ResolvedTypeKind::Char16 => Some("char16"),
        ResolvedTypeKind::Char32 => Some("char32"),
        ResolvedTypeKind::Bool => Some("bool"),
        ResolvedTypeKind::Unit => Some("unit"),
        _ => None,
    }
}

fn native_type_key(ty: &HirType) -> Option<&'static str> {
    match ty {
        HirType::I8 => Some("i8"),
        HirType::I16 => Some("i16"),
        HirType::I32 => Some("i32"),
        HirType::I64 => Some("i64"),
        HirType::I128 => Some("i128"),
        HirType::U8 => Some("u8"),
        HirType::U16 => Some("u16"),
        HirType::U32 => Some("u32"),
        HirType::U64 => Some("u64"),
        HirType::U128 => Some("u128"),
        HirType::ISize => Some("isize"),
        HirType::USize => Some("usize"),
        HirType::F32 => Some("f32"),
        HirType::F64 => Some("f64"),
        HirType::Str => Some("str"),
        HirType::Char8 => Some("char8"),
        HirType::Char16 => Some("char16"),
        HirType::Char32 => Some("char32"),
        HirType::Bool => Some("bool"),
        HirType::Unit => Some("unit"),
        HirType::Ptr(_) => Some("ptr"),
        HirType::Ref(_) => Some("ref"),
        HirType::Array(..) => Some("array"),
        HirType::Func(..) => Some("func"),
        HirType::Tuple(..) => Some("tuple"),
        HirType::Failable(..) => Some("failable"),
        HirType::Nullable(_) => Some("nullable"),
        _ => None,
    }
}

fn collect_type_nodes<'k>(stmt: &'k HirStmt, out: &mut Vec<&'k HirTypeNode>) {
    match &stmt.kind {
        HirStmtKind::HirVarDecl { ty, .. } => {
            if let Some(t) = ty {
                collect_type_refs(t, out);
            }
        }
        HirStmtKind::HirFunctionDef {
            params,
            return_type,
            generic_type_params,
            ..
        }
        | HirStmtKind::HirFunctionDecl {
            params,
            return_type,
            generic_type_params,
            ..
        } => {
            for p in params {
                collect_type_refs(&p.ty, out);
            }
            collect_type_refs(return_type, out);
            for t in generic_type_params {
                collect_type_refs(t, out);
            }
            if let HirStmtKind::HirFunctionDef { body, .. } = &stmt.kind {
                for s in body {
                    collect_type_nodes(s, out);
                }
            }
        }
        HirStmtKind::HirStructDecl {
            contracts,
            generic_type_params,
            fields,
            ..
        } => {
            for c in contracts {
                collect_type_refs(c, out);
            }
            for t in generic_type_params {
                collect_type_refs(t, out);
            }
            for f in fields {
                collect_type_refs(&f.ty, out);
            }
        }
        HirStmtKind::HirEnumDecl { underlying, .. } => collect_type_refs(underlying, out),
        HirStmtKind::HirVariantDecl {
            contracts,
            generic_type_params,
            members,
            ..
        } => {
            for c in contracts {
                collect_type_refs(c, out);
            }
            for t in generic_type_params {
                collect_type_refs(t, out);
            }
            for m in members {
                for mt in &m.member_types {
                    collect_type_refs(mt, out);
                }
            }
        }
        HirStmtKind::HirIf { condition, body, else_body } => {
            collect_expr_type_refs(condition, out);
            for s in body {
                collect_type_nodes(s, out);
            }
            if let Some(el) = else_body {
                for s in el {
                    collect_type_nodes(s, out);
                }
            }
        }
        HirStmtKind::HirWhile { condition, body } => {
            collect_expr_type_refs(condition, out);
            for s in body {
                collect_type_nodes(s, out);
            }
        }
        HirStmtKind::HirReturn(Some(e)) | HirStmtKind::HirExpr(e) => collect_expr_type_refs(e, out),
        HirStmtKind::HirAlias { original, .. } => collect_type_refs(original, out),
        HirStmtKind::HirContractDecl { functions, generic_type_params, .. } => {
            for t in generic_type_params {
                collect_type_refs(t, out);
            }
            for f in functions {
                collect_type_nodes(f, out);
            }
        }
        _ => {}
    }
}

fn collect_expr_type_refs<'k>(expr: &'k HirExpr, out: &mut Vec<&'k HirTypeNode>) {
    match &expr.kind {
        HirExprKind::GenericInstantion { type_params, .. } => {
            for t in type_params {
                collect_type_refs(t, out);
            }
        }
        HirExprKind::StaticCast(ty, e) | HirExprKind::BitCast(ty, e) => {
            collect_type_refs(ty, out);
            collect_expr_type_refs(e, out);
        }
        HirExprKind::SizeOf(ty) => collect_type_refs(ty, out),
        HirExprKind::Instantiation { init_ty, body } => {
            collect_type_refs(init_ty, out);
            for f in body {
                collect_expr_type_refs(&f.value, out);
            }
        }
        HirExprKind::Binary(l, _, r) => {
            collect_expr_type_refs(l, out);
            collect_expr_type_refs(r, out);
        }
        HirExprKind::Unary(_, e)
        | HirExprKind::Unwrap(e)
        | HirExprKind::Postfix(e, _)
        | HirExprKind::Index { target: e, .. } => collect_expr_type_refs(e, out),
        HirExprKind::Call(callee, args) => {
            collect_expr_type_refs(callee, out);
            for a in args {
                collect_expr_type_refs(a, out);
            }
        }
        HirExprKind::TupleInst { body } => {
            for e in body {
                collect_expr_type_refs(e, out);
            }
        }
        HirExprKind::DollarScope { params, body, result } => {
            for p in params {
                collect_expr_type_refs(p, out);
            }
            for s in body {
                collect_type_nodes(s, out);
            }
            if let Some(r) = result {
                collect_expr_type_refs(r, out);
            }
        }
        _ => {}
    }
}

fn collect_type_refs<'k>(ty: &'k HirTypeNode, out: &mut Vec<&'k HirTypeNode>) {
    out.push(ty);
    match &ty.kind {
        HirType::Ptr(i) | HirType::Ref(i) | HirType::Nullable(i) => collect_type_refs(i, out),
        HirType::Array(i, _) => collect_type_refs(i, out),
        HirType::Func(params, ret) => {
            for p in params {
                collect_type_refs(p, out);
            }
            collect_type_refs(ret, out);
        }
        HirType::Tuple(fts) => {
            for t in fts {
                collect_type_refs(t, out);
            }
        }
        HirType::Failable(ok, err) => {
            collect_type_refs(ok, out);
            collect_type_refs(err, out);
        }
        HirType::GenericType { type_params, .. } => {
            for t in type_params {
                collect_type_refs(t, out);
            }
        }
        _ => {}
    }
}

fn stmt_max_id(stmt: &HirStmt) -> usize {
    let mut max = stmt.hir_id.local;
    let mut tys = Vec::new();
    collect_type_nodes(stmt, &mut tys);
    for t in tys {
        max = max.max(t.hir_id.local);
    }
    match &stmt.kind {
        HirStmtKind::HirFunctionDef { params, body, .. } => {
            for p in params {
                if let Some(d) = &p.default {
                    max = max.max(expr_max_id(d));
                }
            }
            for s in body {
                max = max.max(stmt_max_id(s));
            }
        }
        _ => {}
    }
    max
}

fn expr_max_id(expr: &HirExpr) -> usize {
    let mut max = expr.hir_id.local;
    match &expr.kind {
        HirExprKind::Binary(l, _, r) => max = max.max(expr_max_id(l)).max(expr_max_id(r)),
        HirExprKind::Unary(_, e) | HirExprKind::Unwrap(e) | HirExprKind::Postfix(e, _) => {
            max = max.max(expr_max_id(e));
        }
        HirExprKind::Call(c, args) => {
            max = max.max(expr_max_id(c));
            for a in args {
                max = max.max(expr_max_id(a));
            }
        }
        HirExprKind::Index { target, index } => max = max.max(expr_max_id(target)).max(expr_max_id(index)),
        HirExprKind::TupleInst { body } => {
            for e in body {
                max = max.max(expr_max_id(e));
            }
        }
        HirExprKind::GenericInstantion { type_params, .. } => {
            for t in type_params {
                max = max.max(t.hir_id.local);
            }
        }
        HirExprKind::StaticCast(t, e) | HirExprKind::BitCast(t, e) => {
            max = max.max(t.hir_id.local).max(expr_max_id(e))
        }
        HirExprKind::SizeOf(t) => max = max.max(t.hir_id.local),
        HirExprKind::Instantiation { init_ty, body } => {
            max = max.max(init_ty.hir_id.local);
            for f in body {
                max = max.max(expr_max_id(&f.value));
            }
        }
        HirExprKind::DollarScope { params, body, result } => {
            for p in params {
                max = max.max(expr_max_id(p));
            }
            for s in body {
                max = max.max(stmt_max_id(s));
            }
            if let Some(r) = result {
                max = max.max(expr_max_id(r));
            }
        }
        _ => {}
    }
    max
}
