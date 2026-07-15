use crate::{
    diagnostics::{CompilerError, Diagnostics, Phase, Span},
    hir::{
        HirAnonStructField, HirEnumMember, HirParam, HirStmt, HirStmtKind, HirType, HirTypeNode,
        HirVariantMember,
    },
    layout::LayoutEngine,
    lowering::NodeId,
    semantics::{
        semantics::{InstanceKey, ResolvedTypeKind, SemanticCtxt, TypeInfo},
        type_checker::registry::TypeRegistry,
    },
    target::TargetSpec,
};

pub struct TypeChecker<'a> {
    hir: &'a Vec<HirStmt>,
    pub ctxt: &'a mut SemanticCtxt,
    pub registry: TypeRegistry,
    pub layout_engine: LayoutEngine<'a>,
    pub active_generic_params: Vec<String>,
    diagnostics: &'a mut Diagnostics,
    pub corrupted: bool,
}

impl<'a> TypeChecker<'a> {
    pub fn new(
        diagnostics: &'a mut Diagnostics,
        hir: &'a Vec<HirStmt>,
        ctxt: &'a mut SemanticCtxt,
        target: &'a TargetSpec,
    ) -> Self {
        TypeChecker {
            hir,
            ctxt,
            registry: TypeRegistry::new(),
            layout_engine: LayoutEngine::new(target),
            active_generic_params: Vec::new(),
            diagnostics,
            corrupted: false,
        }
    }

    pub fn check(&mut self) {
        for stmt in self.hir {
            self.check_stmt(stmt);
        }
    }

    pub fn update_kind(&mut self, node_id: NodeId, new_kind: ResolvedTypeKind) {
        if let Some(info) = self.ctxt.types.types.get_mut(&node_id) {
            info.kind = new_kind;
        }
    }

    pub fn flush_layout_errors(&mut self) {
        if self.layout_engine.corrupted {
            self.corrupted = true;
        }
        for (message, span) in self.layout_engine.errors.drain(..) {
            self.diagnostics
                .report(CompilerError::error(message, Phase::Semantics, Some(span)));
        }
    }

    pub fn unknown(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Unknown;
        let ty_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, ty_id.clone(), span.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id: ty_id,
            layout,
            span,
        }
    }

    pub fn primitive(&mut self, kind: ResolvedTypeKind, span: Span) -> TypeInfo {
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn func(
        &mut self,
        span: Span,
        gen_params: Vec<TypeInfo>,
        params: Vec<TypeInfo>,
        ret: TypeInfo,
    ) -> TypeInfo {
        let _ps: Vec<String> = params
            .iter()
            .map(|param| format!("{}", param.name.clone()))
            .collect();
        let kind = ResolvedTypeKind::Func {
            params,
            gen_type_params: gen_params,
            ret_type: Box::new(ret),
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn tuple(&mut self, fields: Vec<TypeInfo>, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Tuple { fields };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn anonymous(&mut self, fields: Vec<(String, TypeInfo)>, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Anonymous { fields };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn generic(&mut self, name: String, span: Span) -> TypeInfo {
        if self.active_generic_params.contains(&name) {
            self.primitive(ResolvedTypeKind::GenericParam(name.clone()), span.clone())
        } else {
            self.unknown(span)
        }
    }

    pub fn array(&mut self, inner: TypeInfo, size: Option<u64>, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Array {
            inner: Box::new(inner.clone()),
            size,
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn nullable(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Nullable {
            ty: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn failable(&mut self, ok: TypeInfo, err: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Failable {
            ok: Box::new(ok),
            err: Box::new(err),
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn pointer(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Pointer {
            inner: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn reference(&mut self, inner: TypeInfo, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Ref {
            inner: Box::new(inner),
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());
        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn unit(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Unit;
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn boolean(&mut self, span: Span) -> TypeInfo {
        let kind = ResolvedTypeKind::Bool;
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            kind: kind.clone(),
            name: TypeInfo::name(kind),
            type_id,
            layout,
            span,
        }
    }

    pub fn struct_ty(
        &mut self,
        name: String,
        gen_params: Vec<TypeInfo>,
        members: Vec<(String, TypeInfo)>,
        span: Span,
    ) -> TypeInfo {
        let kind = ResolvedTypeKind::Struct {
            name: name.clone(),
            gen_type_params: gen_params,
            members,
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            name: TypeInfo::name(kind.clone()),
            kind,
            type_id,
            layout,
            span,
        }
    }

    pub fn enum_ty(
        &mut self,
        name: String,
        underlying: TypeInfo,
        members: Vec<(String, TypeInfo)>,
        span: Span,
    ) -> TypeInfo {
        let kind = ResolvedTypeKind::Enum {
            name: name.clone(),
            underlying: Box::new(underlying),
            members,
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            name: TypeInfo::name(kind.clone()),
            kind,
            type_id,
            layout,
            span,
        }
    }

    pub fn variant_ty(
        &mut self,
        name: String,
        gen_params: Vec<TypeInfo>,
        arms: Vec<(String, TypeInfo, Vec<TypeInfo>)>,
        span: Span,
    ) -> TypeInfo {
        let kind = ResolvedTypeKind::Variant {
            name: name.clone(),
            gen_type_params: gen_params,
            arms,
        };
        let type_id = self.registry.issue_id(kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&kind, type_id.clone(), span.clone());

        TypeInfo {
            name: TypeInfo::name(kind.clone()),
            kind,
            type_id,
            layout,
            span,
        }
    }

    pub fn type_from_hir_type(&mut self, ty: &HirTypeNode) -> TypeInfo {
        let type_info = match &ty.kind {
            HirType::I8 => self.primitive(ResolvedTypeKind::I8, ty.span.clone()),
            HirType::I16 => self.primitive(ResolvedTypeKind::I16, ty.span.clone()),
            HirType::I32 => self.primitive(ResolvedTypeKind::I32, ty.span.clone()),
            HirType::I64 => self.primitive(ResolvedTypeKind::I64, ty.span.clone()),
            HirType::I128 => self.primitive(ResolvedTypeKind::I128, ty.span.clone()),
            HirType::U8 => self.primitive(ResolvedTypeKind::U8, ty.span.clone()),
            HirType::U16 => self.primitive(ResolvedTypeKind::U16, ty.span.clone()),
            HirType::U32 => self.primitive(ResolvedTypeKind::U32, ty.span.clone()),
            HirType::U64 => self.primitive(ResolvedTypeKind::U64, ty.span.clone()),
            HirType::U128 => self.primitive(ResolvedTypeKind::U128, ty.span.clone()),
            HirType::ISize => self.primitive(ResolvedTypeKind::ISize, ty.span.clone()),
            HirType::USize => self.primitive(ResolvedTypeKind::USize, ty.span.clone()),
            HirType::F32 => self.primitive(ResolvedTypeKind::F32, ty.span.clone()),
            HirType::F64 => self.primitive(ResolvedTypeKind::F64, ty.span.clone()),
            HirType::Bool => self.primitive(ResolvedTypeKind::Bool, ty.span.clone()),
            HirType::Unit => self.unit(ty.span.clone()),

            HirType::Ptr(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.pointer(inner_ty, ty.span.clone())
            }
            HirType::Ref(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.reference(inner_ty, ty.span.clone())
            }
            HirType::Nullable(inner) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.nullable(inner_ty, ty.span.clone())
            }

            HirType::Failable(ok, err) => {
                let ok_ty = self.type_from_hir_type(ok);
                let err_ty = self.type_from_hir_type(err);
                self.failable(ok_ty, err_ty, ty.span.clone())
            }

            HirType::Array(inner, size) => {
                let inner_ty = self.type_from_hir_type(inner);
                self.array(inner_ty, *size, ty.span.clone())
            }

            HirType::Func(params, return_type) => {
                let _ps = params
                    .iter()
                    .map(|p| self.type_from_hir_type(p).clone())
                    .collect();
                let ret_ty = self.type_from_hir_type(return_type);
                self.func(ty.span.clone(), _ps, vec![], ret_ty)
            }
            HirType::Tuple(fields) => {
                let members = fields.iter().map(|f| self.type_from_hir_type(f)).collect();
                self.tuple(members, ty.span.clone())
            }

            HirType::AnonymousStruct(members) => {
                let fields = members
                    .iter()
                    .map(|f| self.anon_struct_field_type(f))
                    .collect();
                self.anonymous(fields, ty.span.clone())
            }

            HirType::GenericType { type_params, .. } => {
                let decl_id: NodeId = *self
                    .ctxt
                    .names
                    .resolved
                    .get(&ty.hir_id)
                    .expect("Resolver already linked this ID");

                let concrete_args = type_params
                    .iter()
                    .map(|arg| self.type_from_hir_type(arg))
                    .collect();

                let key = InstanceKey {
                    original_def_id: decl_id,
                    concrete_args,
                };

                self.ctxt.monomorph_backlog.insert(key);

                self.look_up_declared_type(ty.hir_id, ty.span.clone())
            }

            HirType::CustomType(_) => self.look_up_declared_type(ty.hir_id, ty.span.clone()),
            HirType::GenericPlaceHolder(name) => self.generic(name.clone(), ty.span.clone()),
        };
        self.insert(ty.hir_id, type_info.clone());
        type_info
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

    pub fn is_integer(&self, kind: &ResolvedTypeKind) -> bool {
        match kind {
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
            | ResolvedTypeKind::USize => true,
            _ => false,
        }
    }

    fn struct_field_type(&mut self, member: &HirParam) -> (String, TypeInfo) {
        let field_ty = self.type_from_hir_type(&member.ty);
        (member.name.clone(), field_ty)
    }


    fn variant_field_ty(
        &mut self,
        member: &HirVariantMember,
        variant_ty: TypeInfo,
    ) -> (String, TypeInfo, Vec<TypeInfo>) {
        let payloads = member
            .member_types
            .iter()
            .map(|ps| self.type_from_hir_type(ps))
            .collect();
        (member.name.clone(), variant_ty, payloads)
    }

    fn anon_struct_field_type(&mut self, member: &HirAnonStructField) -> (String, TypeInfo) {
        let field_ty = self.type_from_hir_type(&member.ty);
        (member.name.clone(), field_ty)
    }

    fn enum_member_type(
        &mut self,
        member: &HirEnumMember,
        enum_ty: TypeInfo,
    ) -> (String, TypeInfo) {
        (member.name.clone(), enum_ty)
    }

    fn get_ty_node_name(&self, ty: &HirTypeNode) -> String {
        if let HirType::GenericPlaceHolder(name) = &ty.kind {
            name.clone()
        } else {
            "".to_string()
        }
    }

    pub fn declare_custom_types(&mut self, stmt: &HirStmt) {
        let ty_kind = match &stmt.kind {
            HirStmtKind::HirStructDecl {
                name,
                generic_type_params,
                fields,
                ..
            } => {
                let param_names: Vec<String> = generic_type_params
                    .iter()
                    .map(|p| self.get_ty_node_name(p))
                    .collect();

                self.active_generic_params = param_names.clone();

                let members = fields.iter().map(|f| self.struct_field_type(f)).collect();

                self.active_generic_params.clear();

                ResolvedTypeKind::Struct {
                    name: name.clone(),
                    gen_type_params: param_names
                        .iter()
                        .map(|name| {
                            self.primitive(
                                ResolvedTypeKind::GenericParam(name.clone()),
                                stmt.span.clone(),
                            )
                        })
                        .collect(),
                    members,
                }
            }
            HirStmtKind::HirVariantDecl {
                name,
                members,
                generic_type_params,
                ..
            } => {
                let g_params = generic_type_params
                    .iter()
                    .map(|gen_p| self.type_from_hir_type(gen_p))
                    .collect();

                let variant_stub =
                    self.variant_ty(name.clone(), g_params, vec![], stmt.span.clone());

                let new_kind = ResolvedTypeKind::Variant {
                    name: name.clone(),
                    gen_type_params: generic_type_params
                        .iter()
                        .map(|gen_p| self.type_from_hir_type(gen_p))
                        .collect(),
                    arms: members
                        .iter()
                        .map(|m| self.variant_field_ty(m, variant_stub.clone()))
                        .collect(),
                };
                self.update_kind(stmt.hir_id, new_kind.clone());
                new_kind
            }
            HirStmtKind::HirEnumDecl {
                name,
                underlying,
                members,
                ..
            } => {
                let underlying_ty = self.type_from_hir_type(underlying);
                let enum_stub = self.enum_ty(
                    name.clone(),
                    underlying_ty.clone(),
                    vec![],
                    stmt.span.clone(),
                );
                let fields = members
                    .iter()
                    .map(|f| self.enum_member_type(f, enum_stub.clone()))
                    .collect();
                let new_kind = ResolvedTypeKind::Enum {
                    name: name.clone(),
                    underlying: Box::new(underlying_ty),
                    members: fields,
                };
                self.update_kind(stmt.hir_id, new_kind.clone());
                new_kind
            }
            HirStmtKind::HirFunctionDecl {
                params,
                return_type,
                generic_type_params,
                ..
            }
            | HirStmtKind::HirFunctionDef {
                params,
                return_type,
                generic_type_params,
                ..
            } => {
                let param_names: Vec<String> = generic_type_params
                    .iter()
                    .map(|p| self.get_ty_node_name(p))
                    .collect();

                self.active_generic_params = param_names.clone();

                let ret_ty = self.type_from_hir_type(return_type);

                for p in params{
                    self.check_func_param_type(p);
                }

                let resolved_params: Vec<TypeInfo> = params
                    .iter()
                    .map(|p| self.type_from_hir_type(&p.ty))
                    .collect();

                self.active_generic_params.clear();

                ResolvedTypeKind::Func {
                    params: resolved_params,
                    gen_type_params: param_names
                        .iter()
                        .map(|name| {
                            self.primitive(
                                ResolvedTypeKind::GenericParam(name.clone()),
                                stmt.span.clone(),
                            )
                        })
                        .collect(),
                    ret_type: Box::new(ret_ty),
                }
            }
            HirStmtKind::HirAlias { original, .. } => {
                let original_ty = self.type_from_hir_type(original);
                original_ty.kind
            }
            _ => ResolvedTypeKind::Unknown,
        };

        let ty_id = self.registry.issue_id(ty_kind.clone());
        let layout = self
            .layout_engine
            .layout_of(&ty_kind, ty_id.clone(), stmt.span.clone());
        let ty_info = TypeInfo {
            kind: ty_kind.clone(),
            name: TypeInfo::name(ty_kind),
            type_id: ty_id,
            layout,
            span: stmt.span.clone(),
        };

        self.insert(stmt.hir_id, ty_info);
    }

    pub fn is_signed_numeric(&self, ty: &TypeInfo) -> bool {
        match &ty.kind {
            ResolvedTypeKind::I8
            | ResolvedTypeKind::I32
            | ResolvedTypeKind::ISize
            | ResolvedTypeKind::I128
            | ResolvedTypeKind::F32
            | ResolvedTypeKind::F64 => true,
            _ => false,
        }
    }

    pub fn get_decl_type(&mut self, decl_id: &NodeId, span: Span) -> TypeInfo {
        match self.ctxt.types.types.get(&decl_id) {
            Some(ty) => ty.clone(),
            None => self.unknown(span),
        }
    }

    pub fn type_mismatch(&mut self, expected: &TypeInfo, actual: &TypeInfo, span: Span) {
        self.report(
            format!(
                "Type mismatch between '{}' and '{}'",
                expected.name, actual.name
            ),
            Some(span),
        );
    }

    pub fn unknown_member(&mut self, field_name: &String, ty_name: &String, span: Span) {
        self.report(
            format!("'{}' is not a member of '{}'", field_name, ty_name),
            Some(span),
        );
    }

    pub fn specialize_signature(
        &mut self,
        template: &TypeInfo,
        concrete_args: &[TypeInfo],
        span: Span,
    ) -> TypeInfo {
        match &template.kind {
            ResolvedTypeKind::Struct {
                name,
                gen_type_params,
                members,
            } => {
                let specialized_members = members
                    .iter()
                    .map(|(field_name, field_ty)| {
                        (
                            field_name.clone(),
                            self.substitute_type(field_ty, gen_type_params, concrete_args),
                        )
                    })
                    .collect();

                self.struct_ty(
                    name.clone(),
                    concrete_args.to_vec(),
                    specialized_members,
                    span,
                )
            }
            ResolvedTypeKind::Func {
                params,
                gen_type_params,
                ret_type,
            } => {
                let specialized_params = params
                    .iter()
                    .map(|param_ty| self.substitute_type(param_ty, gen_type_params, concrete_args))
                    .collect();

                let specialized_ret =
                    self.substitute_type(ret_type, gen_type_params, concrete_args);

                let specialized_kind = ResolvedTypeKind::Func {
                    params: specialized_params,
                    gen_type_params: Vec::new(),
                    ret_type: Box::new(specialized_ret),
                };

                let ty_id = self.registry.issue_id(specialized_kind.clone());
                let layout =
                    self.layout_engine
                        .layout_of(&specialized_kind, ty_id.clone(), span.clone());

                TypeInfo {
                    name: TypeInfo::name(specialized_kind.clone()),
                    kind: specialized_kind,
                    type_id: ty_id,
                    layout,
                    span,
                }
            }
            _ => template.clone(),
        }
    }

    fn substitute_type(
        &mut self,
        current: &TypeInfo,
        template_gen_params: &[TypeInfo],
        concrete_args: &[TypeInfo],
    ) -> TypeInfo {
        match &current.kind {
            ResolvedTypeKind::GenericParam(param_name) => {
                let position = template_gen_params
                    .iter()
                    .position(|param| &param.name == param_name);

                if let Some(idx) = position {
                    if let Some(concrete) = concrete_args.get(idx) {
                        let mut concrete_clone = concrete.clone();
                        concrete_clone.span = current.span.clone(); // Retain local usage span
                        return concrete_clone;
                    }
                }

                current.clone()
            }
            ResolvedTypeKind::Pointer { inner } => {
                let substituted_inner =
                    self.substitute_type(inner, template_gen_params, concrete_args);
                self.pointer(substituted_inner, current.span.clone())
            }
            ResolvedTypeKind::Ref { inner } => {
                let substituted_inner =
                    self.substitute_type(inner, template_gen_params, concrete_args);
                self.reference(substituted_inner, current.span.clone())
            }
            // ... check other variants like Arrays or Tuples if they contain nested generics
            _ => current.clone(),
        }
    }

    pub fn insert(&mut self, id: NodeId, ty: TypeInfo) {
        self.ctxt.types.types.insert(id, ty);
    }

    pub fn look_up_declared_type(&mut self, usage_id: NodeId, span: Span) -> TypeInfo {
        if let Some(decl_id) = self.ctxt.names.resolved.get(&usage_id) {
            self.get_decl_type(&decl_id.clone(), span.clone())
        } else {
            self.unknown(span)
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
