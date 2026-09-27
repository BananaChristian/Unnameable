use std::collections::HashMap;

use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    hir::{HirExpr, HirExprKind, HirLiteral, HirParam, HirStmt, HirStmtKind, HirType, HirTypeNode},
};

/// What a binding permits.
///
/// `mut var` and `mut T` are two *different* permissions and are tracked
/// separately, because they answer different questions:
///
/// - `reassignable` (`mut var`) answers "may this binding be reassigned?"
/// - `mut_at_depth` (`mut T`) answers "may a write go *through* the type `T`?"
///
/// A direct write of the binding (`x = 1`) is permitted by either, because
/// writing to a value of type `mut T` *is* writing through `mut T`. A write
/// that reaches through the type (`x.f = 1`, `x[i] = 1`, `^p = 1`) needs
/// `mut_at_depth` at the right depth; `mut var` alone does not grant it,
/// because `mut var` says nothing about the type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingKind {
    /// Declared `const var`: no writes at all.
    pub is_const: bool,
    /// Declared `mut var`: the binding itself may be reassigned.
    pub reassignable: bool,
    /// `mut_at_depth[d]` is true iff the type reached by peeling `d` pointer
    /// layers off the declared type is `mut _`.
    ///
    /// The depth matters because *where* `mut` sits decides what it permits:
    /// `var p: ptr<mut i32>` has `[false, true]`, so `^p = 1` is allowed but
    /// `p = q` is not -- the `mut` belongs to the pointee, not to `p`. A
    /// declared `mut S` has `[true]`, so both `s = other` and `s.f = 1` are
    /// allowed. A shorter vector than the actual deref depth denies, which is
    /// the safe direction.
    pub mut_at_depth: Vec<bool>,
}

impl BindingKind {
    /// Nothing is permitted. Also used for names that are not in scope.
    pub(super) fn deny_all() -> Self {
        BindingKind {
            is_const: false,
            reassignable: false,
            mut_at_depth: Vec::new(),
        }
    }

    fn with_type(is_const: bool, reassignable: bool, ty: Option<&HirTypeNode>) -> Self {
        BindingKind {
            is_const,
            reassignable,
            mut_at_depth: Self::mut_depths(ty),
        }
    }

    /// Walk the declared type, recording at each pointer depth whether the type
    /// sitting there is `mut _`. Stops as soon as a layer is not a
    /// pointer/reference, because nothing deeper can be reached by dereferencing.
    fn mut_depths(ty: Option<&HirTypeNode>) -> Vec<bool> {
        let mut out = Vec::new();
        let mut cur = ty;
        while let Some(node) = cur {
            out.push(matches!(node.kind, HirType::Mut(_)));
            match &node.kind {
                HirType::Ptr(inner) | HirType::Ref(inner) => cur = Some(inner),
                // Not a pointer: dereferencing stops here, so there is no deeper
                // level to record.
                _ => break,
            }
        }
        out
    }

    /// May this binding be written to directly (reassignment)? `depth` is always
    /// 0 for a direct write, since writing the binding touches its own type.
    pub(super) fn allows_direct_write(&self) -> bool {
        !self.is_const && (self.reassignable || self.allows_write_through(0))
    }

    /// May a write go *through* this binding's type at `depth` dereferences?
    /// Only `mut T` grants this.
    ///
    /// Deliberately not gated on `is_const`: `const` qualifies the *binding* and
    /// is enforced by [`BindingKind::allows_direct_write`]. Gating here too would
    /// re-conflate the two permissions this type exists to keep apart.
    ///
    /// This is safe today because a `const` binding cannot have storage to write
    /// through: `is_compile_time_literal` restricts its initializer to a scalar
    /// `HirLiteral`, and every `HirLiteral` variant other than `ArrayLiteral` and
    /// `Null` is a scalar (`ArrayLiteral`/`Null` are rejected outright). A scalar
    /// has no fields, no elements, and is not a pointer, so no through-write is
    /// even expressible against a `const` binding.
    pub(super) fn allows_write_through(&self, depth: usize) -> bool {
        self.mut_at_depth.get(depth).copied().unwrap_or(false)
    }
}

/// How a write reaches the binding it is checked against, and how many pointer
/// layers it had to cross to get there. A path built by [`WritePath::direct`] is
/// a plain reassignment; the rest go *through* the binding's type and therefore
/// require `mut T` at the depth they reached.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct WritePath {
    /// True when the write reached the binding through its type rather than
    /// targeting the binding itself.
    through: bool,
    /// How many dereferences were crossed. A write through `^p` sits one layer
    /// deeper than a write through `p`, and `mut_at_depth` is indexed by this.
    derefs: usize,
}

impl WritePath {
    /// `x = 1` -- reassigning the binding itself, so only `mut var` or a `mut T`
    /// at depth 0 (the binding's own type) can permit it.
    pub(super) fn direct() -> Self {
        WritePath {
            through: false,
            derefs: 0,
        }
    }

    /// Mark the write as going through the current type, without changing depth.
    pub(super) fn through_aggregate(&self) -> Self {
        WritePath {
            through: true,
            derefs: self.derefs,
        }
    }

    /// Cross one more pointer layer: `^p`.
    pub(super) fn through_deref(&self) -> Self {
        WritePath {
            through: true,
            derefs: self.derefs + 1,
        }
    }

    pub(super) fn is_through(&self) -> bool {
        self.through
    }

    /// True when the outermost step that made this a through-write was a
    /// dereference, so the diagnostic should blame the pointee rather than the
    /// pointer.
    pub(super) fn is_deref(&self) -> bool {
        self.through && self.derefs > 0
    }

    pub(super) fn deref_depth(&self) -> usize {
        self.derefs
    }
}

pub struct Validator {
    diagnostics: SharedDiagnostics,
    scopes: Vec<HashMap<String, BindingKind>>,
    pub corrupted: bool,
}

impl Validator {
    pub fn new(diagnostics: SharedDiagnostics) -> Self {
        Validator {
            diagnostics,
            scopes: vec![HashMap::new()],
            corrupted: false,
        }
    }

    pub fn look_up(&self, name: &str) -> Option<BindingKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(kind) = scope.get(name) {
                return Some(kind.clone());
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn is_compile_time_literal(&self, expr: &HirExpr) -> bool {
        match &expr.kind {
            HirExprKind::Literal(inner) => match inner {
                HirLiteral::Null | HirLiteral::ArrayLiteral(_) => false, //For now these are a no
                //go
                _ => true,
            },
            _ => false,
        }
    }

    pub fn run(&mut self, hir: &[HirStmt]) {
        for stmt in hir {
            self.check_stmt(stmt);
        }
    }

    pub fn check_stmt(&mut self, stmt: &HirStmt) {
        match &stmt.kind {
            HirStmtKind::HirVarDecl { .. } => self.check_var_decl(stmt),
            HirStmtKind::HirIf { .. } => self.check_if(stmt),
            HirStmtKind::HirWhile { .. } => self.check_while(stmt),
            HirStmtKind::HirExpr(_) | HirStmtKind::HirTailExpr(_) => self.check_expr_stmt(stmt),
            HirStmtKind::HirFunctionDef { .. } => self.check_func_decl(stmt),
            HirStmtKind::HirReturn(val) => {
                if let Some(e) = val {
                    self.check_expr(e);
                }
            }
            _ => (),
        }
    }

    fn check_expr_stmt(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirExpr(inner) | HirStmtKind::HirTailExpr(inner) = &stmt.kind {
            self.check_expr(inner);
        }
    }

    fn check_param(&mut self, param: &HirParam) {
        let kind = BindingKind::with_type(false, param.mutable, Some(&param.ty));
        self.scopes
            .last_mut()
            .unwrap()
            .insert(param.name.clone(), kind);
    }

    fn check_func_decl(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirFunctionDef { params, body, .. } = &stmt.kind {
            self.enter_scope();
            for param in params {
                self.check_param(param);
            }
            for st in body {
                self.check_stmt(st);
            }
            self.exit_scope();
        }
    }

    fn check_while(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirWhile { condition, body } = &stmt.kind {
            self.enter_scope();
            self.check_expr(condition);
            for st in body {
                self.check_stmt(st);
            }
            self.exit_scope();
        }
    }

    fn check_if(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirIf {
            condition,
            body,
            else_body,
        } = &stmt.kind
        {
            self.enter_scope();
            self.check_expr(condition);
            for st in body {
                self.check_stmt(st);
            }
            self.exit_scope();

            if let Some(el_bod) = else_body {
                self.enter_scope();
                for el_st in el_bod {
                    self.check_stmt(el_st);
                }
                self.exit_scope();
            }
        }
    }

    fn check_var_decl(&mut self, stmt: &HirStmt) {
        if let HirStmtKind::HirVarDecl {
            name,
            mutable,
            constant,
            init,
            ty,
            ..
        } = &stmt.kind
        {
            let is_constant = *constant;

            // The initializer may itself contain mutations
            // (e.g. `var z := y = 3;`); walk it.
            self.check_expr(init);

            // `const` and `mut var` are two qualifiers on the *same* binding and
            // genuinely contradict each other, so that pairing is rejected.
            //
            // `mut T` is not in that category: it qualifies the type, not the
            // binding. `const var x: mut i32` is a compile-time-constant binding
            // to a value of mutable type -- "constant name, mutable storage" --
            // which is permitted. The binding still cannot be reassigned
            // (`allows_direct_write` refuses when `is_const`); only writes that
            // go *through* the type could ever be allowed, and a const binding
            // additionally cannot have a type with reachable storage, because
            // `is_compile_time_literal` limits its initializer to a scalar
            // literal. So this grants no way to write through a `const` binding
            // in practice; it just stops the type qualifier from being reported
            // as if it were a binding qualifier.
            if *mutable && is_constant {
                self.report(
                    format!(
                        "Variable '{}' cannot be const and mutable at the same time",
                        name,
                    ),
                    Some(stmt.span.clone()),
                );
            }

            //Is it constant if so, apply come checks
            if is_constant {
                if !self.is_compile_time_literal(init) {
                    self.report(format!("Constant variable '{}' must be initilialized with a compile time value",name), 
                            Some(stmt.span.clone()));
                }
            }

            let kind = BindingKind::with_type(is_constant, *mutable, ty.as_ref());
            self.scopes.last_mut().unwrap().insert(name.clone(), kind);
        }
    }

    pub fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report(CompilerError::error(message, Phase::Semantics, span));
    }
}
