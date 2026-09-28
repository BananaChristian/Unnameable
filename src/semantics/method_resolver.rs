//! Resolves `receiver.method(args)` into the ordinary top-level call that
//! `impl` already lowered to.
//!
//! `impl Point { func sum(self: Point): i32 { ... } }` lowers to a plain
//! function named `Point_sum` taking the receiver as an ordinary first
//! parameter. This pass turns the sugar back into that direct call:
//!
//! ```text
//! p.sum(a)   -->   Point_sum(p, a)
//! ```
//!
//! It runs on HIR after type checking (the receiver's type is needed to know
//! which `{Type}_{fn}` to look for) and before monomorphization, so by the time
//! MIR is built there is nothing left to resolve. MIR, the bytecode builder, the
//! VM and codegen never see a method call or a `self` parameter — they see a
//! function call like any other.
//!
//! Resolution is entirely static: the callee is chosen at compile time from the
//! receiver's *declared* type. There is no runtime type, no vtable, and nothing
//! consulted at the call site.

use std::collections::{HashMap, HashSet};

use crate::{
    hir::{HirBinaryOp, HirExpr, HirExprKind, HirMatchArm, HirStmt, HirStmtKind},
    lowering::NodeId,
    semantics::{ResolvedTypeKind, SemanticCtxt},
};

/// Every declared function name mapped to its declaration id, so a `{Type}_{fn}`
/// method is confirmed to exist rather than assumed and its signature can be
/// read from the type table.
pub fn collect_fn_decls(hir: &[HirStmt], out: &mut HashMap<String, NodeId>) {
    for stmt in hir {
        match &stmt.kind {
            HirStmtKind::HirFunctionDef { name, body, .. } => {
                out.insert(name.clone(), stmt.hir_id);
                collect_fn_decls(body, out);
            }
            HirStmtKind::HirFunctionDecl { name, .. } => {
                out.insert(name.clone(), stmt.hir_id);
            }
            _ => {}
        }
    }
}

pub struct MethodResolver<'a> {
    ctxt: &'a SemanticCtxt,
    /// struct/variant name -> field names. Kept for the ambiguity check, which
    /// lives in the type checker; retained here so a future pass can reuse it.
    #[allow(dead_code)]
    fields: HashMap<String, HashSet<String>>,
}

impl<'a> MethodResolver<'a> {
    pub fn new(ctxt: &'a SemanticCtxt) -> Self {
        MethodResolver {
            ctxt,
            fields: HashMap::new(),
        }
    }

    pub fn run(&mut self, hir: &mut [HirStmt]) {
        self.collect_fields();
        for stmt in hir.iter_mut() {
            self.lower_stmt(stmt);
        }
    }

    /// Field names come from the resolved type table rather than the HIR decls,
    /// because that is the same source the type checker validated access
    /// against.
    fn collect_fields(&mut self) {
        for info in self.ctxt.types.types.values() {
            match &info.kind {
                ResolvedTypeKind::Struct { name, members, .. } => {
                    self.fields
                        .entry(name.clone())
                        .or_default()
                        .extend(members.iter().map(|m| m.0.clone()));
                }
                ResolvedTypeKind::Enum { name, members, .. } => {
                    self.fields
                        .entry(name.clone())
                        .or_default()
                        .extend(members.iter().map(|m| m.0.clone()));
                }
                _ => {}
            }
        }
    }

    fn lower_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::HirFunctionDef { body, .. } => {
                self.lower_stmts(body);
            }
            HirStmtKind::HirFunctionDecl { .. } => {}
            // The initializer is where a method call most often hides, since
            // `var x = recv.m()` is a natural way to write it. Omitting this arm
            // leaves such calls unrewritten and they ICE in the MIR builder.
            HirStmtKind::HirVarDecl { init, .. } => self.lower_expr(init),
            HirStmtKind::HirIf {
                condition,
                body,
                else_body,
            } => {
                self.lower_expr(condition);
                self.lower_block(body);
                if let Some(else_body) = else_body {
                    self.lower_block(else_body);
                }
            }
            HirStmtKind::HirWhile { condition, body } => {
                self.lower_expr(condition);
                self.lower_block(body);
            }
            HirStmtKind::HirReturn(v) => {
                if let Some(v) = v.as_deref_mut() {
                    self.lower_expr(v);
                }
            }
            HirStmtKind::HirExpr(e) | HirStmtKind::HirTailExpr(e) => self.lower_expr(e),
            // Declarations with no expressions in them: structs, contracts,
            // enums, variants, aliases, imports, break, continue.
            _ => {}
        }
    }

    fn lower_stmts(&mut self, stmts: &mut [HirStmt]) {
        for stmt in stmts.iter_mut() {
            self.lower_stmt(stmt);
        }
    }

    fn lower_block(&mut self, body: &mut [HirStmt]) {
        self.lower_stmts(body);
    }

    fn lower_exprs(&mut self, exprs: &mut [HirExpr]) {
        for e in exprs.iter_mut() {
            self.lower_expr(e);
        }
    }

    fn lower_match_arm(&mut self, arm: &mut HirMatchArm) {
        if let Some(guard) = &mut arm.guard {
            self.lower_expr(guard);
        }
        self.lower_expr(&mut arm.body);
    }

    fn lower_expr(&mut self, expr: &mut HirExpr) {
        // Descend first so nested calls are resolved before this level is
        // inspected, then consider rewriting this node.
        match &mut expr.kind {
            HirExprKind::Literal(_)
            | HirExprKind::Identifier(_)
            | HirExprKind::GenericInstantion { .. }
            | HirExprKind::SizeOf(_) => {}

            HirExprKind::Binary(l, op, r) => {
                self.lower_expr(l);
                self.lower_expr(r);
                // A resolved method call is an access whose right-hand side is
                // the call; flatten it after descending so nested calls inside
                // the arguments are already rewritten.
                if matches!(op, HirBinaryOp::Access) {
                    self.resolve_method_call(expr);
                }
            }
            HirExprKind::Unary(_, inner) => self.lower_expr(inner),
            HirExprKind::Unwrap(inner) => self.lower_expr(inner),
            HirExprKind::Postfix(inner, _) => self.lower_expr(inner),
            HirExprKind::Marked(inner) => self.lower_expr(inner),
            HirExprKind::StaticCast(_, inner) | HirExprKind::BitCast(_, inner) => {
                self.lower_expr(inner)
            }

            HirExprKind::Call(callee, args) => {
                self.lower_expr(callee);
                self.lower_exprs(args);
            }
            HirExprKind::Instantiation { body, .. } => {
                for param in body.iter_mut() {
                    self.lower_expr(&mut param.value);
                }
            }
            HirExprKind::TupleInst { body } => self.lower_exprs(body),
            HirExprKind::Index { target, index } => {
                self.lower_expr(target);
                self.lower_expr(index);
            }
            HirExprKind::DollarScope {
                params,
                body,
                result,
            } => {
                self.lower_exprs(params);
                self.lower_block(body);
                if let Some(r) = result {
                    self.lower_expr(r);
                }
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.lower_expr(scrutinee);
                for arm in arms.iter_mut() {
                    self.lower_match_arm(arm);
                }
            }
            HirExprKind::Block(body) => self.lower_block(body),
        }
    }

    /// Flatten a method call the type checker resolved.
    ///
    /// `recv.method(args)` parses as `Binary(recv, Access, Call(method, args))`;
    /// the type checker looked at that shape, typed it as a call to
    /// `{Type}_{method}`, and recorded the mapping here keyed on this node. This
    /// is the only place that knows the shape, and the rewrite turns
    ///
    /// ```text
    /// recv.method(a)   ->   Call(Binary(recv, Access, Call(method, [a])))
    /// ```
    ///
    /// into the ordinary call
    ///
    /// ```text
    /// {Type}_{method}(recv, a)
    /// ```
    ///
    /// After this, the tree is indistinguishable from one where the user wrote
    /// the mangled free function call by hand, which is exactly the property that
    /// keeps MIR and everything after it ignorant of methods.
    fn resolve_method_call(&mut self, expr: &mut HirExpr) {
        let HirExprKind::Binary(recv, HirBinaryOp::Access, call) = &mut expr.kind else {
            return;
        };
        let mangled = match self.ctxt.methods.method_calls.get(&expr.hir_id) {
            Some(m) => m.clone(),
            None => return,
        };
        let HirExprKind::Call(callee, args) = &mut call.kind else {
            return;
        };
        // The callee identifier was already bound to the method's declaration by
        // the type checker, so reusing this node keeps it resolvable.
        let method_id = callee.hir_id;
        let method_span = callee.span.clone();

        // Receiver first, then the explicit arguments.
        let mut new_args = Vec::with_capacity(args.len() + 1);
        new_args.push(*std::mem::replace(
            recv,
            Box::new(HirExpr::new(
                method_id,
                HirExprKind::Identifier(String::new()),
                method_span.clone(),
            )),
        ));
        new_args.append(args);

        // Replace the whole access node, so nothing of the method-call shape is
        // left behind for later phases to trip over.
        expr.kind = HirExprKind::Call(
            Box::new(HirExpr::new(
                method_id,
                HirExprKind::Identifier(mangled),
                method_span.clone(),
            )),
            new_args,
        );
    }
}
