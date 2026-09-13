mod common;

use common::analyze;
use unnc::const_and_mut_validator::Validator;
use unnc::hir::{HirExpr, HirExprKind, HirStmt, HirStmtKind, HirType};
use unnc::indexer::NodeIndex;
use unnc::mir::MIRBuilder;
use unnc::target::TargetSpec;

/// Runs the pipeline up through monomorphization and returns the
/// monomorphized HIR tree plus the instance backlog recorded during type
/// checking.
fn mono(src: &str) -> (Vec<HirStmt>, Vec<String>) {
    let (mut semantics, diag) = analyze(src, &[]);
    assert!(
        !semantics.corrupted,
        "analysis should succeed for:\n{src}\ngot: {:?}",
        common::messages(&diag),
    );
    let mut backlog: Vec<String> = semantics
        .ctxt
        .monomorph_backlog
        .iter()
        .map(|k| {
            format!(
                "{:?}[{}]",
                k.original_def_id.local,
                k.concrete_args
                    .iter()
                    .map(|a| a.name.clone())
                    .collect::<Vec<_>>()
                    .join(",")
            )
        })
        .collect();
    backlog.sort();
    (semantics.generate_monormophizer_hir(), backlog)
}

fn find_func<'a>(hir: &'a [HirStmt], name: &str) -> &'a HirStmt {
    hir.iter()
        .find(|s| matches!(&s.kind, HirStmtKind::HirFunctionDef { name: n, .. } if n == name))
        .unwrap_or_else(|| panic!("expected function '{name}' in monomorphized tree"))
}

fn find_var_init<'a>(body: &'a [HirStmt], var_name: &str) -> &'a HirExpr {
    body.iter()
        .find_map(|s| match &s.kind {
            HirStmtKind::HirVarDecl { name, init, .. } if name == var_name => Some(init.as_ref()),
            _ => None,
        })
        .unwrap_or_else(|| panic!("expected var '{var_name}' in function body"))
}

fn find_decl<'a>(hir: &'a [HirStmt], name: &str) -> &'a HirStmt {
    hir.iter()
        .find(|s| {
            matches!(
                &s.kind,
                HirStmtKind::HirFunctionDef { name: n, .. }
                    | HirStmtKind::HirStructDecl { name: n, .. }
                    | HirStmtKind::HirEnumDecl { name: n, .. }
                    | HirStmtKind::HirVariantDecl { name: n, .. }
                    | HirStmtKind::HirFunctionDecl { name: n, .. }
                    if n == name
            )
        })
        .unwrap_or_else(|| panic!("expected declaration '{name}' in monomorphized tree"))
}

/// The concrete-args suffix of a single backlog entry (its def-id differs by
/// node ordering, so assert only the argument type).
fn single_instance_arg(backlog: &[String]) -> &str {
    assert_eq!(backlog.len(), 1, "expected exactly one instance in {backlog:?}");
    let entry = &backlog[0];
    entry
        .split_once('[')
        .map(|(_, rest)| rest.trim_end_matches(']'))
        .unwrap_or(entry)
}

#[test]
fn generic_call_emits_mangled_instance_and_rewrites_call_site() {
    let (hir, backlog) = mono(
        "generics <T> { func identity(v: T): T { return v; } }\n\
         func main(): i32 { var x := identity::<i32>(5); return 0i32; }\n",
    );

    assert_eq!(backlog, vec!["6[i32]"]);

    let def = find_func(&hir, "_U_identity_i32");
    let HirStmtKind::HirFunctionDef {
        params,
        return_type,
        body,
        ..
    } = &def.kind
    else {
        panic!("expected function def");
    };
    assert_eq!(params.len(), 1);
    assert!(
        matches!(params[0].ty.kind, HirType::I32),
        "param must become concrete i32: {:?}",
        params[0].ty.kind
    );
    assert!(matches!(return_type.kind, HirType::I32));

    let inst_body_ret = params[0].name.clone();
    let body_ret = body
        .iter()
        .find_map(|s| match &s.kind {
            HirStmtKind::HirReturn(Some(e)) => Some(e),
            _ => None,
        })
        .expect("instance should return");
    assert!(
        matches!(body_ret.kind, HirExprKind::Identifier(ref n) if n == &inst_body_ret),
        "identity instance must return its param, got {:?}",
        body_ret.kind
    );

    let main = find_func(&hir, "main");
    let HirStmtKind::HirFunctionDef { body, .. } = &main.kind else {
        panic!("expected function def");
    };
    assert_call_rewritten_to(
        body,
        |k| matches!(k, HirStmtKind::HirVarDecl { name, .. } if name == "x"),
        "_U_identity_i32",
    );
}

#[test]
fn same_template_two_concrete_types_both_emitted() {
    let (hir, backlog) = mono(
        "generics <T> { func identity(v: T): T { return v; } }\n\
         func main(): i32 { var a := identity::<i32>(5); var b := identity::<f64>(5.0); return 0i32; }\n",
    );

    let entries: Vec<&str> = backlog.iter().map(|s| s.as_str()).collect();
    assert!(entries.iter().any(|s| s.ends_with("[i32]")), "missing i32 in {backlog:?}");
    assert!(entries.iter().any(|s| s.ends_with("[f64]")), "missing f64 in {backlog:?}");

    find_func(&hir, "_U_identity_i32");
    find_func(&hir, "_U_identity_f64");
}

#[test]
fn non_generic_function_passes_through_unmangled() {
    let (hir, backlog) = mono(
        "func add(a: i32, b: i32): i32 { return a + b; }\n\
         func main(): i32 { return add(1, 2); }\n",
    );

    assert!(backlog.is_empty());
    find_func(&hir, "add");
    assert!(
        !hir.iter().any(|s| {
            matches!(&s.kind, HirStmtKind::HirFunctionDef{name, ..} if name.starts_with("_U_"))
        }),
        "no mangled instances should be produced for non-generic code"
    );

    let main = find_func(&hir, "main");
    let HirStmtKind::HirFunctionDef { body, .. } = &main.kind else {
        panic!("expected function def");
    };
    let ret = body
        .iter()
        .find_map(|s| match &s.kind {
            HirStmtKind::HirReturn(Some(e)) => Some(e),
            _ => None,
        })
        .expect("main should return");
    match &ret.kind {
        HirExprKind::Call(callee, _) => match &callee.kind {
            HirExprKind::Identifier(name) => assert_eq!(name, "add"),
            other => panic!("expected identifier callee, got {other:?}"),
        },
        other => panic!("expected call, got {other:?}"),
    }
}

/// Runs the *entire* front-end pipeline down to a built MIR module for `src`.
/// Any stage corruption (type checker, validator, control-flow, MIR builder)
/// fails the test, so this proves the generated mono instances are fully
/// buildable — not just structurally correct.
fn mono_e2e(src: &str) -> unnc::mir::MIRModule {
    let (mut semantics, diag) = analyze(src, &[]);
    if semantics.corrupted {
        panic!(
            "analysis should succeed for:\n{src}\ngot: {:?}",
            common::messages(&diag),
        );
    }

    let monomorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorphized_hir);

    if semantics.verify_contracts(&hir_index, common::Rc::clone(&diag)) {
        panic!("contract verification failed for:\n{src}");
    }
    if semantics.check_control_flow(&hir_index, common::Rc::clone(&diag)) {
        panic!("control-flow check failed for:\n{src}");
    }

    let mut validator = Validator::new(common::Rc::clone(&diag));
    validator.run(&monomorphized_hir);
    if validator.corrupted {
        panic!(
            "validator failed for:\n{src}\ngot: {:?}",
            common::messages(&diag),
        );
    }

    let target: &'static TargetSpec = Box::leak(Box::new(TargetSpec::new(None, None, None, None)));
    let mut mir_builder = MIRBuilder::new(
        &hir_index,
        &semantics.ctxt.types,
        target,
        common::Rc::clone(&diag),
        "test".to_string(),
    );
    let mir_module = mir_builder.build_module();
    if mir_builder.corrupted {
        panic!(
            "MIR build failed for:\n{src}\ngot: {:?}",
            common::messages(&diag),
        );
    }
    mir_module
}

fn assert_call_rewritten_to(
    body: &[HirStmt],
    stmt_pred: impl Fn(&HirStmtKind) -> bool,
    expected: &str,
) {
    let found = body
        .iter()
        .find(|s| stmt_pred(&s.kind))
        .expect("expected statement in body");
    let expr = match &found.kind {
        HirStmtKind::HirReturn(Some(e)) => e.as_ref(),
        HirStmtKind::HirVarDecl { init, .. } => init.as_ref(),
        other => panic!("unexpected stmt kind {other:?}"),
    };
    match &expr.kind {
        HirExprKind::Call(callee, _) => match &callee.kind {
            HirExprKind::Identifier(name) => assert_eq!(name, expected),
            other => panic!("expected rewritten identifier, got {other:?}"),
        },
        other => panic!("expected call, got {other:?}"),
    }
}

#[test]
fn ptr_generic_arg_substitutes_inner_type() {
    // MONO-3: `substitute_type` must recurse into `Ptr`'s inner type, so the
    // instance's param is `ptr<i32>`, never a leftover `ptr<T>`.
    let (hir, backlog) = mono(
        "generics <T> { func deref(p: ptr<T>): T { return ^p; } }\n\
         func main(): i32 { var x := 7i32; var q := deref::<i32>(@x); return 0i32; }\n",
    );

    assert_eq!(single_instance_arg(&backlog), "i32");

    let def = find_func(&hir, "_U_deref_i32");
    let HirStmtKind::HirFunctionDef { params, .. } = &def.kind else {
        panic!("expected function def");
    };
    assert_eq!(params.len(), 1);
    let HirType::Ptr(inner) = &params[0].ty.kind else {
        panic!("expected pointer param, got {:?}", params[0].ty.kind);
    };
    assert!(
        matches!(inner.kind, HirType::I32),
        "ptr generic argument must be substituted: {:?}",
        inner.kind
    );
}

#[test]
fn generic_call_inside_return_gets_rewritten() {
    // MONO-6: `HirReturn` bodies were never visited, so a generic call inside
    // a `return` kept its `GenericInstantion` node (leaving an unresolved
    // call in the emitted code).
    let (hir, backlog) = mono(
        "generics <T> { func identity(v: T): T { return v; } }\n\
         func pp(x: i32): i32 { return identity::<i32>(x); }\n\
         func main(): i32 { var y := pp(5); return 0i32; }\n",
    );

    assert_eq!(single_instance_arg(&backlog), "i32");
    find_func(&hir, "_U_identity_i32");

    let pp = find_func(&hir, "pp");
    let HirStmtKind::HirFunctionDef { body, .. } = &pp.kind else {
        panic!("expected function def");
    };
    assert_call_rewritten_to(
        body,
        |k| matches!(k, HirStmtKind::HirReturn(_)),
        "_U_identity_i32",
    );
}

#[test]
fn nested_generic_with_generic_arg_typechecks_and_emits_both_instances() {
    // MONO-5: `identity::<T>(v)` inside a generic `id2` must typecheck (the
    // generic param stays in scope for the body), and monomorphization must
    // lazily emit `_U_identity_i32` when `id2::<i32>` is instantiated.
    let (hir, backlog) = mono(
        "generics <T> { func identity(v: T): T { return v; } func id2(v: T): T { return identity::<T>(v); } }\n\
         func main(): i32 { var z := id2::<i32>(5); return 0i32; }\n",
    );

    assert_eq!(single_instance_arg(&backlog), "i32");

    let id2 = find_func(&hir, "_U_id2_i32");
    let HirStmtKind::HirFunctionDef { params, body, .. } = &id2.kind else {
        panic!("expected function def");
    };
    assert!(matches!(params[0].ty.kind, HirType::I32));
    assert_call_rewritten_to(
        body,
        |k| matches!(k, HirStmtKind::HirReturn(_)),
        "_U_identity_i32",
    );

    let iden = find_func(&hir, "_U_identity_i32");
    let HirStmtKind::HirFunctionDef { params, .. } = &iden.kind else {
        panic!("expected function def");
    };
    assert!(matches!(params[0].ty.kind, HirType::I32));
}

#[test]
fn struct_generic_arg_resolves_to_custom_type() {
    // MONO-7: a non-generic struct passed to a generic must lower to
    // `CustomType(Point)`, never the `Unit` fallback.
    let (hir, backlog) = mono(
        "struct Point { x: i32, y: i32 }\n\
         generics <T> { func tag(v: T): i32 { return 1i32; } }\n\
         func main(): i32 { var p := .Point{.x = 1i32, .y = 2i32}; var q := tag::<Point>(p); return 0i32; }\n",
    );

    assert_eq!(single_instance_arg(&backlog), "Point");

    let def = find_func(&hir, "_U_tag_Point");
    let HirStmtKind::HirFunctionDef { params, .. } = &def.kind else {
        panic!("expected function def");
    };
    assert_eq!(params.len(), 1);
    assert!(
        matches!(&params[0].ty.kind, HirType::CustomType(name) if name == "Point"),
        "generic struct arg must become CustomType: {:?}",
        params[0].ty.kind
    );
}

#[test]
fn generic_struct_value_emits_concrete_instance_and_field_access() {
    // MONO-4: a generic struct value (`.Pair<i32>{...}`) must resolve to the
    // mangled instance and its fields must be substituted.
    let (hir, backlog) = mono(
        "generics <T> { struct Pair { a: T, b: T } }\n\
         func main(): i32 { var pr := .Pair<i32>{.a = 1i32, .b = 2i32}; var g := pr.b; return g; }\n",
    );

    assert_eq!(single_instance_arg(&backlog), "i32");

    let def = find_decl(&hir, "_U_Pair_i32");
    let HirStmtKind::HirStructDecl { fields, .. } = &def.kind else {
        panic!("expected struct decl instance");
    };
    assert_eq!(fields.len(), 2);
    assert!(matches!(fields[1].ty.kind, HirType::I32));

    let main = find_func(&hir, "main");
    let HirStmtKind::HirFunctionDef { body, .. } = &main.kind else {
        panic!("expected function def");
    };
    let init = find_var_init(body, "pr");
    match &init.kind {
        HirExprKind::Instantiation { init_ty, .. } => {
            assert!(
                matches!(&init_ty.kind, HirType::CustomType(name) if name == "_U_Pair_i32"),
                "struct value must point at the mangled instance: {:?}",
                init_ty.kind
            );
        }
        other => panic!("expected struct instantiation, got {other:?}"),
    }
}

#[test]
fn end_to_end_generic_functions_build_mir() {
    // MONO-1 + MONO-2: two instances of one template must both resolve at
    // their call sites *and* carry concrete recorded types, so the MIR builder
    // (registration pre-pass + fresh-id type table) can lower them.
    let mir = mono_e2e(
        "generics <T> { func identity(v: T): T { return v; } }\n\
         func main(): i32 { var a := identity::<i32>(5); var b := identity::<f64>(5.0); return 0i32; }\n",
    );
    assert!(
        mir.functions.iter().any(|(_, f)| f.name == "_U_identity_i32"),
        "expected _U_identity_i32 in MIR module"
    );
    assert!(
        mir.functions.iter().any(|(_, f)| f.name == "_U_identity_f64"),
        "expected _U_identity_f64 in MIR module"
    );
}

#[test]
fn end_to_end_ptr_and_nested_generics_build_mir() {
    // MONO-1/2/3/5 combined: nested `identity::<T>` inside `id2::<i32>`,
    // plus a `ptr` instance, must all lower to MIR without missing type ids.
    mono_e2e(
        "generics <T> { func identity(v: T): T { return v; } }\n\
         generics <U> { func deref(p: ptr<U>): U { return ^p; } }\n\
         generics <V> { func id2(v: V): V { return identity::<V>(v); } }\n\
         func main(): i32 { var x := 7i32; var q := deref::<i32>(@x); var z := id2::<i32>(5); return 0i32; }\n",
    );
}

#[test]
fn end_to_end_generic_struct_value_builds_mir() {
    // MONO-4: a generic struct instance must be registered (name → struct id)
    // before the root function body that uses it is lowered.
    mono_e2e(
        "generics <T> { struct Pair { a: T, b: T } }\n\
         func main(): i32 { var pr := .Pair<i32>{.a = 1i32, .b = 2i32}; var g := pr.b; return g; }\n",
    );
}

#[test]
fn end_to_end_struct_arg_and_return_generics_build_mir() {
    // MONO-1/6/7: a generic called with a non-generic struct arg (CustomType
    // param) and a generic call inside a `return` must both lower to MIR.
    mono_e2e(
        "struct Point { x: i32, y: i32 }\n\
         generics <T> { func tag(v: T): i32 { return 1i32; } func identity(v: T): T { return v; } }\n\
         func pp(p: Point): Point { return identity::<Point>(p); }\n\
         func main(): i32 { var p := .Point{.x = 1i32, .y = 2i32}; var q := tag::<Point>(p); var r := pp(p); return 0i32; }\n",
    );
}