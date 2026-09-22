mod common;

use std::{cell::RefCell, rc::Rc};

use common::analyze;
use unnc::bc_builder::BytecodeBuilder;
use unnc::const_and_mut_validator::Validator;
use unnc::diagnostics::Diagnostics;
use unnc::dollar_folder::Folder;
use unnc::dollar_verifier::DollarVerifier;
use unnc::hir::{HirExpr, HirExprKind, HirStmt, HirStmtKind, HirType};
use unnc::indexer::NodeIndex;
use unnc::mir::{ConstantValue, MIRBuilder, MIRInstruction, MIRModule, MIRValue};
use unnc::target::TargetSpec;
use unnc::vm::{EvalResultTable, VMValue, VM};

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

/// Runs the front end down through MIR construction. Returns `Err` with the
/// reported messages if any stage (including MIR construction) corrupted.
fn mir_build_checked(src: &str) -> Result<(MIRModule, Rc<RefCell<Diagnostics>>), Vec<String>> {
    let (mut semantics, diag) = analyze(src, &[]);
    if semantics.corrupted {
        return Err(common::messages(&diag));
    }

    let monomorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorphized_hir);

    if semantics.verify_contracts(&hir_index, Rc::clone(&diag)) {
        return Err(common::messages(&diag));
    }
    if semantics.check_control_flow(&hir_index, Rc::clone(&diag)) {
        return Err(common::messages(&diag));
    }

    let mut validator = Validator::new(Rc::clone(&diag));
    validator.run(&monomorphized_hir);
    if validator.corrupted {
        return Err(common::messages(&diag));
    }

    let target: &'static TargetSpec = Box::leak(Box::new(TargetSpec::new(None, None, None, None)));
    let mut mir_builder = MIRBuilder::new(
        &hir_index,
        &semantics.ctxt.types,
        target,
        Rc::clone(&diag),
        "test".to_string(),
    );
    let mir_module = mir_builder.build_module();
    if mir_builder.corrupted {
        return Err(common::messages(&diag));
    }
    Ok((mir_module, diag))
}

/// Runs the full Dollar pipeline (dollar verifier -> bytecode -> VM -> folder)
/// over a successfully-built MIR module and returns the VM's eval table.
fn dollar_pipeline_run(
    mir_module: &mut MIRModule,
    diag: &Rc<RefCell<Diagnostics>>,
) -> EvalResultTable {
    let mut dollar_verifier = DollarVerifier::new(mir_module, Rc::clone(diag));
    dollar_verifier.verify();
    assert!(
        !dollar_verifier.corrupted,
        "dollar verifier failed for: {:?}",
        common::messages(diag),
    );

    let mut bc_builder = BytecodeBuilder::new(mir_module, Rc::clone(diag));
    let bytecode = bc_builder.build();

    let mut vm = VM::new(&bytecode, Rc::clone(diag));
    let eval_table = vm.execute();

    let mut folder = Folder::new(Rc::clone(diag), mir_module, &eval_table);
    folder.fold();
    assert!(
        !folder.corrupted,
        "dollar fold failed for: {:?}",
        common::messages(diag),
    );

    eval_table
}

#[test]
fn dollar_scope_non_const_capture_is_a_clean_error() {
    let src = "variant Shape {\n  Circle(f32)\n}\n\
        func dollar_test(s: Shape) {\n\
        var x := $${ match s{ Shape.Circle(f) => f }; };\n\
    }";
    let err = match mir_build_checked(src) {
        Ok(_) => panic!("runtime capture must be rejected at MIR build"),
        Err(err) => err,
    };
    assert!(
        err.iter()
            .any(|m| m.contains("Cannot capture 's' into dollar scope")),
        "expected capture error, got: {err:?}",
    );
}

#[test]
fn dollar_scope_const_capture_folds_match_arms() {
    let src = r#"const var k := 7;
const var j := 99;
func dollar_a(){
  var x := $$ |k| {
    match k{
      7 => 1
      _ => 0
    };
  };
}
func dollar_b(){
  var y := $$ |j| {
    match j{
      7 => 1
      _ => 0
    };
  };
}
"#;
    let (mut mir_module, diag) = mir_build_checked(src)
        .unwrap_or_else(|err| panic!("const-capture dollar match should build: {err:?}"));

    let eval_table = dollar_pipeline_run(&mut mir_module, &diag);
    let mut folded: Vec<isize> = eval_table
        .results
        .values()
        .map(|v| match v {
            VMValue::Int(i) => *i,
            other => panic!("expected Int eval result, got {other:?}"),
        })
        .collect();
    folded.sort();
    assert_eq!(folded, vec![0, 1]);

    for func in mir_module.functions.values() {
        if let Some(body) = &func.body {
            for block in body.blocks.values() {
                assert!(
                    !block
                        .instructions
                        .iter()
                        .any(|i| matches!(i, MIRInstruction::DollarEval { .. })),
                    "DollarEval should be folded away in '{}'",
                    func.name,
                );
            }
        }
    }
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

/// Collects every statement id in `stmt`'s subtree, including if/while bodies
/// and any nested function definitions (the shapes the node indexer indexes).
fn collect_stmt_ids<'a>(stmt: &'a HirStmt, out: &mut Vec<unnc::lowering::NodeId>) {
    out.push(stmt.hir_id.clone());
    match &stmt.kind {
        HirStmtKind::HirIf {
            body,
            else_body,
            ..
        } => {
            for s in body {
                collect_stmt_ids(s, out);
            }
            if let Some(el) = else_body {
                for s in el {
                    collect_stmt_ids(s, out);
                }
            }
        }
        HirStmtKind::HirWhile { body, .. } => {
            for s in body {
                collect_stmt_ids(s, out);
            }
        }
        HirStmtKind::HirFunctionDef { body, .. } => {
            for s in body {
                collect_stmt_ids(s, out);
            }
        }
        HirStmtKind::HirContractDecl { functions, .. } => {
            for f in functions {
                collect_stmt_ids(f, out);
            }
        }
        _ => {}
    }
}

#[test]
fn node_index_round_trips_monomorphized_tree_without_collisions() {
    // The NodeIndex keys statements by `NodeId`. Monomorphized trees mix
    // original ids (root functions/structs, kept in place) with the fresh
    // ids allocated for appended instances — every statement must land in
    // the index exactly once, and `get` must return that exact statement.
    let (hir, _) = mono(
        "generics <T> { struct Pair { a: T, b: T } func identity(v: T): T { return v; } }\n\
         func main(): i32 {\n\
             var pr := .Pair<i32>{.a = 1i32, .b = 2i32};\n\
             var g := identity::<i32>(pr.a);\n\
             if g > 0i32 {\n\
                 var alt := identity::<i32>(g);\n\
                 return alt;\n\
             }\n\
             return g;\n\
         }\n",
    );

    let index = NodeIndex::build(&hir);

    // Templates are dropped from the unified tree; only main + instances.
    assert_eq!(index.roots.len(), 3, "roots = {:?}", index.roots);
    assert!(
        index.get(&index.roots[1]).is_some() && index.get(&index.roots[2]).is_some(),
        "appended instances must be reachable roots"
    );

    // Bijection: every statement in the tree is in the index, unique.
    let mut all_ids = Vec::new();
    for root in &hir {
        collect_stmt_ids(root, &mut all_ids);
    }
    assert_eq!(
        all_ids.len(),
        index.nodes.len(),
        "node ids in the tree must map 1:1 into the index (fresh-id collisions?)"
    );
    assert_eq!(
        all_ids.iter().collect::<std::collections::HashSet<_>>().len(),
        all_ids.len(),
        "duplicate statement ids in the monomorphized tree"
    );

    for stmt in &hir {
        assert_stmt_indexed(stmt, &index);
    }

    // Fresh ids live strictly above every original id: no overlap that could
    // let two statements share a key.
    let mut original_max = 0usize;
    let mut fresh_min = usize::MAX;
    for root in &hir {
        let name = match &root.kind {
            HirStmtKind::HirStructDecl { name, .. } | HirStmtKind::HirFunctionDef { name, .. } => Some(name.clone()),
            _ => None,
        };
        let id = root.hir_id.local;
        match name {
            Some(n) if n.starts_with("_U_") => fresh_min = fresh_min.min(id),
            Some(_) => original_max = original_max.max(id),
            None => original_max = original_max.max(id),
        }
    }
    assert!(
        fresh_min > original_max,
        "fresh instance ids ({fresh_min}) must exceed original ids ({original_max})"
    );
}

fn assert_stmt_indexed(stmt: &HirStmt, index: &NodeIndex) {
    match index.get(&stmt.hir_id) {
        Some(found) => assert_eq!(found.hir_id, stmt.hir_id),
        None => panic!("statement {:?} missing from node index", stmt.hir_id),
    }
    match &stmt.kind {
        HirStmtKind::HirIf {
            body,
            else_body,
            ..
        } => {
            for s in body {
                assert_stmt_indexed(s, index);
            }
            if let Some(el) = else_body {
                for s in el {
                    assert_stmt_indexed(s, index);
                }
            }
        }
        HirStmtKind::HirWhile { body, .. } => {
            for s in body {
                assert_stmt_indexed(s, index);
            }
        }
        HirStmtKind::HirFunctionDef { body, .. } => {
            for s in body {
                assert_stmt_indexed(s, index);
            }
        }
        HirStmtKind::HirContractDecl { functions, .. } => {
            for f in functions {
                assert_stmt_indexed(f, index);
            }
        }
        _ => {}
    }
}

/// Runs analyze → monomorphize → index → contract verification and returns
/// every diagnostic. Asserts analysis stays clean first, so a failure is
/// attributable to the contract verifier rather than an earlier phase.
fn mono_verify_contracts(src: &str) -> Vec<String> {
    let (mut semantics, diag) = analyze(src, &[]);
    if semantics.corrupted {
        panic!(
            "analysis should stay clean before contract verification for:\n{src}\ngot: {:?}",
            common::messages(&diag),
        );
    }
    let monomorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorphized_hir);
    semantics.verify_contracts(&hir_index, common::Rc::clone(&diag));
    common::messages(&diag)
}

#[test]
fn end_to_end_contract_satisfied_by_methods_builds_mir() {
    // The contract verifier consumes the *monomorphized* tree (root structs
    // keep their original contract-node ids, so the decl stays resolvable).
    // This proves the whole pipeline handles a satisfied contract: verification
    // passes and the mangled method `Point_get` reaches the MIR module.
    let mir = mono_e2e(
        "contract HasGet { func get(): i32 }\n\
         struct Point: HasGet { x: i32, y: i32 }\n\
         methods Point { func get(): i32 { return 42i32; } }\n\
         func main(): i32 { var p := .Point{.x = 1i32, .y = 2i32}; return 0i32; }\n",
    );
    assert!(
        mir.functions.iter().any(|(_, f)| f.name == "Point_get"),
        "expected Point_get in MIR module"
    );
}

#[test]
fn contract_missing_implementation_is_reported() {
    let msgs = mono_verify_contracts(
        "contract HasGet { func get(): i32 }\n\
         struct Point: HasGet { x: i32 }\n\
         func main(): i32 { var p := .Point{.x = 1i32}; return 0i32; }\n",
    );
    assert!(
        msgs.iter()
            .any(|m| m.contains("'Point' missing implementation of 'get'")),
        "expected missing-implementation report, got {msgs:?}",
    );
}

#[test]
fn contract_wrong_param_count_is_reported() {
    let msgs = mono_verify_contracts(
        "contract HasGet { func get(): i32 }\n\
         struct Point: HasGet { x: i32 }\n\
         methods Point { func get(n: i32): i32 { return n; } }\n\
         func main(): i32 { var p := .Point{.x = 1i32}; return 0i32; }\n",
    );
    assert!(
        msgs.iter()
            .any(|m| m.contains("wrong number of parameters")),
        "expected param-count report, got {msgs:?}",
    );
}

#[test]
fn contract_wrong_return_type_is_reported() {
    let msgs = mono_verify_contracts(
        "contract HasGet { func get(): i32 }\n\
         struct Point: HasGet { x: i32 }\n\
         methods Point { func get(): u64 { return 1u64; } }\n\
         func main(): i32 { var p := .Point{.x = 1i32}; return 0i32; }\n",
    );
    assert!(
        msgs.iter()
            .any(|m| m.contains("return type does not match contract")),
        "expected return-type report, got {msgs:?}",
    );
}

#[test]
fn free_function_wearing_method_name_is_reported_not_panicked() {
    // The impl lookup matches `Point_get` by name alone; a *free* function
    // with zero params reaches `verify_signature` with no `self` to skip. The
    // old `params[1..]` slice panicked on this — it must surface as a report.
    let msgs = mono_verify_contracts(
        "contract HasGet { func get(): i32 }\n\
         struct Point: HasGet { x: i32 }\n\
         func Point_get(): i32 { return 5i32; }\n\
         func main(): i32 { var p := .Point{.x = 1i32}; return 0i32; }\n",
    );
    assert!(
        msgs.iter()
            .any(|m| m.contains("without a 'self' receiver")),
        "expected receiver report instead of a panic, got {msgs:?}",
    );
}

#[test]
fn generic_struct_instance_with_contract_is_skipped_not_reported() {
    // Contract-verification boundary for monomorphized generic instances:
    // `_U_Pair_i32` carries contract usages with fresh ids the name table
    // never sees, so the verifier skips it. There is no way to write the
    // `_U_Pair_i32_get` impl it would otherwise demand (generic methods don't
    // exist), so clean here is the guarantee: the instance duplicate must
    // never turn into a false 'missing implementation' error.
    let msgs = mono_verify_contracts(
        "contract HasGet { func get(): i32 }\n\
         generics <T> { struct Pair: HasGet { a: T, b: T } }\n\
         func main(): i32 { var pr := .Pair<i32>{.a = 1i32, .b = 2i32}; var g := pr.b; return g; }\n",
    );
    assert!(msgs.is_empty(), "expected no reports, got {msgs:?}");
}

/// Runs analyze → monomorphize → index → serialize and returns the exported
/// stub, the same shape `write_stub_for_module` writes to disk (minus bincode).
fn serialize_stub(src: &str) -> unnc::serializer::ExportStub {
    let (mut semantics, diag) = analyze(src, &[]);
    if semantics.corrupted {
        panic!(
            "analysis should succeed for:\n{src}\ngot: {:?}",
            common::messages(&diag),
        );
    }
    let monomorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorphized_hir);
    unnc::serializer::Serializer::new("foo".to_string(), &semantics.ctxt, &hir_index, diag).serialize()
}

#[test]
fn exposed_generic_instances_serialize_as_concrete_symbols() {
    // The serializer consumes the monomorphized tree, so an `expose`-d
    // generic template surfaces only through its concrete instances. Both the
    // func and struct instance must export fully concrete signatures — the
    // struct instance must NOT carry the template's generic param list (that
    // stale `[T]` used to leak into the stub and name it `Pair<T>`).
    let stub = serialize_stub(
        "generics <T> { expose func identity(v: T): T { return v; } expose struct Pair { a: T, b: T } }\n\
         func main(): i32 { var x := identity::<i32>(5); var pr := .Pair<i32>{.a = 1i32, .b = 2i32}; return 0i32; }\n",
    );

    let func = stub
        .exposed_symbols
        .get("foo__U_identity_i32")
        .unwrap_or_else(|| panic!("missing foo__U_identity_i32 in {:?}", stub.exposed_symbols.keys().collect::<Vec<_>>()));
    match &func.kind {
        unnc::semantics::ResolvedTypeKind::Func {
            params, ret_type, ..
        } => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0].kind, unnc::semantics::ResolvedTypeKind::I32));
            assert!(matches!(ret_type.kind, unnc::semantics::ResolvedTypeKind::I32));
        }
        other => panic!("expected func instance, got {other:?}"),
    }

    let st = stub
        .exposed_symbols
        .get("foo__U_Pair_i32")
        .unwrap_or_else(|| panic!("missing foo__U_Pair_i32 in {:?}", stub.exposed_symbols.keys().collect::<Vec<_>>()));
    match &st.kind {
        unnc::semantics::ResolvedTypeKind::Struct {
            name,
            gen_type_params,
            members,
        } => {
            assert_eq!(name, "_U_Pair_i32");
            assert!(
                gen_type_params.is_empty(),
                "concrete instance must not re-export a generic param, got {gen_type_params:?}"
            );
            assert_eq!(st.name, "_U_Pair_i32");
            assert_eq!(members.len(), 2);
            assert!(
                matches!(members[1].1.kind, unnc::semantics::ResolvedTypeKind::I32),
                "substituted member expected: {:?}",
                members[1].1.kind
            );
        }
        other => panic!("expected struct instance, got {other:?}"),
    }
}

#[test]
fn uninstantiated_exposed_template_exports_nothing() {
    // Boundary, documented: only *instances* cross module boundaries — an
    // expose-d template that is never instantiated is dropped from the unified
    // tree and produces no stub symbol (cross-module *generic* imports are not
    // part of the import model yet).
    let stub = serialize_stub(
        "generics <T> { expose func identity(v: T): T { return v; } }\n\
         func main(): i32 { return 0i32; }\n",
    );
    assert!(stub.exposed_symbols.is_empty(), "{:?}", stub.exposed_symbols);
}

fn constant_to_i128(c: &ConstantValue) -> i128 {
    match c {
        ConstantValue::I8(v) => *v as i128,
        ConstantValue::U8(v) => *v as i128,
        ConstantValue::I16(v) => *v as i128,
        ConstantValue::U16(v) => *v as i128,
        ConstantValue::I32(v) => *v as i128,
        ConstantValue::U32(v) => *v as i128,
        ConstantValue::I64(v) => *v as i128,
        ConstantValue::U64(v) => *v as i128,
        ConstantValue::Int(v) => *v as i128,
        ConstantValue::UInt(v) => *v as i128,
        ConstantValue::I128(v) => *v as i128,
        ConstantValue::U128(v) => *v as i128,
        ConstantValue::Bool(v) => *v as i128,
        other => panic!("unexpected constant {other:?}"),
    }
}

/// Every integer constant an arm's test block compares the scrutinee (or its
/// tag) against, sorted. Non-empty for any match built from the VM grammar.
fn cmp_constants(f: &unnc::mir::MIRFn) -> Vec<i128> {
    let body = f.body.as_ref().expect("function must have a body");
    let mut out = Vec::new();
    for bb in body.blocks.values() {
        for inst in &bb.instructions {
            if let MIRInstruction::Compare { rhs, .. } = inst {
                if let MIRValue::Constant(c) = rhs {
                    out.push(constant_to_i128(c));
                }
            }
        }
    }
    out.sort_unstable();
    out
}

#[test]
fn end_to_end_scalar_match_builds_mir_with_arm_checks() {
    // A scalar match with distinct literal arms must compare the scrutinee
    // against each arm's constant (0 and 5 — non-coincidental), branch per
    // arm, and merge every arm body into one function-wide result register.
    let mir = mono_e2e(
        "func pick(v: i32): i32 { return match v { 0 => 10i32, 5 => 50i32, _ => 0i32 }; }\n\
         func main(): i32 { var r := pick(3); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "pick")
        .expect("pick should reach MIR");
    assert_eq!(cmp_constants(f), vec![0, 5]);
    // match + wildcard arm + unreachable block + merge, all distinct
    let body = f.body.as_ref().unwrap();
    assert!(body.blocks.len() >= 10, "match should emit test/entry/body blocks");
}

#[test]
fn end_to_end_match_with_guard_builds_mir() {
    // A bind-all arm guarded by a real condition is emitted as a branch: the
    // first arm tests 0 and the guard tests 3 (both reach MIR as compares).
    let mir = mono_e2e(
        "func classify(v: i32): i32 {\n\
             return match v {\n\
                 0 => 10i32,\n\
                 n if n > 3 => n,\n\
                 _ => 0i32,\n\
             };\n\
         }\n\
         func main(): i32 { var r := classify(7); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "classify")
        .expect("classify should reach MIR");
    assert_eq!(cmp_constants(f), vec![0, 3]);
    let body = f.body.as_ref().unwrap();
    assert!(
        body.blocks
            .values()
            .any(|bb| matches!(bb.terminator, unnc::mir::Terminator::Branch { .. })),
        "guarded arm must branch on the guard condition"
    );
}

#[test]
fn end_to_end_or_pattern_match_builds_mir() {
    // `1 | 2` lowers to a disjunction of the two arm checks; both constants
    // must reach MIR compares.
    let mir = mono_e2e(
        "func classify(v: i32): i32 { return match v { 1 | 2 => 100i32, _ => 0i32 }; }\n\
         func main(): i32 { var r := classify(2); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "classify")
        .expect("classify should reach MIR");
    assert_eq!(cmp_constants(f), vec![1, 2]);
}

#[test]
fn end_to_end_variant_match_compares_tags_and_binds_payloads() {
    // Circle gets tag 0, Square tag 1. The match must compare the loaded tag
    // against both discriminants and bind Circle's payload into the arm scope.
    let mir = mono_e2e(
        "variant Shape { Circle(i8, i64), Square }\n\
         func area(s: Shape): i8 {\n\
             return match s {\n\
                 Shape.Circle(r, _) => r,\n\
                 Shape.Square => 2i8,\n\
             };\n\
         }\n\
         func main(): i32 { var s := Shape.Circle(3, 9); var a := area(s); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "area")
        .expect("area should reach MIR");
    assert_eq!(cmp_constants(f), vec![0, 1]);
}

#[test]
fn end_to_end_enum_match_compares_member_values() {
    // Similarly for enums: RED and GREEN must both be tested against the
    // scrutinee (their underlying value), and coverage is exhaustive.
    let mir = mono_e2e(
        "enum Color: u8 { RED, GREEN }\n\
         func code(c: Color): i32 {\n\
             return match c {\n\
                 Color.RED => 1i32,\n\
                 Color.GREEN => 2i32,\n\
             };\n\
         }\n\
         func main(): i32 { var c := Color.RED; var k := code(c); return k; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "code")
        .expect("code should reach MIR");
    assert_eq!(cmp_constants(f), vec![0, 1]);
}

#[test]
fn end_to_end_tuple_pattern_binds_elements_into_scope() {
    // A tuple pattern binds element 0 into the arm scope; returning it must
    // load from the GEP reached into the scrutinee.
    let mir = mono_e2e(
        "func first(t: (i32, i64)): i32 {\n\
             return match t {\n\
                 (a, _) => a,\n\
             };\n\
         }\n\
         func main(): i32 { var tup := .(7i32, 8i64); var r := first(tup); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "first")
        .expect("first should reach MIR");
    let body = f.body.as_ref().unwrap();
    assert!(
        body.blocks
            .values()
            .flat_map(|bb| &bb.instructions)
            .any(|i| matches!(i, MIRInstruction::GetElementPtr { .. })),
        "binding a tuple element must GEP into the scrutinee"
    );
}

#[test]
fn end_to_end_struct_pattern_binds_named_fields() {
    // Bind the SECOND field (i64, non-index-0) so an index-off-by-one bug in
    // the GEP would produce a wrong type/offset and fail the load.
    let mir = mono_e2e(
        "struct Point { x: i32, y: i64 }\n\
         func py(p: Point): i64 {\n\
             return match p {\n\
                 .Point{ .x = _, .y = b } => b,\n\
             };\n\
         }\n\
         func main(): i32 { var p := .Point{.x = 1i32, .y = 2i64}; var b := py(p); return 0i32; }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "py")
        .expect("py should reach MIR");
    let body = f.body.as_ref().unwrap();
    assert!(
        body.blocks
            .values()
            .flat_map(|bb| &bb.instructions)
            .any(|i| matches!(i, MIRInstruction::GetElementPtr { .. })),
        "binding a struct field must GEP into the scrutinee"
    );
}

#[test]
fn end_to_end_match_as_value_and_block_bodies_build_mir() {
    // A match used as an rvalue (`var v := match ...`) and block-expression
    // arm bodies (with a trailing value) must both lower to MIR.
    let mir = mono_e2e(
        "func main(): i32 {\n\
             var v := match 2 { 1 => 100i32, 2 => { var t := 50i32; t * 2; }, _ => 0i32 };\n\
             return v;\n\
         }\n",
    );
    let (_, f) = mir
        .functions
        .iter()
        .find(|(_, f)| f.name == "main")
        .expect("main should reach MIR");
    assert_eq!(cmp_constants(f), vec![1, 2]);
    let body = f.body.as_ref().unwrap();
    assert!(
        body.blocks
            .values()
            .flat_map(|bb| &bb.instructions)
            .any(|i| matches!(i, MIRInstruction::Alloca { .. })),
        "block arm body's var must alloca"
    );
}