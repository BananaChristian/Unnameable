use std::{collections::HashMap, rc::Rc};

use unnc::lowering::NodeId;
use unnc::semantics::Semantics;

mod common;

use common::{analyze, messages, pipe_imported};

fn map(pairs: &[(usize, usize)]) -> HashMap<NodeId, NodeId> {
    pairs
        .iter()
        .map(|(a, b)| {
            (
                NodeId {
                    local: *a,
                    external: 0,
                },
                NodeId {
                    local: *b,
                    external: 0,
                },
            )
        })
        .collect()
}

fn resolved_names(semantics: &Semantics) -> HashMap<NodeId, NodeId> {
    semantics.ctxt.names.resolved.clone()
}

fn assert_clean(
    (semantics, diag): (Semantics<'_>, Rc<std::cell::RefCell<common::Diagnostics>>),
) -> Semantics<'_> {
    assert!(
        !semantics.corrupted,
        "unexpected corruption: {:?}",
        messages(&diag)
    );
    assert!(
        messages(&diag).is_empty(),
        "unexpected errors: {:?}",
        messages(&diag)
    );
    semantics
}

// basic decl self-resolution
#[test]
fn var_decl_self_resolves() {
    let s = assert_clean(analyze("var x := 1;", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1)]));
}

#[test]
fn var_init_identifier_resolves_to_decl() {
    let s = assert_clean(analyze("var x := 1;\nvar y := x;", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (2, 1), (3, 3)]));
}

#[test]
fn struct_decl_self_resolves_and_fields_self_register() {
    let s = assert_clean(analyze("struct Point { a: i32, b: i32 }", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (3, 3), (4, 4)]));
}

#[test]
fn func_param_usage_resolves_to_param_decl() {
    let s = assert_clean(analyze("func f(a: i32): i32 { return a; }", &[]));
    assert_eq!(resolved_names(&s), map(&[(3, 1), (5, 5)]));
}

#[test]
fn func_local_var_usage_resolves() {
    let s = assert_clean(analyze("func f(): i32 { var y := 5; return y; }", &[]));
    assert_eq!(resolved_names(&s), map(&[(2, 2), (3, 2), (5, 5)]));
}

#[test]
fn global_visible_inside_func_body() {
    let s = assert_clean(analyze("var x := 1;\nfunc f(): i32 { return x; }", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (3, 1), (5, 5)]));
}

#[test]
fn enum_decl_self_resolves() {
    let s = assert_clean(analyze("enum Color: u8 { RED, GREEN }", &[]));
    assert_eq!(resolved_names(&s), map(&[(3, 3)]));
}

#[test]
fn variant_members_self_register() {
    let s = assert_clean(analyze("variant Shape { Circle(i8, i64), Square }", &[]));
    assert_eq!(resolved_names(&s), map(&[(2, 2), (3, 3), (4, 4)]));
}

#[test]
fn contract_func_symbol_self_resolves() {
    let s = assert_clean(analyze("contract HasX { func get(): i32 }", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1)]));
}

#[test]
fn two_contracts_allow_same_func_name() {
    let s = assert_clean(analyze(
        "contract HasX { func get(): i32 }\ncontract HasY { func get(): i32 }",
        &[],
    ));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (4, 4)]));
}

// diagnostics on unresolved references
#[test]
fn undeclared_identifier_reported() {
    let (s, diag) = analyze("x + 1;", &[]);
    assert!(s.corrupted);
    assert_eq!(
        messages(&diag),
        vec![
            "'x' is not declared",
            "Left operand of arithmetic operation must be numeric"
        ]
    );
    assert!(resolved_names(&s).is_empty());
}

#[test]
fn undeclared_local_in_func_reported() {
    let (s, diag) = analyze("func f(): i32 { var y := x; return y; }", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'x' is not declared"]);
    assert_eq!(resolved_names(&s), map(&[(2, 2), (3, 2), (5, 5)]));
}

#[test]
fn undeclared_param_type_reported() {
    let (s, diag) = analyze("func f(p: MissingType): i32 { return 0; }", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'MissingType' is not declared"]);
    assert_eq!(resolved_names(&s), map(&[(5, 5)]));
}

// duplicate definitions
#[test]
fn duplicate_var_definition_rejected() {
    let (s, diag) = analyze("var x := 1;\nvar x := 2;", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'x' already defined in this scope"]);
    assert_eq!(resolved_names(&s), map(&[(1, 1), (3, 1)]));
}

#[test]
fn duplicate_func_definition_rejected() {
    let (s, diag) = analyze(
        "func f(): i32 { return 1; }\nfunc f(): i32 { return 2; }",
        &[],
    );
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'f' already defined in this scope"]);
    assert_eq!(resolved_names(&s), map(&[(3, 3), (7, 3)]));
}

#[test]
fn func_param_and_body_var_same_scope_conflict() {
    let (s, diag) = analyze("func f(x: i32): i32 { var x := 5; return x; }", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'x' already defined in this scope"]);
}

// scoping
#[test]
fn if_body_reuses_enclosing_scope() {
    let s = assert_clean(analyze("var x := 1;\nif x > 0 { var y := 2; }", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (2, 1), (6, 6)]));
}

#[test]
fn if_body_cannot_shadow_outer_var() {
    let (s, diag) = analyze("var x := 1;\nif x > 0 { var x := 2; }", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'x' already defined in this scope"]);
    assert_eq!(resolved_names(&s), map(&[(1, 1), (2, 1), (6, 1)]));
}

#[test]
fn while_body_cannot_shadow_outer_var() {
    let (s, diag) = analyze("var x := 1;\nwhile x > 0 { var x := 2; }", &[]);
    assert!(s.corrupted);
    assert_eq!(messages(&diag), vec!["'x' already defined in this scope"]);
    assert_eq!(resolved_names(&s), map(&[(1, 1), (2, 1), (6, 1)]));
}

#[test]
fn while_body_var_is_fine_when_no_conflict() {
    let s = assert_clean(analyze("while true { var w := 1; }", &[]));
    assert_eq!(resolved_names(&s), map(&[(2, 2)]));
}

#[test]
fn func_param_shadows_outer_var() {
    let s = assert_clean(analyze(
        "var x := 1;\nfunc f(x: i32): i32 { return x; }",
        &[],
    ));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (5, 3), (7, 7)]));
}

#[test]
fn if_body_new_var_does_not_escape() {
    let s = assert_clean(analyze(
        "func f(): i32 { if true { var y := 2; } return 0; }",
        &[],
    ));
    assert_eq!(resolved_names(&s), map(&[(3, 3), (7, 7)]));
}

// usage resolution across constructs
#[test]
fn struct_usage_in_func_param_and_field_access() {
    let s = assert_clean(analyze(
        "struct Point { a: i32, b: i32 }\nfunc f(p: Point): i32 { return p.a; }",
        &[],
    ));
    assert_eq!(
        resolved_names(&s),
        map(&[(1, 1), (3, 3), (4, 4), (5, 4), (8, 6), (9, 1), (12, 12)])
    );
}

#[test]
fn call_callee_resolves_to_func_decl() {
    let s = assert_clean(analyze(
        "func g(): i32 { return 5; }\nfunc f(): i32 { return g(); }",
        &[],
    ));
    assert_eq!(resolved_names(&s), map(&[(3, 3), (5, 3), (8, 8)]));
}

#[test]
fn sizeof_type_usage_resolves_to_struct() {
    let s = assert_clean(analyze("struct Foo { a: i32 }\nvar s := sizeof<Foo>;", &[]));
    assert_eq!(resolved_names(&s), map(&[(1, 1), (2, 2), (3, 2), (5, 5)]));
}

// alias
#[test]
fn alias_target_is_not_resolved_on_its_own() {
    let s = assert_clean(analyze("alias i32 as Int", &[]));
    assert!(resolved_names(&s).is_empty());
}

#[test]
fn alias_usage_in_annotation_resolves_to_alias_decl() {
    let s = assert_clean(analyze("alias i32 as Int\nvar Int y := 5;", &[]));
    assert_eq!(resolved_names(&s), map(&[(2, 1), (4, 4)]));
}

#[test]
fn dollar_scope_capture_params_resolve_to_outer_consts() {
    let s = assert_clean(analyze(
        "const var a := 5;\nconst var b := 3;\nvar g := $$|a, b|{ a + b; };",
        &[],
    ));
    // the capture list identifiers and their in-body uses resolve to the outer
    // const declarations (the scope body sees the captured values)
    assert_eq!(
        resolved_names(&s),
        map(&[(1, 1), (3, 3), (4, 1), (5, 3), (6, 1), (7, 3), (10, 10)])
    );
}

#[test]
fn dollar_scope_capture_of_undeclared_name_is_rejected() {
    let (s, diag) = analyze("var g := $$|a, b|{ a + b; };", &[]);
    assert!(s.corrupted);
    let msgs = messages(&diag);
    assert!(
        msgs.iter().any(|m| m == "'a' is not declared"),
        "got: {msgs:?}"
    );
}

#[test]
fn imported_symbol_usage_resolves_to_external_decl() {
    let stub_path = std::env::temp_dir().join(format!("unnc_res_foo_{}.bin", std::process::id()));
    common::write_stub_for_module("foo", "expose struct Point { x: i32, y: i32 }", &stub_path);

    let (src, importer, diag) = pipe_imported(
        "import foo::Point\nfunc f(p: Point): i32 { return 0; }",
        &[stub_path.to_str().unwrap()],
    );
    let import_decl = importer
        .resolve_imported_name("Point")
        .expect("imported Point should resolve from stub");

    let target = Box::leak(Box::new(common::TargetSpec::new(None, None, None, None)));
    let mut semantics = common::Semantics::new(src, target);
    semantics.analyze(diag.clone(), &importer);
    assert!(!semantics.corrupted, "got: {:?}", messages(&diag));

    let mapped_to_import = resolved_names(&semantics)
        .values()
        .any(|decl| *decl == import_decl);
    assert!(
        mapped_to_import,
        "no usage resolved to import decl {import_decl:?}"
    );
    std::fs::remove_file(&stub_path).ok();
}
