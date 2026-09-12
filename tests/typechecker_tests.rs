use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

use unnc::lowering::NodeId;
use unnc::semantics::{ResolvedTypeKind, Semantics};

mod common;

use common::{analyze, messages};

type Row = (String, usize, usize, usize); // name, type_id.0, size, alignment

fn tab(s: &Semantics) -> BTreeMap<usize, Row> {
    s.ctxt
        .types
        .types
        .iter()
        .map(|(id, ti)| {
            (
                id.local,
                (
                    ti.name.clone(),
                    ti.type_id.0,
                    ti.layout.size,
                    ti.layout.alignment,
                ),
            )
        })
        .collect()
}

fn spans(s: &Semantics) -> BTreeMap<usize, (usize, usize)> {
    s.ctxt
        .types
        .types
        .iter()
        .map(|(id, ti)| (id.local, (ti.span.start, ti.span.end)))
        .collect()
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

fn assert_errors<'a>(
    (semantics, diag): (Semantics<'a>, Rc<std::cell::RefCell<common::Diagnostics>>),
    expected: &[&str],
) -> Semantics<'a> {
    let got = messages(&diag);
    assert_eq!(got, expected.to_vec(), "diagnostics mismatch");
    semantics
}

fn assert_entry(sp: &Semantics, id: usize, name: &str, tid: usize, size: usize, align: usize) {
    let t = tab(sp);
    let (n, t_, s, a) = t
        .get(&id)
        .unwrap_or_else(|| panic!("no type entry for id {id}, table: {t:?}"));
    assert_eq!(
        (n.as_str(), *t_, *s, *a),
        (name, tid, size, align),
        "entry {id} mismatch"
    );
}

fn assert_span(sp: &Semantics, id: usize, start: usize, end: usize) {
    let s = spans(sp);
    match s.get(&id) {
        Some((a, b)) => assert_eq!((*a, *b), (start, end), "span mismatch for id {id}"),
        None => panic!("no span for id {id}: {s:?}"),
    }
}

fn tid_by_name(sp: &Semantics, name: &str) -> usize {
    let mut found: HashSet<usize> = HashSet::new();
    for ti in sp.ctxt.types.types.values() {
        if ti.name == name {
            found.insert(ti.type_id.0);
        }
    }
    assert_eq!(
        found.len(),
        1,
        "expected exactly one TypeId for '{name}', got {found:?}"
    );
    *found.iter().next().unwrap()
}

// literals, TypeId ordering, var-statement span quirks
#[test]
fn literal_typing_order_and_var_spans() {
    let s = assert_clean(analyze(
        "var a := 42;\nvar i64 b := 42;\nvar c := 3.5;\nvar f32 d := 100;\nvar e := true;\nvar f := 'x';\nvar s := \"hi\";",
        &[],
    ));
    // stmt stubs are Unit (id 0), check_var's probe unknown is id 1, literals follow
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 2, "i64", 3, 8, 8);
    assert_entry(&s, 3, "i64", 3, 8, 8); // coerced literal keeps the ANNOTATION span
    assert_entry(&s, 4, "i64", 3, 8, 8);
    assert_entry(&s, 5, "f64", 4, 8, 8);
    assert_entry(&s, 7, "f32", 5, 4, 4);
    assert_entry(&s, 10, "bool", 6, 1, 1);
    assert_entry(&s, 12, "char8", 7, 1, 1);
    assert_entry(&s, 14, "str", 8, 8, 8);

    // unannotated var stmt entry takes the INIT literal's span
    assert_span(&s, 0, 9, 11);
    assert_span(&s, 1, 9, 11);
    // annotated var: annotation node, coerced init, and stmt all carry annotation span
    assert_span(&s, 2, 17, 20);
    assert_span(&s, 3, 17, 20);
    assert_span(&s, 4, 17, 20);
}

#[test]
fn annotated_init_type_mismatch_keeps_annotation_entry() {
    let s = assert_errors(
        analyze("var x := 1i64;\nvar i32 y := x;", &[]),
        &["Type mismatch between 'i32' and 'i64'"],
    );
    assert_entry(&s, 4, "i32", 3, 4, 4);
    assert_span(&s, 4, 19, 22);
    assert_entry(&s, 3, "i64", 2, 8, 8);
    assert_span(&s, 3, 9, 13);
}

// structs: layout computed once by the layout engine
#[test]
fn struct_layout_includes_padding_mixed_sizes() {
    let s = assert_clean(analyze("struct Mixed { a: u8, b: u64, c: i16 }", &[]));
    // a@0(1b), pad to 8, b@8(8b), c@16(2b), pad tail -> 24 bytes, align 8
    assert_entry(&s, 6, "Mixed", 0, 24, 8);
    assert_entry(&s, 0, "u8", 1, 1, 1);
    assert_entry(&s, 2, "u64", 2, 8, 8);
    assert_entry(&s, 4, "i16", 3, 2, 2);

    match &s
        .ctxt
        .types
        .types
        .get(&NodeId {
            local: 6,
            external: 0,
        })
        .unwrap()
        .kind
    {
        ResolvedTypeKind::Struct { name, members, .. } => {
            assert_eq!(name, "Mixed");
            assert_eq!(members.len(), 3);
            assert_eq!(members[0].0, "a");
            assert_eq!(members[1].0, "b");
            assert_eq!(members[2].0, "c");
        }
        other => panic!("expected Struct kind, got {other:?}"),
    }
}

#[test]
fn struct_init_coerces_float_literals_to_fields() {
    let s = assert_clean(analyze(
        "struct Pt { x: f32, y: f32 }\nvar p := .Pt{.x = 1.0, .y = 2.0};",
        &[],
    ));
    assert_entry(&s, 4, "Pt", 0, 8, 4); // struct decl stmt
    assert_entry(&s, 5, "Pt", 0, 8, 4); // var stmt
}

#[test]
fn cyclic_struct_dependency_reported_with_empty_layout() {
    let s = assert_errors(
        analyze("struct Cyclic { x: Cyclic }", &[]),
        &["Detected cyclic dependency"],
    );
    // registry still returns a TypeId (0 = first issued), layout forced empty
    assert_entry(&s, 0, "Cyclic", 0, 0, 0);
    assert_entry(&s, 1, "Cyclic", 0, 0, 0);
}

// enums, variants
#[test]
fn enum_layout_matches_underlying_type() {
    let s = assert_clean(analyze("enum Color: u8 { RED, GREEN }", &[]));
    assert_eq!(tid_by_name(&s, "Color"), 3);
    assert_entry(&s, 3, "Color", 3, 1, 1);
    assert_entry(&s, 0, "u8", 1, 1, 1);

    match &s
        .ctxt
        .types
        .types
        .get(&NodeId {
            local: 3,
            external: 0,
        })
        .unwrap()
        .kind
    {
        ResolvedTypeKind::Enum {
            name,
            underlying,
            members,
        } => {
            assert_eq!(name, "Color");
            assert_eq!(underlying.kind, ResolvedTypeKind::U8);
            assert_eq!(members.len(), 2);
        }
        other => panic!("expected Enum kind, got {other:?}"),
    }
}

#[test]
fn variant_layout_is_tag_plus_max_payload() {
    let s = assert_clean(analyze(
        "variant Shape { Circle(i8, i64), Square }\nvar w := Shape.Circle(1, 2);",
        &[],
    ));
    assert_eq!(tid_by_name(&s, "Shape"), 4);
    assert_entry(&s, 4, "Shape", 4, 24, 8); // tag 4 + arm_offset 16 + payload i64, align 8

    match &s
        .ctxt
        .types
        .types
        .get(&NodeId {
            local: 4,
            external: 0,
        })
        .unwrap()
        .kind
    {
        ResolvedTypeKind::Variant { name, arms, .. } => {
            assert_eq!(name, "Shape");
            assert_eq!(arms.len(), 2);
            assert_eq!(arms[0].0, "Circle");
            assert_eq!(arms[0].3.len(), 2); // payload types i8, i64
            assert_eq!(arms[1].0, "Square");
            assert_eq!(arms[1].3.len(), 0);
        }
        other => panic!("expected Variant kind, got {other:?}"),
    }
}

// arrays, tuples, sizeof
#[test]
fn array_literal_infers_inner_from_unsuffixed_ints() {
    let s = assert_clean(analyze(
        "var l3 := [1, 2, 3];\nvar e3 := sizeof<isize>;",
        &[],
    ));
    assert_entry(&s, 3, "arr[isize,3]", 3, 24, 8);
    assert_span(&s, 3, 10, 19);
    // sizeof yields usize
    assert_entry(&s, 6, "usize", 4, 8, 8);
    assert_span(&s, 6, 31, 45);
}

#[test]
fn tuple_literal_type_and_scalar_access_error() {
    let s = assert_errors(
        analyze("var t2 := .(1, 2, 3);\nvar t3 := t2.0.1;", &[]),
        &["Cannot carry out an access operation on type 'isize'"],
    );
    assert_eq!(tid_by_name(&s, "(isize, isize, isize)"), 3);
    // (1,2,3) -> 24 bytes, align 8
    let mut hits = 0;
    for ti in s.ctxt.types.types.values() {
        if ti.name == "(isize, isize, isize)" {
            hits += 1;
            assert_eq!((ti.layout.size, ti.layout.alignment), (24, 8));
        }
    }
    assert!(hits > 0);
}

// operators: bitwise keyword vs logical, arithmetic operand rules
#[test]
fn bitwise_keywords_require_integer_operands_logical_and_is_distinct() {
    let s = assert_errors(
        analyze("var lg := true and false;\nvar j := true && false;", &[]),
        &["Bitwise operators require integer operands but got bool and bool"],
    );
    // failed bitwise expr -> Unknown entries; successful logical && -> bool
    assert_entry(&s, 2, "unknown", 1, 0, 0);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
    assert_entry(&s, 6, "bool", 2, 1, 1);
    assert_entry(&s, 7, "bool", 2, 1, 1);
}

#[test]
fn right_arithmetic_operand_must_be_numeric() {
    let s = assert_errors(
        analyze("var a := 1 + true;", &[]),
        &["Right operand of arithmetic operation must be numeric"],
    );
    assert_entry(&s, 2, "unknown", 1, 0, 0);
}

// control flow: nested statements type correctly, top-level stmts are Unit
#[test]
fn nested_assignments_type_while_and_if_stmts_are_unit() {
    let s = assert_clean(analyze(
        "var i := 0;\nwhile i < 3 { i = i + 1; }\nvar cmp := 1 < 2;\nif cmp { var z := 0; } else { var z2 := 1; }",
        &[],
    ));
    // top-level while stmt keeps the declare-stub Unit, span covers whole stmt
    assert_entry(&s, 10, "()", 0, 0, 0);
    assert_span(&s, 10, 12, 42);
    // top-level if stmt likewise
    assert_entry(&s, 20, "()", 0, 0, 0);
    assert_span(&s, 20, 57, 101);
    // nested assignment `i = i + 1` resolves to isize via the var decl
    assert_entry(&s, 4, "bool", 3, 1, 1); // i < 3
    assert_entry(&s, 13, "bool", 3, 1, 1); // 1 < 2
}

#[test]
fn top_level_assignments_are_type_checked() {
    let s = assert_clean(analyze(
        "var an0 := 1;\nan0 = 5;\nvar ai := 1;\nai += 2;\nai -= 1;",
        &[],
    ));
    // top-level assignment/compound-assign expressions are fully type-checked.
    // Assignment nodes keep the left operand's type; spans follow the
    // pre-existing convention that coerce_ty/look_up_declared_type carry the
    // declaration's span into the usage node.
    assert_span(&s, 0, 11, 12);
    assert_span(&s, 1, 11, 12);
    assert_span(&s, 4, 11, 12);
    assert_span(&s, 5, 33, 34);
    assert_span(&s, 6, 33, 34);
    assert_span(&s, 9, 33, 34);
    assert_span(&s, 12, 33, 34);
    assert_entry(&s, 4, "isize", 2, 8, 8);
    assert_entry(&s, 9, "isize", 2, 8, 8);
    assert_entry(&s, 12, "isize", 2, 8, 8);
    // the var decls themselves are typed normally
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 5, "isize", 2, 8, 8);
    assert_entry(&s, 6, "isize", 2, 8, 8);
}

// functions
#[test]
fn func_entries_param_ret_and_nested_var_typing() {
    let s = assert_clean(analyze(
        "func h(a: i32): i32 { var k := 1; k = k + 1; return k; }",
        &[],
    ));
    assert_entry(&s, 0, "i32", 1, 4, 4); // param decl
    assert_entry(&s, 2, "i32", 1, 4, 4); // ret type
    assert_entry(&s, 12, "func(i32) : i32", 2, 8, 8);
    assert_span(&s, 12, 0, 56);
    assert_entry(&s, 4, "isize", 4, 8, 8); // var k := 1
    assert_span(&s, 4, 31, 32);
}

#[test]
fn return_and_call_coerce_unsuffixed_literals_silently() {
    let s = assert_clean(analyze(
        "func bad(): f32 { return 0; }\nfunc good(): i32 { return 0; }\nvar ok := bad();\nvar ok2 := good();",
        &[],
    ));
    assert_entry(&s, 3, "func() : f32", 2, 8, 8);
    assert_entry(&s, 7, "func() : i32", 5, 8, 8);
    assert_span(&s, 7, 30, 64);
    // call results are the declared ret types
    assert_entry(&s, 10, "f32", 1, 4, 4);
    assert_entry(&s, 13, "i32", 4, 4, 4);
}

#[test]
fn pointer_param_rejects_address_of_isize() {
    let s = assert_errors(
        analyze(
            "func g(p: ptr<i32>): i32 { return ^p; }\nvar v2 := 42;\nvar r2 := g(@v2);",
            &[],
        ),
        &["Type mismatch between 'ptr<isize>' and 'ptr<i32>'"],
    );
    assert_entry(&s, 1, "ptr<i32>", 2, 8, 8);
    assert_span(&s, 1, 10, 17);
    assert_entry(&s, 7, "func(ptr<i32>) : i32", 3, 8, 8);
    // @v2 yields ptr<isize>: pointers to differing inner types don't unify
    assert_entry(&s, 12, "ptr<isize>", 6, 8, 8);
}

// call arguments
#[test]
fn call_arg_count_mismatch() {
    let s = assert_errors(
        analyze(
            "func f3(a: i32, b: i32): i32 { return a; }\nvar q := f3(1);",
            &[],
        ),
        &["Invalid argument count expected '2' but got '1'"],
    );
    assert_entry(&s, 7, "func(i32, i32) : i32", 2, 8, 8);
}

#[test]
fn call_arg_type_mismatch() {
    let _s = assert_errors(
        analyze(
            "func f4(a: i32, b: i32): i32 { return a; }\nvar q2 := f4(1, true);",
            &[],
        ),
        &["Type mismatch between 'bool' and 'i32'"],
    );
}

#[test]
fn default_param_literal_is_coerced_and_calls_can_omit_it() {
    let s = assert_clean(analyze(
        "func g(a: i32 := 7): i32 { return a; }\nvar r2 := g();\nvar r3 := g(3);",
        &[],
    ));
    // g() no longer errors: the default makes the call arity 0..=1, and the
    // unsuffixed default `7` is coerced to the i32 annotation
    assert_eq!(tid_by_name(&s, "func(i32) : i32"), 3);
    assert_entry(&s, 1, "i32", 1, 4, 4); // default literal coerced to i32
}

#[test]
fn default_param_mismatch_is_reported_once() {
    let s = assert_errors(
        analyze("func g(a: i32 := true): i32 { return a; }", &[]),
        &["Type mismatch between 'i32' and 'bool'"],
    );
    // the identical mismatch is no longer double-reported
    assert_entry(&s, 1, "bool", 2, 1, 1);
}

#[test]
fn default_params_reject_out_of_range_argument_counts() {
    let s = assert_errors(
        analyze(
            "func gh(a: i32, b: i32 := 8): i32 { return a + b; }\nvar z := gh(1, 2, 3);",
            &[],
        ),
        &["Invalid argument count expected '1-2' but got '3'"],
    );
    assert_eq!(tid_by_name(&s, "func(i32, i32) : i32"), 3);
}

// member access / struct init errors
#[test]
fn unknown_struct_member_and_field_identifiers() {
    let s = assert_errors(
        analyze(
            "struct Foo { x: i32 }\nfunc f(): i32 { var u := .Foo{.x = 1}; return u.y; }",
            &[],
        ),
        &["'y' is not a member of 'Foo'"],
    );
    // the failing field identifier, the access expr, and the return stmt all stay Unknown
    assert_entry(&s, 10, "unknown", 4, 0, 0);
    assert_entry(&s, 11, "unknown", 4, 0, 0);
    assert_entry(&s, 12, "unknown", 4, 0, 0);
    assert_span(&s, 10, 70, 71);
    assert_entry(&s, 6, "i32", 2, 4, 4); // `.x = 1` literal coerced to i32
}

#[test]
fn successful_field_identifier_nodes_are_unit() {
    let s = assert_clean(analyze(
        "struct Foo { x: i32, y: i32 }\nfunc f(): i32 { var u := .Foo{.x = 1, .y = 2}; return u.y; }",
        &[],
    ));
    // the accessed field identifier `y` carries Unit (the reference itself is
    // not an error, and its value is read through the access expression)
    assert_entry(&s, 14, "()", 1, 0, 1);
    // the access expression and the return stmt are typed by the field type
    assert_entry(&s, 15, "i32", 2, 4, 4);
    assert_entry(&s, 16, "i32", 2, 4, 4);
}

#[test]
fn unknown_ctor_field_reports_span() {
    let (s, diag) = analyze(
        "struct Foo2 { x: i32 }\nfunc f2(): i32 { var u := .Foo2{.z = 1}; return u.x; }",
        &[],
    );
    assert!(s.corrupted);
    assert_eq!(
        messages(&diag),
        vec!["Unknown field 'z' in struct initialization"]
    );
    let errs = diag.borrow();
    let span = errs.errors[0].span.clone();
    assert!(span.is_some(), "expected a diagnostic span, got None");
    let sp = span.unwrap();
    assert_eq!(
        (sp.start, sp.end),
        (55, 61),
        "unexpected span for the unknown field"
    );
}

#[test]
fn variant_arm_arg_count_and_unknown_member() {
    let _s = assert_errors(
        analyze(
            "variant Shape2 { Circle(i8, i64), Square }\nvar w1 := Shape2.Circle(1);",
            &[],
        ),
        &["Variant 'Shape2.Circle' expects 2 arguments, but got 1"],
    );
    let s = assert_errors(
        analyze(
            "variant Shape3 { Circle(i8, i64), Square }\nvar w2 := Shape3.Nope;",
            &[],
        ),
        &["'Nope' is not a member of 'Shape3'"],
    );
    assert_entry(&s, 6, "unknown", 5, 0, 0);
    assert_span(&s, 6, 60, 64);
}

// indexing
#[test]
fn index_type_rejects_float_index() {
    let s = assert_errors(
        analyze("var list := [1, 2];\nvar z := list[2.5];", &[]),
        &["Invalid index type 'f64' array indexes must be integers"],
    );
    // the failed index expression and its var stmt are Unknown
    assert_entry(&s, 6, "unknown", 1, 0, 0);
    assert_entry(&s, 7, "unknown", 1, 0, 0);
}

#[test]
fn indexing_non_indexable_type_reports_error() {
    let s = assert_errors(
        analyze("var x0 := 5;\nvar y0 := x0[0];", &[]),
        &["Cannot index into a non indexable type 'isize'"],
    );
    assert_entry(&s, 4, "unknown", 1, 0, 0);
    assert_entry(&s, 5, "unknown", 1, 0, 0);
}

// cast / bitcast
#[test]
fn numeric_cast_is_allowed_bool_cast_is_not() {
    let s = assert_clean(analyze("var v := 3;\nvar c7 := cast<u32>(v);", &[]));
    assert_entry(&s, 2, "u32", 3, 4, 4);
    let s = assert_errors(
        analyze("var pv := 3;\nvar qv := cast<bool>(pv);", &[]),
        &["Invalid cast cannot convert 'isize' to 'bool'"],
    );
    assert_entry(&s, 4, "unknown", 1, 0, 0);
    assert_entry(&s, 5, "unknown", 1, 0, 0);
}

#[test]
fn bitcast_requires_matching_sizes() {
    let s = assert_errors(
        analyze("var bm := bitcast<u64>(1i32);", &[]),
        &["bitcast size mismatch, cannot reinterpret 'i32' (4 bytes) as 'u64' (8 bytes), ensure sizes match"],
    );
    assert_entry(&s, 2, "unknown", 1, 0, 0);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
}

// str / char literals (width suffixes)
#[test]
fn str_and_char_literal_width_typing() {
    let s = assert_clean(analyze("var s := \"hello\";", &[]));
    assert_entry(&s, 0, "str", 2, 8, 8);
    assert_entry(&s, 1, "str", 2, 8, 8);
    assert_span(&s, 0, 9, 16);

    let s = assert_clean(analyze(
        "var a := 'x';\nvar b := 'x'c16;\nvar c := 'x'c32;",
        &[],
    ));
    // char8 (default) 1x1, char16 2x2, char32 4x4
    assert_entry(&s, 0, "char8", 2, 1, 1);
    assert_entry(&s, 1, "char8", 2, 1, 1);
    assert_span(&s, 0, 9, 12);
    assert_entry(&s, 2, "char16", 3, 2, 2);
    assert_entry(&s, 3, "char16", 3, 2, 2);
    assert_span(&s, 2, 23, 29);
    assert_entry(&s, 4, "char32", 4, 4, 4);
    assert_entry(&s, 5, "char32", 4, 4, 4);
    assert_span(&s, 4, 40, 46);
}

#[test]
fn str_annotation_param_ret_and_call() {
    let s = assert_clean(analyze(
        "func id(s: str): str { return s; }\nvar a := \"a\";\nvar r := id(a);",
        &[],
    ));
    // param s 0-4, ret type at 2 (17,20), func declares at 5/8 (0,38), var a 6/7/9 (44,47), call 10/11 (17,20)
    assert_entry(&s, 0, "str", 1, 8, 8);
    assert_span(&s, 0, 11, 14);
    assert_entry(&s, 2, "str", 1, 8, 8);
    assert_span(&s, 2, 17, 20);
    assert_entry(&s, 5, "func(str) : str", 2, 8, 8);
    assert_span(&s, 5, 0, 38);
    assert_entry(&s, 6, "str", 1, 8, 8);
    assert_span(&s, 6, 44, 47);
    assert_entry(&s, 8, "func(str) : str", 2, 8, 8);
    assert_entry(&s, 11, "str", 1, 8, 8);
    assert_span(&s, 11, 17, 20);
}

// ptr / ref / deref
#[test]
fn annotated_pointer_var_accepts_address_of() {
    let s = assert_clean(analyze("var x := 5;\nvar ptr<isize> p := @x;", &[]));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_span(&s, 0, 9, 10);
    assert_entry(&s, 2, "isize", 2, 8, 8); // isize inside the annotation
    assert_span(&s, 2, 20, 25);
    assert_entry(&s, 3, "ptr<isize>", 3, 8, 8);
    assert_span(&s, 3, 16, 25);
    assert_entry(&s, 4, "isize", 2, 8, 8); // @x
    assert_entry(&s, 6, "ptr<isize>", 3, 8, 8); // var stmt carries annotation span
    assert_span(&s, 6, 16, 25);
}

#[test]
fn reference_annotation_rejects_pointer_value() {
    let s = assert_errors(
        analyze("var x := 5;\nvar ref<isize> r := @x;", &[]),
        &["Type mismatch between 'ref<isize>' and 'ptr<isize>'"],
    );
    assert_entry(&s, 3, "ref<isize>", 4, 8, 8);
    assert_span(&s, 3, 16, 25);
    assert_entry(&s, 5, "ptr<isize>", 3, 8, 8); // @x stays ptr
    assert_entry(&s, 6, "ref<isize>", 4, 8, 8);
    assert_span(&s, 6, 16, 25);
}

#[test]
fn pointer_chain_address_of_then_deref() {
    let s = assert_clean(analyze("var x := 5;\nvar p := @x;\nvar v := ^p;", &[]));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 3, "ptr<isize>", 3, 8, 8);
    assert_entry(&s, 6, "isize", 2, 8, 8); // ^p deref result
    assert_entry(&s, 7, "isize", 2, 8, 8);
}

#[test]
fn deref_non_pointer_reports_error() {
    let s = assert_errors(
        analyze("var x := 5;\nvar v := ^x;", &[]),
        &["Cannot dereference type 'isize'"],
    );
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
    assert_entry(&s, 4, "unknown", 1, 0, 0);
}

// ---------------------------------------------------------------------------
// function pointers
// ---------------------------------------------------------------------------

#[test]
fn fnptr_annotation_in_struct_field() {
    let s = assert_clean(analyze("struct S { cb: func(i32) : i32 }", &[]));
    assert_entry(&s, 0, "i32", 1, 4, 4); // param i32
    assert_span(&s, 0, 20, 23);
    assert_entry(&s, 1, "i32", 1, 4, 4); // ret i32
    assert_entry(&s, 2, "func(i32) : i32", 2, 8, 8); // fnptr annotation
    assert_span(&s, 2, 15, 32);
    assert_entry(&s, 3, "func(i32) : i32", 2, 8, 8);
    assert_entry(&s, 4, "S", 0, 8, 8);
}

#[test]
fn fnptr_referencing_declared_function() {
    let s = assert_clean(analyze(
        "func helper(x: i32): i32 { return x; }\nvar h := helper;",
        &[],
    ));
    assert_entry(&s, 0, "i32", 1, 4, 4);
    assert_entry(&s, 5, "func(i32) : i32", 2, 8, 8); // func decl type
    assert_span(&s, 5, 0, 42);
    assert_entry(&s, 7, "func(i32) : i32", 2, 8, 8); // var h := helper
}

#[test]
fn fnptr_struct_field_accepts_function_value() {
    let s = assert_clean(analyze(
        "struct S { cb: func(i32) : i32 }\nfunc helper(x: i32): i32 { return x; }\nvar s := .S{.cb = helper};",
        &[],
    ));
    // fnptr annotation (2/3) and helper value (10/12/13) dedup to the same TypeId
    assert_entry(&s, 2, "func(i32) : i32", 3, 8, 8);
    assert_span(&s, 2, 15, 32);
    assert_entry(&s, 3, "func(i32) : i32", 3, 8, 8);
    assert_entry(&s, 10, "func(i32) : i32", 3, 8, 8);
    assert_span(&s, 10, 33, 75);
    assert_entry(&s, 11, "S", 0, 8, 8);
    assert_entry(&s, 12, "func(i32) : i32", 3, 8, 8);
    assert_entry(&s, 13, "func(i32) : i32", 3, 8, 8);
    assert_entry(&s, 14, "S", 0, 8, 8);
    assert_entry(&s, 15, "S", 0, 8, 8);
    assert_span(&s, 14, 0, 37);
}

// dollar-block result typing
#[test]
fn dollar_scope_result_typing() {
    let s = assert_clean(analyze("var d := $${ 5; };", &[]));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 2, "isize", 2, 8, 8);
    assert_span(&s, 0, 13, 14);
}

#[test]
fn dollar_scope_empty_result_is_unit() {
    let s = assert_clean(analyze("var d := $$ { };", &[]));
    assert_entry(&s, 0, "()", 0, 0, 1);
    assert_entry(&s, 1, "()", 0, 0, 1);
    assert_span(&s, 0, 9, 16);
}

#[test]
fn dollar_scope_capture_result_typing() {
    let s = assert_clean(analyze("const var a := 5;\nvar d := $$|a|{ a; };", &[]));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 2, "isize", 2, 8, 8);
    assert_entry(&s, 3, "isize", 2, 8, 8);
    assert_entry(&s, 4, "isize", 2, 8, 8);
    assert_entry(&s, 5, "isize", 2, 8, 8);
    assert_span(&s, 2, 15, 16);
}

// const / expose globals
#[test]
fn const_var_global_referenced_from_function() {
    let s = assert_clean(analyze(
        "const var g := 5;\nfunc f(): isize { return g; }\nvar u := f();",
        &[],
    ));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_span(&s, 0, 15, 16);
    assert_entry(&s, 2, "isize", 2, 8, 8); // ret annotation isize
    assert_span(&s, 2, 28, 33);
    assert_entry(&s, 5, "func() : isize", 3, 8, 8);
    assert_span(&s, 5, 18, 51);
    assert_entry(&s, 7, "isize", 2, 8, 8); // f() call result
    assert_entry(&s, 8, "isize", 2, 8, 8);
}

#[test]
fn expose_var_global_typing() {
    let s = assert_clean(analyze("expose var e := 9;\nvar u := e;", &[]));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 2, "isize", 2, 8, 8);
    assert_entry(&s, 3, "isize", 2, 8, 8);
    assert_span(&s, 0, 16, 17);
    assert_span(&s, 3, 16, 17);
}

// generics: function instantiation, struct specialization, operator bounds
#[test]
fn generic_func_instantiation_types_and_monomorph_backlog() {
    let s = assert_clean(analyze(
        "generics<T> { func id(x: T): T { return x; } }\nvar r := id::<i32>(5);",
        &[],
    ));
    // template params/body are all T (0x0), the generic func decl is func(T) : T
    assert_entry(&s, 1, "T", 1, 0, 0);
    assert_entry(&s, 6, "func(T) : T", 2, 8, 8);
    assert_span(&s, 6, 14, 46);
    // the ::<i32> instantiation specializes the signature
    assert_entry(&s, 7, "i32", 4, 4, 4);
    assert_entry(&s, 8, "func(i32) : i32", 5, 8, 8);
    assert_span(&s, 8, 56, 65);
    assert_entry(&s, 9, "i32", 4, 4, 4);
    assert_entry(&s, 10, "i32", 4, 4, 4);
    assert_entry(&s, 11, "i32", 4, 4, 4);

    // one monomorphization instance requested: func id with arg i32
    let backlog: Vec<(usize, Vec<String>)> = s
        .ctxt
        .monomorph_backlog
        .iter()
        .map(|k| {
            (
                k.original_def_id.local,
                k.concrete_args.iter().map(|t| t.name.clone()).collect(),
            )
        })
        .collect();
    assert_eq!(backlog, vec![(6, vec!["i32".to_string()])]);
}

#[test]
fn generic_struct_instantiation_specializes_type_and_members() {
    let s = assert_clean(analyze(
        "generics<T> { struct Box { v: T } }\nvar b := .Box<i32>{ .v = 5 };",
        &[],
    ));
    // template stays generic with deferred layout (v is T -> empty)
    assert_entry(&s, 3, "Box<T>", 3, 0, 1);
    assert_span(&s, 3, 14, 35);
    // .Box<i32> specializes the struct: member v is i32 -> real 4x4 layout
    assert_entry(&s, 4, "i32", 5, 4, 4);
    assert_entry(&s, 5, "Box<i32>", 6, 4, 4);
    assert_span(&s, 5, 46, 49);
    assert_entry(&s, 6, "i32", 5, 4, 4);
    assert_entry(&s, 8, "Box<i32>", 6, 4, 4);
    assert_entry(&s, 9, "Box<i32>", 6, 4, 4);

    match &s
        .ctxt
        .types
        .types
        .get(&NodeId {
            local: 5,
            external: 0,
        })
        .unwrap()
        .kind
    {
        ResolvedTypeKind::Struct { name, members, .. } => {
            assert_eq!(name, "Box"); // kind keeps the template name; display name is "Box<i32>"
            assert_eq!(members.len(), 1);
            assert_eq!(members[0].0, "v");
            assert_eq!(members[0].1.name, "i32");
        }
        other => panic!("expected specialized Struct kind, got {other:?}"),
    }

    // monomorphization requested for the template def (id 3) with arg i32
    let backlog: Vec<(usize, Vec<String>)> = s
        .ctxt
        .monomorph_backlog
        .iter()
        .map(|k| {
            (
                k.original_def_id.local,
                k.concrete_args.iter().map(|t| t.name.clone()).collect(),
            )
        })
        .collect();
    assert_eq!(backlog, vec![(3, vec!["i32".to_string()])]);
}

#[test]
fn generic_struct_layout_mixed_size_fields() {
    let s = assert_clean(analyze(
        "generics<T> { struct Pair { a: isize, b: T } }\nvar p := .Pair<i32>{ .a = 1, .b = 2 };",
        &[],
    ));
    // a@0 (8b), b@8 (4b), tail pad to align 8 -> 16 bytes
    assert_entry(&s, 5, "Pair<T>", 4, 8, 8);
    assert_entry(&s, 7, "Pair<i32>", 7, 16, 8);
    assert_span(&s, 7, 57, 61);
    assert_entry(&s, 10, "i32", 6, 4, 4);
    assert_entry(&s, 12, "Pair<i32>", 7, 16, 8);
    assert_entry(&s, 13, "Pair<i32>", 7, 16, 8);
}

#[test]
fn generic_body_operator_on_type_param_reports_error() {
    let s = assert_errors(
        analyze(
            "generics<T> { func wid(x: T): T { return x + x; } }\nvar r2 := wid::<i32>(5);",
            &[],
        ),
        &["Left operand of arithmetic operation must be numeric"],
    );
    // the x + x expression and its return stmt are Unknown
    assert_entry(&s, 6, "unknown", 3, 0, 0);
    assert_entry(&s, 7, "unknown", 3, 0, 0);
    // signature specialization still works
    assert_entry(&s, 8, "func(T) : T", 2, 8, 8);
    assert_entry(&s, 10, "func(i32) : i32", 5, 8, 8);
}

// boolean logic operators
#[test]
fn logical_bool_operators_type_as_bool() {
    let s = assert_clean(analyze(
        "var lg0 := true && false;\nvar lg1 := lg0 || true;",
        &[],
    ));
    // every node in both statements is Bool
    for id in 0..=7 {
        assert_entry(&s, id, "bool", 2, 1, 1);
    }
    assert_span(&s, 0, 11, 15); // literal true
    assert_span(&s, 2, 11, 24); // true && false
    assert_span(&s, 6, 37, 48); // lg0 || true
}

#[test]
fn logical_numeric_operands_reports_error() {
    let s = assert_errors(
        analyze("var a := 5 && 3;", &[]),
        &["logical binary operator cannot be applied to types 'isize' and 'isize'"],
    );
    assert_entry(&s, 2, "unknown", 1, 0, 0);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
}

#[test]
fn logical_mixed_operands_reports_error() {
    let s = assert_errors(
        analyze("var a := true && 5;", &[]),
        &["logical binary operator cannot be applied to types 'bool' and 'isize'"],
    );
    assert_entry(&s, 0, "bool", 2, 1, 1);
    assert_entry(&s, 1, "isize", 3, 8, 8);
    assert_entry(&s, 2, "unknown", 1, 0, 0);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
}

// comparisons
#[test]
fn comparison_operators_type_as_bool() {
    let s = assert_clean(analyze(
        "var c0 := 1 < 2;\nvar c1 := 1 == 1;\nvar c2 := 2 >= 1;\nvar c3 := 2 != 3;",
        &[],
    ));
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "isize", 2, 8, 8);
    assert_entry(&s, 2, "bool", 3, 1, 1);
    assert_entry(&s, 3, "bool", 3, 1, 1);
    assert_entry(&s, 6, "bool", 3, 1, 1);
    assert_entry(&s, 7, "bool", 3, 1, 1);
    assert_entry(&s, 10, "bool", 3, 1, 1);
    assert_entry(&s, 14, "bool", 3, 1, 1);
    assert_span(&s, 2, 10, 15); // 1 < 2
    assert_span(&s, 6, 27, 33); // 1 == 1
    assert_span(&s, 14, 63, 69); // 2 != 3
}

#[test]
fn comparison_mismatched_operands_reports_error() {
    let s = assert_errors(
        analyze("var a := 1 == \"s\";", &[]),
        &["Type mismatch between 'isize' and 'str'"],
    );
    assert_entry(&s, 0, "isize", 2, 8, 8);
    assert_entry(&s, 1, "str", 3, 8, 8);
    assert_entry(&s, 2, "unknown", 1, 0, 0);
    assert_entry(&s, 3, "unknown", 1, 0, 0);
}

#[test]
fn arrays_can_be_compared_for_equality() {
    let s = assert_clean(analyze(
        "var a := [1, 2];\nvar b := [3, 4];\nvar c := a == b;",
        &[],
    ));
    assert_entry(&s, 8, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 9, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 10, "bool", 4, 1, 1);
    assert_entry(&s, 11, "bool", 4, 1, 1);
}

// casts
#[test]
fn cast_array_to_scalar_reports_error() {
    let s = assert_errors(
        analyze("var a := [1, 2];\nvar b := cast<i32>(a);", &[]),
        &["Invalid cast cannot convert 'arr[isize,2]' to 'i32'"],
    );
    assert_entry(&s, 2, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 4, "i32", 4, 4, 4);
    assert_entry(&s, 5, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 6, "unknown", 1, 0, 0);
}

#[test]
fn ptr_and_int_casts_type_correctly() {
    let s = assert_clean(analyze(
        "var x := 5;\nvar p := cast<ptr<isize>>(@x);\nvar y := cast<isize>(p);",
        &[],
    ));
    assert_entry(&s, 3, "ptr<isize>", 3, 8, 8); // cast<ptr<isize>>(@x)
    assert_entry(&s, 7, "ptr<isize>", 3, 8, 8); // p decl
    assert_entry(&s, 10, "isize", 2, 8, 8); // cast<isize>(p)
    assert_entry(&s, 11, "isize", 2, 8, 8); // y stmt
}

// assignments (top-level expression statements)
#[test]
fn assignment_nodes_type_as_left_operand() {
    let s = assert_clean(analyze("var x := 5;\nx = 7;", &[]));
    assert_entry(&s, 2, "isize", 2, 8, 8); // x usage
    assert_entry(&s, 3, "isize", 2, 8, 8); // literal 7 (coerced)
    assert_entry(&s, 4, "isize", 2, 8, 8); // assignment statement
}

#[test]
fn assignment_mismatch_reports_error() {
    let s = assert_errors(
        analyze("var x := 5;\nx = \"s\";", &[]),
        &["Type mismatch between 'isize' and 'str'"],
    );
    assert_entry(&s, 3, "str", 3, 8, 8);
    assert_entry(&s, 4, "unknown", 1, 0, 0);
}

#[test]
fn compound_assignment_mismatch_reports_error() {
    let s = assert_errors(
        analyze("mut var x := 5;\nx += true;", &[]),
        &["Type mismatch between 'isize' and 'bool'"],
    );
    assert_entry(&s, 3, "bool", 3, 1, 1);
    assert_entry(&s, 4, "unknown", 1, 0, 0);
}

#[test]
fn assignment_to_struct_field_types_the_field() {
    let s = assert_clean(analyze(
        "struct S { a: isize, b: isize }\nvar s := .S{.a = 1, .b = 2};\ns.b = 9;",
        &[],
    ));
    assert_entry(&s, 10, "S", 0, 16, 8); // s usage
    assert_entry(&s, 13, "()", 1, 0, 1); // field access node
    assert_entry(&s, 15, "isize", 2, 8, 8); // s.b = 9 result
    assert_entry(&s, 16, "isize", 2, 8, 8); // statement
}

#[test]
fn array_typed_chained_assignment_reports_error() {
    let s = assert_errors(
        analyze("var a := [1, 2];\nvar b := [3, 4];\na = b = a;", &[]),
        &["Cannot chain assignment through an array-typed assignment"],
    );
    assert_entry(&s, 8, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 11, "arr[isize,2]", 3, 16, 8);
    assert_entry(&s, 12, "unknown", 1, 0, 0);
}
