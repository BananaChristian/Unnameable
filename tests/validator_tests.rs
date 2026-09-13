mod common;

use common::messages;
use unnc::const_and_mut_validator::Validator;

use common::analyze;

/// Runs the full pipeline up to (and including) the const_and_mut validator
/// and returns every reported diagnostic message.
fn validate(src: &str) -> Vec<String> {
    let (mut semantics, diag) = analyze(src, &[]);
    let hir = semantics.generate_monormophizer_hir();
    let mut validator = Validator::new(diag.clone());
    validator.run(&hir);
    messages(&diag)
}

fn assert_clean(src: &str) {
    let msgs = validate(src);
    assert!(
        msgs.is_empty(),
        "expected no diagnostics for:\n{src}\ngot: {msgs:?}",
    );
}

fn assert_messages(src: &str, expected: &[&str]) {
    let msgs = validate(src);
    let expected: Vec<String> = expected.iter().map(|s| s.to_string()).collect();
    assert_eq!(msgs, expected, "diagnostics mismatch for:\n{src}");
}

// ---------------------------------------------------------------------------
// const declarations
// ---------------------------------------------------------------------------

#[test]
fn const_and_mut_are_mutually_exclusive() {
    assert_messages(
        "const mut var x := 5;",
        &["Variable 'x' cannot be const and mutable at the same time"],
    );
}

#[test]
fn const_with_literal_init_passes() {
    assert_clean("const var x := 5;\nconst var s := \"s\";\nconst var t := true;");
}

#[test]
fn const_with_array_literal_init_reports() {
    assert_messages(
        "const var a := [1, 2];",
        &["Constant variable 'a' must be initilialized with a compile time value"],
    );
}

#[test]
fn const_with_expression_init_reports() {
    assert_messages(
        "const var b := 1 + 2;",
        &["Constant variable 'b' must be initilialized with a compile time value"],
    );
}

// ---------------------------------------------------------------------------
// assignment targets
// ---------------------------------------------------------------------------

#[test]
fn assign_to_immutable_reports() {
    assert_messages(
        "var x := 5;\nx = 7;",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn assign_to_mutable_passes() {
    assert_clean("mut var x := 5;\nx = 7;");
}

#[test]
fn assign_to_const_reports() {
    assert_messages(
        "const var k := 5;\nk = 7;",
        &["Cannot assign to constant variable 'k'"],
    );
}

#[test]
fn compound_assign_to_immutable_reports() {
    assert_messages(
        "var x := 5;\nx += 1;\nx -= 1;\nx *= 2;\nx /= 2;\nx %= 3;",
        &[
            "Cannot add and assign to immutable variable 'x'",
            "Cannot subtract and assign to immutable variable 'x'",
            "Cannot multiply and assign to immutable variable 'x'",
            "Cannot divide and assign to immutable variable 'x'",
            "Cannot modulo and assign to immutable variable 'x'",
        ],
    );
}

#[test]
fn compound_assign_to_mutable_passes() {
    assert_clean("mut var x := 5;\nx += 1;\nx -= 1;\nx *= 2;\nx /= 2;\nx %= 3;");
}

#[test]
fn prefix_increment_decrement_on_immutable_reports() {
    assert_messages(
        "var x := 5;\n++x;\n--x;",
        &[
            "Cannot increment immutable variable 'x'",
            "Cannot decrement immutable variable 'x'",
        ],
    );
}

#[test]
fn prefix_increment_decrement_on_mutable_passes() {
    assert_clean("mut var x := 5;\n++x;\n--x;");
}

#[test]
fn postfix_increment_decrement_on_immutable_reports() {
    assert_messages(
        "var x := 5;\nx++;\nx--;",
        &[
            "Cannot increment immutable variable 'x'",
            "Cannot decrement immutable variable 'x'",
        ],
    );
}

#[test]
fn postfix_increment_decrement_on_mutable_passes() {
    assert_clean("mut var x := 5;\nx++;\nx--;");
}

#[test]
fn assign_chain_reports_each_target() {
    assert_messages(
        "var x := 5;\nvar y := 5;\nx = y = 7;",
        &[
            "Cannot assign to immutable variable 'y'",
            "Cannot assign to immutable variable 'x'",
        ],
    );
}

#[test]
fn assign_chain_of_mutable_passes() {
    assert_clean("mut var x := 5;\nmut var y := 5;\nx = y = 7;");
}

// ---------------------------------------------------------------------------
// function parameters
// ---------------------------------------------------------------------------

#[test]
fn immutable_param_mutation_reports() {
    assert_messages(
        "func f(x: isize) {\n    x = 1;\n}\n",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn mutable_param_mutation_passes() {
    assert_clean("func f(mut x: isize) {\n    x = 1;\n}\n");
}

#[test]
fn immutable_param_increment_reports() {
    assert_messages(
        "func f(x: isize) {\n    x++;\n}\n",
        &["Cannot increment immutable variable 'x'"],
    );
}

// ---------------------------------------------------------------------------
// scopes
// ---------------------------------------------------------------------------

#[test]
fn mutation_inside_while_reports() {
    assert_messages(
        "var x := 5;\nwhile true { x = 1; }",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn mutation_inside_if_and_else_reports() {
    assert_messages(
        "var x := 5;\nif true { x = 1; } else { x = 2; }",
        &[
            "Cannot assign to immutable variable 'x'",
            "Cannot assign to immutable variable 'x'",
        ],
    );
}

#[test]
fn mutation_inside_if_and_else_passes_for_mutable() {
    assert_clean("mut var x := 5;\nif true { x = 1; } else { x = 2; }");
}

#[test]
fn var_declared_inside_if_immutable_reports() {
    assert_messages(
        "if true { var x := 1; x = 2; }",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn var_declared_inside_if_mutable_passes() {
    assert_clean("if true { mut var x := 1; x = 2; }");
}

// ---------------------------------------------------------------------------
// embedded mutations inside expressions
// ---------------------------------------------------------------------------

#[test]
fn embedded_assign_in_binary_operand_reports() {
    assert_messages(
        "var y := 5;\nvar z := 1 + (y = 3);\nvar w := (y = 3) + 1;",
        &[
            "Cannot assign to immutable variable 'y'",
            "Cannot assign to immutable variable 'y'",
        ],
    );
}

#[test]
fn embedded_assign_in_unary_operand_reports() {
    assert_messages(
        "var y := 5;\nvar z := -(y = 3);",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_struct_init_value_reports() {
    assert_messages(
        "struct S { a: isize }\nvar y := 5;\nvar s := .S{.a = (y = 3)};",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_call_argument_reports() {
    assert_messages(
        "func f(a: isize) {}\nvar y := 5;\nf(y = 3);",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_compound_rhs_reports() {
    assert_messages(
        "mut var x := 5;\nvar y := 5;\nx += (y = 3);",
        &["Cannot assign to immutable variable 'y'"],
    );
}

// ---------------------------------------------------------------------------
// aggregate mutation targets (mutability is transitive through the whole value
// and belongs to the root binding: `s.a`, `s.p.x`, `a[0]`, `t.0`)
// ---------------------------------------------------------------------------

#[test]
fn struct_field_write_on_immutable_reports() {
    assert_messages(
        "struct S { a: isize }\nfunc f(): isize {\n    var s := .S{.a = 1};\n    s.a = 2;\n    return 0;\n}",
        &["Cannot assign to immutable variable 's'"],
    );
}

#[test]
fn struct_field_write_on_mutable_passes() {
    assert_clean(
        "struct S { a: isize }\nfunc f(): isize {\n    mut var s := .S{.a = 1};\n    s.a = 2;\n    return 0;\n}",
    );
}

#[test]
fn struct_field_compound_on_immutable_reports() {
    assert_messages(
        "struct S { a: isize }\nfunc f(): isize {\n    var s := .S{.a = 1};\n    s.a += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 's'"],
    );
}

#[test]
fn struct_field_compound_on_mutable_passes() {
    assert_clean(
        "struct S { a: isize }\nfunc f(): isize {\n    mut var s := .S{.a = 1};\n    s.a += 1;\n    return 0;\n}",
    );
}

#[test]
fn array_element_write_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var a := [1, 2];\n    a[0] = 3;\n    return 0;\n}",
        &["Cannot assign to immutable variable 'a'"],
    );
}

#[test]
fn array_element_write_on_mutable_passes() {
    assert_clean("func f(): isize {\n    mut var a := [1, 2];\n    a[0] = 3;\n    return 0;\n}");
}

#[test]
fn array_element_compound_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var a := [1, 2];\n    a[0] += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 'a'"],
    );
}

#[test]
fn tuple_element_write_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var t := .(1, 2);\n    t.0 = 5;\n    return 0;\n}",
        &["Cannot assign to immutable variable 't'"],
    );
}

#[test]
fn tuple_element_write_on_mutable_passes() {
    assert_clean("func f(): isize {\n    mut var t := .(1, 2);\n    t.0 = 5;\n    return 0;\n}");
}

#[test]
fn tuple_element_compound_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var t := .(1, 2);\n    t.0 += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 't'"],
    );
}

#[test]
fn nested_field_write_checks_root_binding() {
    assert_messages(
        "struct Pos { x: isize }\nstruct S { p: Pos }\nfunc f(): isize {\n    var s := .S{.p = .Pos{.x = 1}};\n    s.p.x = 5;\n    return 0;\n}",
        &["Cannot assign to immutable variable 's'"],
    );
    assert_clean(
        "struct Pos { x: isize }\nstruct S { p: Pos }\nfunc f(): isize {\n    mut var s := .S{.p = .Pos{.x = 1}};\n    s.p.x = 5;\n    return 0;\n}",
    );
}

#[test]
fn array_element_index_checked_for_embedded_mutation() {
    assert_messages(
        "func f(): isize {\n    var y := 5;\n    var a := [1, 2];\n    a[y = 3] = 4;\n    return 0;\n}",
        &["Cannot assign to immutable variable 'y'", "Cannot assign to immutable variable 'a'"],
    );
}
