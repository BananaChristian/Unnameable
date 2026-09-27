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
        "const mut var x = 5;",
        &["Variable 'x' cannot be const and mutable at the same time"],
    );
}

#[test]
fn const_with_literal_init_passes() {
    assert_clean("const var x = 5;\nconst var s = \"s\";\nconst var t = true;");
}

#[test]
fn const_with_array_literal_init_reports() {
    assert_messages(
        "const var a = [1, 2];",
        &["Constant variable 'a' must be initilialized with a compile time value"],
    );
}

#[test]
fn const_with_expression_init_reports() {
    assert_messages(
        "const var b = 1 + 2;",
        &["Constant variable 'b' must be initilialized with a compile time value"],
    );
}

// ---------------------------------------------------------------------------
// assignment targets
// ---------------------------------------------------------------------------

#[test]
fn assign_to_immutable_reports() {
    assert_messages(
        "var x = 5;\nx = 7;",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn assign_to_mutable_passes() {
    assert_clean("mut var x = 5;\nx = 7;");
}

#[test]
fn assign_to_const_reports() {
    assert_messages(
        "const var k = 5;\nk = 7;",
        &["Cannot assign to constant variable 'k'"],
    );
}

#[test]
fn compound_assign_to_immutable_reports() {
    assert_messages(
        "var x = 5;\nx += 1;\nx -= 1;\nx *= 2;\nx /= 2;\nx %= 3;",
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
    assert_clean("mut var x = 5;\nx += 1;\nx -= 1;\nx *= 2;\nx /= 2;\nx %= 3;");
}

#[test]
fn prefix_increment_decrement_on_immutable_reports() {
    assert_messages(
        "var x = 5;\n++x;\n--x;",
        &[
            "Cannot increment immutable variable 'x'",
            "Cannot decrement immutable variable 'x'",
        ],
    );
}

#[test]
fn prefix_increment_decrement_on_mutable_passes() {
    assert_clean("mut var x = 5;\n++x;\n--x;");
}

#[test]
fn postfix_increment_decrement_on_immutable_reports() {
    assert_messages(
        "var x = 5;\nx++;\nx--;",
        &[
            "Cannot increment immutable variable 'x'",
            "Cannot decrement immutable variable 'x'",
        ],
    );
}

#[test]
fn postfix_increment_decrement_on_mutable_passes() {
    assert_clean("mut var x = 5;\nx++;\nx--;");
}

#[test]
fn assign_chain_reports_each_target() {
    assert_messages(
        "var x = 5;\nvar y = 5;\nx = y = 7;",
        &[
            "Cannot assign to immutable variable 'y'",
            "Cannot assign to immutable variable 'x'",
        ],
    );
}

#[test]
fn assign_chain_of_mutable_passes() {
    assert_clean("mut var x = 5;\nmut var y = 5;\nx = y = 7;");
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
        "var x = 5;\nwhile true { x = 1; }",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn mutation_inside_if_and_else_reports() {
    assert_messages(
        "var x = 5;\nif true { x = 1; } else { x = 2; }",
        &[
            "Cannot assign to immutable variable 'x'",
            "Cannot assign to immutable variable 'x'",
        ],
    );
}

#[test]
fn mutation_inside_if_and_else_passes_for_mutable() {
    assert_clean("mut var x = 5;\nif true { x = 1; } else { x = 2; }");
}

#[test]
fn var_declared_inside_if_immutable_reports() {
    assert_messages(
        "if true { var x = 1; x = 2; }",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn var_declared_inside_if_mutable_passes() {
    assert_clean("if true { mut var x = 1; x = 2; }");
}

// ---------------------------------------------------------------------------
// embedded mutations inside expressions
// ---------------------------------------------------------------------------

#[test]
fn embedded_assign_in_binary_operand_reports() {
    assert_messages(
        "var y = 5;\nvar z = 1 + (y = 3);\nvar w = (y = 3) + 1;",
        &[
            "Cannot assign to immutable variable 'y'",
            "Cannot assign to immutable variable 'y'",
        ],
    );
}

#[test]
fn embedded_assign_in_unary_operand_reports() {
    assert_messages(
        "var y = 5;\nvar z = -(y = 3);",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_struct_init_value_reports() {
    assert_messages(
        "struct S { a: isize }\nvar y = 5;\nvar s = .S{.a = (y = 3)};",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_call_argument_reports() {
    assert_messages(
        "func f(a: isize) {}\nvar y = 5;\nf(y = 3);",
        &["Cannot assign to immutable variable 'y'"],
    );
}

#[test]
fn embedded_assign_in_compound_rhs_reports() {
    assert_messages(
        "mut var x = 5;\nvar y = 5;\nx += (y = 3);",
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
        "struct S { a: isize }\nfunc f(): isize {\n    var s = .S{.a = 1};\n    s.a = 2;\n    return 0;\n}",
        &["Cannot assign to immutable variable 's'"],
    );
}

#[test]
fn struct_field_write_through_mut_var_only_reports() {
    // `mut var` permits reassigning `s`, but writing `s.a` goes *through* the
    // type `S`, which is immutable. `mut var` does not grant that.
    assert_messages(
        "struct S { a: isize }\nfunc f(): isize {\n    mut var s = .S{.a = 1};\n    s.a = 2;\n    return 0;\n}",
        &["Cannot assign through immutable type of variable 's'"],
    );
}

#[test]
fn struct_field_compound_on_immutable_reports() {
    assert_messages(
        "struct S { a: isize }\nfunc f(): isize {\n    var s = .S{.a = 1};\n    s.a += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 's'"],
    );
}

#[test]
fn struct_field_compound_through_mut_var_only_reports() {
    assert_messages(
        "struct S { a: isize }\nfunc f(): isize {\n    mut var s = .S{.a = 1};\n    s.a += 1;\n    return 0;\n}",
        &["Cannot add and assign through immutable type of variable 's'"],
    );
}

#[test]
fn array_element_write_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var a = [1, 2];\n    a[0] = 3;\n    return 0;\n}",
        &["Cannot assign to immutable variable 'a'"],
    );
}

#[test]
fn array_element_write_through_mut_var_only_reports() {
    assert_messages(
        "func f(): isize {\n    mut var a = [1, 2];\n    a[0] = 3;\n    return 0;\n}",
        &["Cannot assign through immutable type of variable 'a'"],
    );
}

#[test]
fn array_element_compound_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var a = [1, 2];\n    a[0] += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 'a'"],
    );
}

#[test]
fn tuple_element_write_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var t = .(1, 2);\n    t.0 = 5;\n    return 0;\n}",
        &["Cannot assign to immutable variable 't'"],
    );
}

#[test]
fn tuple_element_write_through_mut_var_only_reports() {
    assert_messages(
        "func f(): isize {\n    mut var t = .(1, 2);\n    t.0 = 5;\n    return 0;\n}",
        &["Cannot assign through immutable type of variable 't'"],
    );
}

#[test]
fn tuple_element_compound_on_immutable_reports() {
    assert_messages(
        "func f(): isize {\n    var t = .(1, 2);\n    t.0 += 1;\n    return 0;\n}",
        &["Cannot add and assign to immutable variable 't'"],
    );
}

#[test]
fn nested_field_write_checks_root_binding() {
    assert_messages(
        "struct Pos { x: isize }\nstruct S { p: Pos }\nfunc f(): isize {\n    var s = .S{.p = .Pos{.x = 1}};\n    s.p.x = 5;\n    return 0;\n}",
        &["Cannot assign to immutable variable 's'"],
    );
    assert_messages(
        "struct Pos { x: isize }\nstruct S { p: Pos }\nfunc f(): isize {\n    mut var s = .S{.p = .Pos{.x = 1}};\n    s.p.x = 5;\n    return 0;\n}",
        &["Cannot assign through immutable type of variable 's'"],
    );
    // `mut T` on the root is what permits a write nested arbitrarily deep.
    assert_clean(
        "struct Pos { x: isize }\nstruct S { p: Pos }\nfunc f(): isize {\n    var s: mut S = .S{.p = .Pos{.x = 1}};\n    s.p.x = 5;\n    return 0;\n}",
    );
}

#[test]
fn array_element_index_checked_for_embedded_mutation() {
    assert_messages(
        "func f(): isize {\n    var y = 5;\n    var a = [1, 2];\n    a[y = 3] = 4;\n    return 0;\n}",
        &["Cannot assign to immutable variable 'y'", "Cannot assign to immutable variable 'a'"],
    );
}

// ---------------------------------------------------------------------------
// `mut T` -- a qualifier on the type, distinct from `mut var` on the binding.
//
// The two answer different questions, so they are tracked separately:
//   - `mut var x` permits reassigning the binding `x`.
//   - `mut T` permits writing *through* the type `T` (a field, an element, a
//     dereference), and a direct write of a `mut T` value counts as that too.
//
// So `mut var` alone is not enough for `x.f = 1`, and `mut T` on a *pointee*
// is not enough to rebind the pointer. See the pointer section below.
// ---------------------------------------------------------------------------

#[test]
fn immutable_type_rejects_write() {
    assert_messages(
        "var x: i32 = 1i32; x = 2i32;",
        &["Cannot assign to immutable variable 'x'"],
    );
}

#[test]
fn mut_type_allows_write() {
    assert_clean("var x: mut i32 = 1i32; x = 2i32;");
}

#[test]
fn mut_var_allows_write_of_immutable_type() {
    assert_clean("mut var x: i32 = 1i32; x = 2i32;");
}

#[test]
fn mut_type_allows_compound_assign() {
    assert_clean("var x: mut i32 = 1i32; x += 2i32;");
}

#[test]
fn immutable_type_rejects_compound_assign() {
    assert_messages(
        "var x: i32 = 1i32; x += 2i32;",
        &["Cannot add and assign to immutable variable 'x'"],
    );
}

#[test]
fn mut_type_allows_increment() {
    assert_clean("var x: mut i32 = 1i32; x++;");
}

#[test]
fn immutable_type_rejects_increment() {
    assert_messages(
        "var x: i32 = 1i32; x++;",
        &["Cannot increment immutable variable 'x'"],
    );
}

#[test]
fn mut_type_allows_struct_field_write() {
    assert_clean(
        "struct Food { power: i32 }\nvar s: mut Food = .Food{.power = 1}; s.power = 2i32;",
    );
}

#[test]
fn immutable_type_rejects_struct_field_write() {
    assert_messages(
        "struct Food { power: i32 }\nvar s: Food = .Food{.power = 1}; s.power = 2i32;",
        &["Cannot assign to immutable variable 's'"],
    );
}

#[test]
fn mut_type_allows_tuple_element_write() {
    assert_clean("var t: mut (i32, i32) = .(1i32, 2i32); t.0 = 5i32;");
}

#[test]
fn immutable_type_rejects_tuple_element_write() {
    assert_messages(
        "var t: (i32, i32) = .(1i32, 2i32); t.0 = 5i32;",
        &["Cannot assign to immutable variable 't'"],
    );
}

#[test]
fn mut_type_allows_array_element_write() {
    assert_clean("var a: mut [i32, 3] = [1i32, 2i32, 3i32]; a[0] = 9i32;");
}

#[test]
fn immutable_type_rejects_array_element_write() {
    assert_messages(
        "var a: [i32, 3] = [1i32, 2i32, 3i32]; a[1] = 9i32;",
        &["Cannot assign to immutable variable 'a'"],
    );
}

#[test]
fn mut_type_param_allows_write() {
    assert_clean("func f(a: mut i32): mut i32 { a = 1i32; return a; }");
}

#[test]
fn immutable_type_param_rejects_write() {
    assert_messages(
        "func f(a: i32): i32 { a = 1i32; return a; }",
        &["Cannot assign to immutable variable 'a'"],
    );
}

// A `mut` on the type and a `mut var` on the binding are independent: either
// one alone is enough, and they combine without complaint.
#[test]
fn mut_var_with_mut_type_allows_write() {
    assert_clean("mut var x: mut i32 = 1i32; x = 2i32;");
}

// `const` and `mut var` are two qualifiers on the *same* binding and genuinely
// contradict each other, so that pairing is still rejected.
#[test]
fn const_mut_var_is_rejected() {
    assert_messages(
        "const mut var x: i32 = 1i32;",
        &["Variable 'x' cannot be const and mutable at the same time"],
    );
}

// `mut T` qualifies the type, not the binding, so a const binding may name a
// mutable type: "constant name, mutable storage". The binding is still constant.
#[test]
fn const_with_mut_type_is_allowed() {
    assert_clean("const var x: mut i32 = 1i32;");
}

#[test]
fn const_with_mut_type_still_refuses_reassignment() {
    assert_messages(
        "const var x: mut i32 = 1i32; x = 2i32;",
        &["Cannot assign to constant variable 'x'"],
    );
}

#[test]
fn const_with_immutable_type_is_allowed() {
    assert_clean("const var x: i32 = 1i32;");
}

#[test]
fn const_with_mut_type_is_readable() {
    assert_clean("const var x: mut i32 = 1i32; var y: i32 = x;");
}

// The invariant that makes `allows_write_through` safe to leave ungated by
// `is_const`: a `const` binding can only ever hold a scalar, so it has no
// storage to write *through*. If this ever starts passing, the ungated through
// check would be aiming a write at a binding that (per AGENTS.md 2.7) has no
// alloca at all -- so this test is load-bearing, not incidental.
#[test]
fn const_binding_of_aggregate_type_is_rejected() {
    assert_messages(
        "struct S { p: i32 }\nconst var s: mut S = .S{.p = 1};",
        &["Constant variable 's' must be initilialized with a compile time value"],
    );
}

#[test]
fn const_binding_of_array_type_is_rejected() {
    assert_messages(
        "const var a: mut [i32, 2] = [1i32, 2i32];",
        &["Constant variable 'a' must be initilialized with a compile time value"],
    );
}

// A const binding cannot hold a pointer either (`@x` is not a literal), so the
// "constant pointer to mutable storage" case is not expressible yet.
#[test]
fn const_binding_of_ptr_type_is_rejected() {
    assert_messages(
        "func f(): i32 { var x: mut i32 = 1i32; const var p: ptr<mut i32> = @x; return 0i32; }",
        &["Constant variable 'p' must be initilialized with a compile time value"],
    );
}

// ---------------------------------------------------------------------------
// `mut` inside a pointee: `ptr<mut T>`.
//
// A pointer carries the permission of what it points at, so `^p = 1` needs
// `ptr<mut T>`. Crucially the `mut` belongs to the *pointee*, so it does not
// let the pointer binding itself be reassigned -- `p = q` still needs
// `mut var p`.
// ---------------------------------------------------------------------------

#[test]
fn deref_write_through_ptr_of_mut_type_passes() {
    assert_clean(
        "func f(): i32 {\n    var x: mut i32 = 1i32;\n    var p: ptr<mut i32> = @x;\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}",
    );
}

#[test]
fn deref_write_through_ptr_of_immutable_type_reports() {
    assert_messages(
        "func f(): i32 {\n    var x: i32 = 1i32;\n    var p: ptr<i32> = @x;\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot assign through pointer to immutable type 'p'"],
    );
}

// The decisive case: the *target* is `mut var`, but the write goes through the
// pointee, so `mut var` must not license it. The pointer's declared type is
// what decides.
#[test]
fn deref_write_of_mut_var_target_through_immutable_pointee_reports() {
    assert_messages(
        "func f(): i32 {\n    mut var x: i32 = 1i32;\n    var p: ptr<i32> = @x;\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot assign through pointer to immutable type 'p'"],
    );
}

#[test]
fn deref_write_through_ptr_of_mut_type_from_mut_var_pointer_passes() {
    assert_clean(
        "func f(): i32 {\n    var x: mut i32 = 1i32;\n    mut var p: ptr<mut i32> = @x;\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}",
    );
}

#[test]
fn deref_compound_assign_through_ptr_of_mut_type_passes() {
    assert_clean(
        "func f(): i32 {\n    var x: mut i32 = 1i32;\n    var p: ptr<mut i32> = @x;\n    var v: i32 = marked { ^p += 2i32; 0i32 };\n    return 0i32;\n}",
    );
}

#[test]
fn deref_compound_assign_through_ptr_of_immutable_type_reports() {
    assert_messages(
        "func f(): i32 {\n    var x: i32 = 1i32;\n    var p: ptr<i32> = @x;\n    var v: i32 = marked { ^p += 2i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot add and assign through pointer to immutable type 'p'"],
    );
}

#[test]
fn param_of_ptr_to_mut_type_allows_deref_write() {
    assert_clean("func g(p: ptr<mut i32>): i32 {\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}");
}

#[test]
fn param_of_ptr_to_immutable_type_rejects_deref_write() {
    assert_messages(
        "func g(p: ptr<i32>): i32 {\n    var v: i32 = marked { ^p = 5i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot assign through pointer to immutable type 'p'"],
    );
}

#[test]
fn field_write_through_ptr_to_mut_struct_passes() {
    assert_clean(
        "struct S { p: i32 }\nfunc f(): i32 {\n    var s: mut S = .S{.p = 1};\n    var q: ptr<mut S> = @s;\n    var v: i32 = marked { ^q.p = 7i32; 0i32 };\n    return 0i32;\n}",
    );
}

#[test]
fn field_write_through_ptr_to_immutable_struct_reports() {
    assert_messages(
        "struct S { p: i32 }\nfunc f(): i32 {\n    var s: S = .S{.p = 1};\n    var q: ptr<S> = @s;\n    var v: i32 = marked { ^q.p = 7i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot assign through pointer to immutable type 'q'"],
    );
}

#[test]
fn field_write_through_mut_var_ptr_to_immutable_struct_reports() {
    // `mut var q` permits `q = other`, not a write through the pointee.
    assert_messages(
        "struct S { p: i32 }\nfunc f(): i32 {\n    var s: S = .S{.p = 1};\n    mut var q: ptr<S> = @s;\n    var v: i32 = marked { ^q.p = 7i32; 0i32 };\n    return 0i32;\n}",
        &["Cannot assign through pointer to immutable type 'q'"],
    );
}

// The mirror image: a `mut` on the pointee must not let the pointer itself be
// reassigned. This is what the per-depth tracking exists for.
#[test]
fn rebinding_ptr_to_mut_type_requires_mut_var_pointer() {
    assert_messages(
        "func f(): i32 {\n    var x: mut i32 = 1i32;\n    var y: mut i32 = 2i32;\n    var p: ptr<mut i32> = @x;\n    var q: ptr<mut i32> = @y;\n    p = q;\n    return 0i32;\n}",
        &["Cannot assign to immutable variable 'p'"],
    );
}

#[test]
fn rebinding_mut_var_ptr_to_mut_type_passes() {
    assert_clean(
        "func f(): i32 {\n    var x: mut i32 = 1i32;\n    var y: mut i32 = 2i32;\n    mut var p: ptr<mut i32> = @x;\n    var q: ptr<mut i32> = @y;\n    p = q;\n    return 0i32;\n}",
    );
}

// ---------------------------------------------------------------------------
// Unsuffixed numeric literals coerce to an explicit annotation, element-wise
// for aggregates.
// ---------------------------------------------------------------------------

#[test]
fn unsuffixed_array_literal_coerces_to_annotated_element_type() {
    assert_clean("var a: [i32, 3] = [1, 2, 3];");
}

#[test]
fn unsuffixed_array_literal_coerces_through_mut_element_type() {
    assert_clean("var a: mut [i32, 3] = [1, 2, 3];");
}

#[test]
fn unsuffixed_nested_array_literal_coerces() {
    assert_clean("var a: [[i32, 2], 2] = [[1, 2], [3, 4]];");
}

#[test]
fn unsuffixed_array_literal_length_mismatch_still_reports() {
    // Coercion must not overwrite the literal's own length and hide this.
    assert_messages(
        "var a: [i32, 3] = [1, 2];",
        &["Type mismatch between '[i32,3]' and '[isize,2]'"],
    );
}

#[test]
fn unsuffixed_array_literal_mixed_elements_still_reports() {
    assert_messages(
        "var a: [i32, 3] = [1, true, 3];",
        &[
            "array elements must all have the same type",
            "Type mismatch between '[i32,3]' and 'unknown'",
        ],
    );
}

#[test]
fn negative_literal_coerces_to_annotated_type() {
    assert_clean("var x: i8 = -1;");
}

#[test]
fn negative_literal_in_array_coerces_to_annotated_element_type() {
    assert_clean("var a: [i8, 2] = [300, -1];");
}
