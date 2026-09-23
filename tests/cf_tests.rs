mod common;

use common::{messages, NodeIndex};
use unnc::cf_checker::ControlFlowChecker;

use common::analyze;

/// Runs the full pipeline up to (and including) the ControlFlowChecker and
/// returns every reported diagnostic message.
fn check(src: &str) -> Vec<String> {
    let (mut semantics, diag) = analyze(src, &[]);
    let hir = semantics.generate_monormophizer_hir();
    let index = NodeIndex::build(&hir);
    let mut checker = ControlFlowChecker::new(&index, &semantics.ctxt, diag.clone());
    checker.run();
    messages(&diag)
}

fn assert_clean(src: &str) {
    let msgs = check(src);
    assert!(
        msgs.is_empty(),
        "expected no diagnostics for:\n{src}\ngot: {msgs:?}",
    );
}

fn assert_messages(src: &str, expected: &[&str]) {
    let msgs = check(src);
    let expected: Vec<String> = expected.iter().map(|s| s.to_string()).collect();
    assert_eq!(msgs, expected, "diagnostics mismatch for:\n{src}");
}

// ---------------------------------------------------------------------------
// terminal returns
// ---------------------------------------------------------------------------

#[test]
fn non_unit_function_without_terminal_return_reports() {
    assert_messages(
        "func f(): isize {\n    var x := 1;\n}\n",
        &["function missing  terminal return statement, expected return statement of type 'isize'"],
    );
    assert_messages(
        "func f(): isize {\n}\n",
        &["function missing  terminal return statement, expected return statement of type 'isize'"],
    );
}

#[test]
fn unit_function_bare_return_passes() {
    assert_clean("func f() {\n    return;\n}\n");
}

#[test]
fn implicit_tail_return_is_a_terminal_return() {
    assert_clean("func f(): isize {\n    var x := 1;\n    x\n}\n");
    assert_clean("func f(): isize {\n    42\n}\n");
    // A trailing `;` discards the expression: no tail, so no terminal return.
    assert_messages(
        "func f(): isize {\n    42;\n}\n",
        &["function missing  terminal return statement, expected return statement of type 'isize'"],
    );
}

#[test]
fn tail_return_type_is_checked() {
    assert_messages(
        "func f(): isize {\n    true\n}\n",
        &["Expected type 'isize' but got 'bool'"],
    );
}

#[test]
fn implicit_tail_return_passes_type_matching() {
    assert_clean("func f(): bool {\n    true\n}\n");
}

#[test]
fn valid_return_type_passes() {
    assert_clean("func f(): isize {\n    return 1;\n}\n");
}

#[test]
fn wrong_return_type_reports() {
    assert_messages(
        "func f(): isize {\n    return \"s\";\n}\n",
        &["Expected type 'isize' but got 'str'"],
    );
}

#[test]
fn return_value_in_unit_function_reports() {
    assert_messages(
        "func f() {\n    return \"s\";\n}\n",
        &["Expected type '()' but got 'str'"],
    );
}

#[test]
fn bare_return_in_non_unit_function_reports() {
    assert_messages(
        "func f(): isize {\n    return;\n}\n",
        &["Expected type 'isize' but got '()'"],
    );
}

#[test]
fn conditional_returns_must_cover_else_branch() {
    assert_clean("func f(): isize {\n    if true { return 1; } else { return 2; }\n}\n");
    assert_messages(
        "func f(): isize {\n    if true { return 1; }\n}\n",
        &["function missing  terminal return statement, expected return statement of type 'isize'"],
    );
    assert_messages(
        "func f(): isize {\n    if true { return 1; } else { return \"s\"; }\n}\n",
        &["Expected type 'isize' but got 'str'"],
    );
}

// ---------------------------------------------------------------------------
// unreachable code
// ---------------------------------------------------------------------------

#[test]
fn return_then_return_is_unreachable() {
    assert_messages(
        "func f(): isize {\n    return 1;\n    return 2;\n}\n",
        &["Unreachable code"],
    );
}

#[test]
fn statement_after_return_is_unreachable() {
    assert_messages(
        "func f(): isize {\n    return 1;\n    var x := 2;\n}\n",
        &["Unreachable code"],
    );
}

// ---------------------------------------------------------------------------
// break / continue placement
// ---------------------------------------------------------------------------

#[test]
fn break_and_continue_inside_loop_passes() {
    assert_clean(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; if x == 3 { break } }\n    return x;\n}\n",
    );
    assert_clean(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; continue }\n    return x;\n}\n",
    );
    assert_clean("func f(): isize {\n    while true { break; }\n    return 0;\n}\n");
    assert_clean(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; continue; }\n    return x;\n}\n",
    );
}

#[test]
fn break_outside_loop_reports() {
    assert_messages(
        "func f(): isize {\n    break\n    return 0;\n}\n",
        &["break statements must only exist inside a loop body"],
    );
    assert_messages(
        "func f(): isize {\n    continue\n    return 0;\n}\n",
        &["continue statements must only exist inside a loop body"],
    );
}

#[test]
fn break_inside_nested_loops_passes() {
    // The inner break exits only the inner loop; the outer `break` then exits
    // the outer loop, so the epilogue is reachable.
    assert_clean(
        "func f(): isize {\n    while true { while true { break } break }\n    return 0;\n}\n",
    );
    // With no outer break the `while true` never stops, so the epilogue is dead.
    assert_messages(
        "func f(): isize {\n    while true { while true { break } }\n    return 0;\n}\n",
        &[
            "Unreachable code",
            "function missing  terminal return statement, expected return statement of type 'isize'",
        ],
    );
}

#[test]
fn break_inside_if_inside_loop_passes() {
    assert_clean(
        "func f(): isize {\n    mut var x := 0;\n    while true { if true { break } }\n    return x;\n}\n",
    );
}

// ---------------------------------------------------------------------------
// top-level flow statements are rejected (they only make sense in a function)
// ---------------------------------------------------------------------------

#[test]
fn top_level_return_reports() {
    assert_messages(
        "func f(): isize {\n    return 1;\n}\nreturn 7;\n",
        &["Return statements must only exist in a function body"],
    );
}

#[test]
fn top_level_break_and_continue_report() {
    assert_messages(
        "var a := 5;\nbreak\n",
        &["break statements must only exist inside a loop body"],
    );
    assert_messages(
        "var a := 5;\ncontinue\n",
        &["continue statements must only exist inside a loop body"],
    );
    assert_messages(
        "var a := 5;\nbreak;\n",
        &["break statements must only exist inside a loop body"],
    );
    assert_messages(
        "var a := 5;\ncontinue;\n",
        &["continue statements must only exist inside a loop body"],
    );
}

#[test]
fn top_level_loop_flow_is_valid() {
    assert_clean("while true {\n    break\n}\n");
}

// ---------------------------------------------------------------------------
// constant-true loops
// ---------------------------------------------------------------------------

#[test]
fn while_true_with_return_satisfies_terminal_return() {
    assert_clean("func f(): isize {\n    while true { return 1; }\n}\n");
}

#[test]
fn while_true_returns_then_epilogue_is_unreachable() {
    assert_messages(
        "func f(): isize {\n    while true { return 1; }\n    return 2;\n}\n",
        &["Unreachable code"],
    );
}

#[test]
fn while_true_with_reachable_break_does_not_diverge() {
    assert_clean("func f(): isize {\n    while true { break }\n    return 0;\n}\n");
    assert_clean(
        "func f(x: bool): isize {\n    while true { if x { break } else { return 1; } }\n    return 0;\n}\n",
    );
}

#[test]
fn while_true_without_return_still_needs_terminal_return() {
    assert_messages(
        "func f(): isize {\n    while true { }\n}\n",
        &["function missing  terminal return statement, expected return statement of type 'isize'"],
    );
}

#[test]
fn break_and_continue_make_following_statements_unreachable() {
    assert_messages(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; break x = 7; }\n    return x;\n}\n",
        &["Unreachable code"],
    );
    assert_messages(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; continue x = 7; }\n    return x;\n}\n",
        &["Unreachable code"],
    );
}

#[test]
fn code_after_non_terminating_loop_branch_is_reachable() {
    assert_clean(
        "func f(): isize {\n    mut var x := 0;\n    while x < 3 { x += 1; break }\n    return x;\n}\n",
    );
}

// ---------------------------------------------------------------------------
// dollar scopes are their own function contexts
// ---------------------------------------------------------------------------

#[test]
fn dollar_scope_loop_flow_passes() {
    assert_clean(
        "func f(): isize {\n    var a := $${\n        var x := 0;\n        while true { break }\n        x;\n    };\n    return 0;\n}\n",
    );
}

#[test]
fn dollar_scope_break_outside_loop_reports() {
    assert_messages(
        "func f(): isize {\n    var a := $${\n        break\n    };\n    return 0;\n}\n",
        &["break statements must only exist inside a loop body"],
    );
}

#[test]
fn dollar_scope_return_must_match_scope_type() {
    assert_messages(
        "func f(): isize {\n    var a := $${\n        return 5;\n    };\n    return 0;\n}\n",
        &["Expected type '()' but got 'isize'"],
    );
}

#[test]
fn dollar_scope_unreachable_after_return() {
    assert_messages(
        "func f(): isize {\n    var a := $${\n        return 1;\n        var x := 2;\n    };\n    return 0;\n}\n",
        &["Expected type '()' but got 'isize'", "Unreachable code"],
    );
}

// ---------------------------------------------------------------------------
// unsuffixed numeric literals take the function's return type
// ---------------------------------------------------------------------------

#[test]
fn unsuffixed_int_literal_returns_match_return_type() {
    assert_clean("func foo(): i32 {\n    return 0;\n}\n");
    assert_clean("func foo(): i64 {\n    return 0;\n}\n");
    assert_clean("func foo(): u32 {\n    return 5;\n}\n");
}

#[test]
fn unsuffixed_float_literal_returns_match_return_type() {
    assert_clean("func foo(): f32 {\n    return 1.5;\n}\n");
    assert_clean("func foo(): f64 {\n    return 1.5;\n}\n");
}

#[test]
fn suffixed_literal_returns_still_match() {
    assert_clean("func foo(): i32 {\n    return 0i32;\n}\n");
    assert_clean("func foo(): i64 {\n    return 0i64;\n}\n");
}

#[test]
fn non_literal_return_mismatch_still_reports() {
    assert_messages(
        "func foo(): i32 {\n    var x := 0;\n    return x;\n}\n",
        &["Expected type 'i32' but got 'isize'"],
    );
}
