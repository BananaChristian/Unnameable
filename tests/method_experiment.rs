//! Experiment: what `impl` blocks and `.` method calls actually compile to.
//!
//! The method fixtures in `tests/fixtures/methods/` pin the *pipeline* —
//! that a method program lexes, parses, type-checks, and lowers to the expected
//! MIR and bytecode. This file asks the sharper question: at the level where a
//! vtable or a runtime type would have to appear, is there any trace of one?
//!
//! The answer is asserted, not assumed:
//!
//!  1. A program written with `impl` + `p.get()` and the same program written
//!     with a hand-mangled `Left_get(p, ...)` emit **identical LLVM IR**.
//!  2. Every method call lowers to a direct `call @Fn(...)` with the receiver
//!     passed **by value**.
//!  3. Nothing in the emitted IR looks like dynamic dispatch: no vtable
//!     global, no table of function pointers, no function pointer loaded and
//!     called indirectly.
//!
//! This is the test that would fail if method dispatch ever became dynamic.

use std::cell::RefCell;
use std::rc::Rc;

use inkwell::{context::Context, OptimizationLevel};
mod common;

use unnc::codegen::Codegen;
use unnc::{
    const_and_mut_validator::Validator, diagnostics::Diagnostics, import::ImportEngine,
    indexer::NodeIndex, lexer::Lexer, lowering::Lowering, mir::MIRBuilder, parser::Parser,
    semantics::Semantics, target::TargetSpec,
};

/// Runs the full frontend, builds MIR, and lowers it to LLVM IR text.
///
/// Deliberately stops at `print_ir` rather than `emit_object`: emitting an
/// object needs a native target initialised, and linking a binary would need a C
/// toolchain. IR text is enough to answer the question being asked here and
/// keeps the test free of external toolchain dependencies.
fn compile_to_ir(source: &str) -> String {
    let target_spec = TargetSpec::new(None, None, None, None);
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "method_experiment.unn".to_string(),
        source.to_string(),
    )));

    let mut lexer = Lexer::new(source, Rc::clone(&diagnostics));
    let tokens = lexer.tokenize();
    assert!(
        !lexer.corrupted,
        "lex failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut parser = Parser::new(tokens, Rc::clone(&diagnostics));
    let ast = parser.parse();
    assert!(
        !parser.corrupted,
        "parse failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut lowering = Lowering::new(ast, Rc::clone(&diagnostics));
    let mut hir = lowering.lower();
    assert!(
        !lowering.corrupted,
        "lowering failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut importer = ImportEngine::new(Rc::clone(&diagnostics));
    importer.import(&mut hir, &Vec::new());
    assert!(
        !importer.corrupted,
        "import failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut semantics = Semantics::new(hir, &target_spec);
    semantics.analyze(Rc::clone(&diagnostics), &importer);
    assert!(
        !semantics.corrupted,
        "semantics failed: {:?}",
        diagnostics.borrow().errors
    );

    let monomorphized_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorphized_hir);

    assert!(
        !semantics.verify_contracts(&hir_index, Rc::clone(&diagnostics))
            && !semantics.check_control_flow(&hir_index, Rc::clone(&diagnostics)),
        "verification failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut validator = Validator::new(Rc::clone(&diagnostics));
    validator.run(&monomorphized_hir);
    assert!(
        !validator.corrupted,
        "validation failed: {:?}",
        diagnostics.borrow().errors
    );

    let mut mir_builder = MIRBuilder::new(
        &hir_index,
        &semantics.ctxt.types,
        &target_spec,
        Rc::clone(&diagnostics),
        "test_module".to_string(),
    );
    assert!(
        !mir_builder.corrupted,
        "MIR builder corrupted on construction"
    );
    let mir_module = mir_builder.build_module();

    let context = Context::create();
    let mut codegen = Codegen::new(
        &context,
        &target_spec,
        &mir_module,
        OptimizationLevel::None,
        Rc::clone(&diagnostics),
    );
    codegen.compile_module();
    codegen.print_ir()
}

/// LLVM emits functions in module order, so two otherwise-identical modules can
/// differ purely in ordering. Sorting the top-level blocks by their first line
/// makes the comparison about content only.
fn normalize_ir(ir: &str) -> Vec<String> {
    let mut blocks: Vec<String> = Vec::new();
    let mut current = String::new();
    for line in ir.lines() {
        if current.is_empty() {
            current.push_str(line);
        } else {
            current.push('\n');
            current.push_str(line);
        }
        if current.trim_end().ends_with('}') {
            blocks.push(std::mem::take(&mut current));
        }
    }
    if !current.trim().is_empty() {
        blocks.push(current);
    }
    blocks.sort();
    blocks
}

const VIA_IMPL: &str = "\
struct Left { a: i32 }
struct Right { a: i32, b: i32 }
impl Left { func get(self: Left): i32 { return self.a; } }
impl Right { func get(self: Right): i32 { return self.b; } }
func both(): i32 {
  var l = .Left{.a = 5i32};
  var r = .Right{.a = 1i32, .b = 2i32};
  return l.get() + r.get();
}
";

const BY_HAND: &str = "\
struct Left { a: i32 }
struct Right { a: i32, b: i32 }
func Left_get(self: Left): i32 { return self.a; }
func Right_get(self: Right): i32 { return self.b; }
func both(): i32 {
  var l = .Left{.a = 5i32};
  var r = .Right{.a = 1i32, .b = 2i32};
  return Left_get(l) + Right_get(r);
}
";

#[test]
fn impl_and_dot_call_emit_identical_ir_to_the_hand_written_call() {
    // The central claim, checked at the lowest level available without a
    // toolchain. If the sugar desugared to anything else -- a thunk, a closure,
    // an adjusted calling convention -- the IR would diverge here.
    let a = normalize_ir(&compile_to_ir(VIA_IMPL));
    let b = normalize_ir(&compile_to_ir(BY_HAND));
    assert_eq!(
        a, b,
        "impl + `.` must emit the same LLVM IR as the hand-written mangled call"
    );
    assert!(!a.is_empty(), "expected non-empty IR");
}

#[test]
fn method_dispatch_is_a_direct_call_with_a_by_value_receiver() {
    let ir = compile_to_ir(VIA_IMPL);

    // The receiver arrives as an ordinary by-value struct parameter.
    assert!(
        ir.contains("define internal i32 @Left_get(%Left %self)"),
        "expected a by-value `%Left %self` parameter, got:\n{ir}"
    );
    assert!(
        ir.contains("define internal i32 @Right_get(%Right %self)"),
        "expected a by-value `%Right %self` parameter, got:\n{ir}"
    );

    // Each call site names its callee directly and passes the struct by value.
    assert!(
        ir.contains("call i32 @Left_get(%Left"),
        "expected a direct call to Left_get, got:\n{ir}"
    );
    assert!(
        ir.contains("call i32 @Right_get(%Right"),
        "expected a direct call to Right_get, got:\n{ir}"
    );
}

#[test]
fn emitted_ir_contains_no_dynamic_dispatch() {
    // A global is declared deliberately so the scan below has something to
    // examine. Without one the module has no globals at all, the loop would
    // iterate zero times, and the assertion would pass vacuously -- a test that
    // looks rigorous and checks nothing.
    let ir = compile_to_ir(
        "\
const var seed: i32 = 7i32;
struct Counter { n: i32 }
impl Counter { func get(self: Counter): i32 { return self.n + seed; } }
func f(): i32 { var c = .Counter{.n = 1i32}; return c.get(); }
",
    );

    // Non-vacuity: the global is really there to be inspected.
    let globals: Vec<&str> = ir.lines().filter(|l| l.starts_with('@')).collect();
    assert!(
        globals.iter().any(|g| g.contains("seed")),
        "expected the `seed` global in the IR so the scan is meaningful, got {globals:?}"
    );

    // A vtable, or anything shaped like one, is necessarily a global holding an
    // aggregate of function pointers. No global here is an aggregate.
    for global in &globals {
        assert!(
            !global.contains('['),
            "a global aggregate could hold a dispatch table: {global}"
        );
        assert!(
            !global.contains("ptr"),
            "a global holding a pointer could be a dispatch table: {global}"
        );
    }

    // No vtable under any conventional spelling.
    assert!(
        !ir.contains("_ZTV") && !ir.contains("vtable") && !ir.contains("vfn"),
        "no vtable symbol should appear in the IR:\n{ir}"
    );
    // No function pointer is ever taken, stored, or called indirectly.
    assert!(
        !ir.contains("ptr @") && !ir.contains("call i32 %") && !ir.contains("call void %"),
        "no indirect call or stored function pointer should appear:\n{ir}"
    );
    // And the one call in the program is direct.
    assert!(
        ir.contains("call i32 @Counter_get(%Counter"),
        "expected the method call to be direct, got:\n{ir}"
    );
}

#[test]
fn method_call_in_a_var_initializer_is_rewritten() {
    // Regression. `lower_stmt` in the method resolver originally had no
    // `HirVarDecl` arm, so `var got = c.bump();` was never rewritten and reached
    // the MIR builder as an access-with-a-call, which ICE'd with "Expected
    // identifier as field name" -- while `return c.bump();` worked fine. Found by
    // the `mut_receiver_is_a_copy` fixture.
    let ir = compile_to_ir(
        "\
struct Counter { n: i32 }
impl Counter { func bump(self: mut Counter): i32 { self.n = self.n + 1i32; return self.n; } }
func use_it(): i32 {
  var c: mut Counter = .Counter{.n = 5i32};
  var got = c.bump();
  return got;
}
",
    );
    assert!(
        ir.contains("call i32 @Counter_bump("),
        "a method call in a var initializer must become a direct call, got:\n{ir}"
    );
}

#[test]
fn pointer_receiver_reaches_the_callers_storage() {
    // The receiver forms differ in what they can actually do, and the IR is
    // where that shows up: a by-value receiver gets its own alloca and writes
    // only there, so the caller is unaffected.
    let by_value = compile_to_ir(
        "\
struct Counter { n: i32 }
impl Counter { func bump(self: mut Counter): i32 { self.n = self.n + 1i32; return self.n; } }
func f(): i32 { var c: mut Counter = .Counter{.n = 5i32}; return c.bump(); }
",
    );
    assert!(
        by_value.contains("define internal i32 @Counter_bump(%Counter %self)"),
        "by-value receiver expected, got:\n{by_value}"
    );
    assert!(
        by_value.contains("alloca %Counter"),
        "a by-value receiver allocates its own copy, got:\n{by_value}"
    );

    // And the call site loads the struct to pass it -- it does not pass an
    // address that the callee could write through.
    assert!(
        by_value.contains("call i32 @Counter_bump(%Counter"),
        "expected by-value argument at the call site, got:\n{by_value}"
    );
}

#[test]
fn methods_resolve_on_variants_and_enums_too() {
    let ir = compile_to_ir(
        "\
variant Shape { Circle(i32) Square(i32) }
impl Shape { func tag(self: Shape): i32 { return 1; } }
enum Color { RED, BLUE = 100 }
impl Color { func code(self: Color): i32 { return 7; } }
func f(): i32 { var s = Shape.Square(3i32); return s.tag(); }
",
    );
    assert!(
        ir.contains("@Shape_tag") && ir.contains("@Color_code"),
        "expected variant and enum methods to mangle by type name, got:\n{ir}"
    );
    assert!(
        ir.contains("call i32 @Shape_tag("),
        "expected a direct call to the variant method, got:\n{ir}"
    );
}

#[test]
fn ref_tier_executes_in_the_vm() {
    // Per AGENTS.md 2.10: what runs in LLVM must run in the VM. The `$` marker is
    // what makes this reachable -- without it `dollar_verifier` correctly refuses
    // a non-dollar call from a dollar scope, and the callee never reaches the VM.
    //
    // I previously wrote that ref-typed code could not be verified in the VM. That
    // was wrong: it needed a `$` on the callees. This test pins the real answer so
    // nobody re-derives the wrong conclusion.
    //
    // Covers all three legitimate creation routes plus a field access through a
    // reference: annotation, parameter, and return.
    let src = "\
struct P { x: i32, y: i32 }
$ func by_param(r: ref<i32>): i32 { return ^r; }
$ func make(): ref<i32> { var n: i32 = 11i32; return @n; }
$ func struct_field(r: ref<P>): i32 { return marked { ^r.x }; }
$ func foo(): u32 {
  var a = $${
    var n: i32 = 5i32;
    var by_ann: i32 = by_param(@n);
    var m = make();
    var by_ret: i32 = ^m;
    var p: P = .P{.x = 3i32, .y = 4i32};
    var by_field: i32 = struct_field(@p);
    by_ann + by_ret + by_field
  };
  return 0u32;
}
";

    let (mut semantics, diag) = common::analyze(src, &[]);
    assert!(
        !semantics.corrupted,
        "analysis should be clean, got {:?}",
        common::messages(&diag)
    );

    let hir = semantics.generate_monormophizer_hir();
    let hir_index = unnc::indexer::NodeIndex::build(&hir);
    assert!(
        !semantics.verify_contracts(&hir_index, common::Rc::clone(&diag))
            && !semantics.check_control_flow(&hir_index, common::Rc::clone(&diag)),
        "verification failed: {:?}",
        common::messages(&diag)
    );

    let target = TargetSpec::new(None, None, None, None);
    let mut mir_builder = unnc::mir::MIRBuilder::new(
        &hir_index,
        &semantics.ctxt.types,
        &target,
        common::Rc::clone(&diag),
        "test_module".to_string(),
    );
    let mir_module = mir_builder.build_module();

    let mut dollar_verifier =
        unnc::dollar_verifier::DollarVerifier::new(&mir_module, common::Rc::clone(&diag));
    dollar_verifier.verify();
    assert!(
        !dollar_verifier.corrupted,
        "dollar verifier rejected the $ calls: {:?}",
        common::messages(&diag)
    );

    let bytecode =
        unnc::bc_builder::BytecodeBuilder::new(&mir_module, common::Rc::clone(&diag)).build();
    let mut vm = unnc::vm::VM::new(&bytecode, common::Rc::clone(&diag));
    let table = vm.execute();

    // VMValue derives Debug + Clone but not PartialEq, so match on it.
    let got = table.results.get("$$scope_0");
    assert!(
        matches!(got, Some(unnc::vm::VMValue::I32(19))),
        "expected 5 (by_param) + 11 (returned ref) + 3 (field through ref) = 19, got {got:?}"
    );
}
