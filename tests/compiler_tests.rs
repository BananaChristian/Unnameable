use std::cell::RefCell;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use unnc::bc_builder::BytecodeBuilder;
use unnc::bc_builder::BytecodePrinter;
use unnc::{
    const_and_mut_validator::Validator, diagnostics::Diagnostics, import::ImportEngine,
    indexer::NodeIndex, lexer::Lexer, lowering::Lowering, mir::MIRBuilder, parser::Parser,
    semantics::Semantics, target::TargetSpec,
};

fn capture_diagnostics(diagnostics: &Rc<RefCell<Diagnostics>>) -> String {
    let diag = diagnostics.borrow();
    let mut output = String::new();

    output.push_str("=== COMPILATION FAILED ===\n");

    if !diag.errors.is_empty() {
        output.push_str(&format!("Errors ({}):\n", diag.errors.len()));
        for err in &diag.errors {
            output.push_str(&format!("  - {:?}\n", err));
        }
    }

    if !diag.warnings.is_empty() {
        output.push_str(&format!("Warnings ({}):\n", diag.warnings.len()));
        for warn in &diag.warnings {
            output.push_str(&format!("  - {:?}\n", warn));
        }
    }

    output
}

fn compile_source_for_test(filename: &str, source: &str) -> String {
    let result = panic::catch_unwind(|| {
        let module_name = "test_module".to_string();
        let target_spec = TargetSpec::new(None, None, None, None);

        let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
            filename.to_string(),
            source.to_string(),
        )));

        let mut lexer = Lexer::new(source, Rc::clone(&diagnostics));
        let tokens = lexer.tokenize();
        if lexer.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mut parser = Parser::new(tokens, Rc::clone(&diagnostics));
        let ast = parser.parse();
        if parser.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mut lowering = Lowering::new(ast, Rc::clone(&diagnostics));
        let mut hir = lowering.lower();
        if lowering.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mut importer = ImportEngine::new(Rc::clone(&diagnostics));
        let empty_stubs: Vec<String> = Vec::new();
        importer.import(&mut hir, &empty_stubs);
        if importer.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mut semantics = Semantics::new(hir, &target_spec);
        semantics.analyze(Rc::clone(&diagnostics), &importer);
        if semantics.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let monomorphized_hir = semantics.generate_monormophizer_hir();
        let hir_index = NodeIndex::build(&monomorphized_hir);

        if semantics.verify_contracts(&hir_index, Rc::clone(&diagnostics))
            || semantics.check_control_flow(&hir_index, Rc::clone(&diagnostics))
        {
            return capture_diagnostics(&diagnostics);
        }

        let mut validator = Validator::new(Rc::clone(&diagnostics));
        validator.run(&monomorphized_hir);
        if validator.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mut mir_builder = MIRBuilder::new(
            &hir_index,
            &semantics.ctxt.types,
            &target_spec,
            Rc::clone(&diagnostics),
            module_name,
        );

        if mir_builder.corrupted {
            return capture_diagnostics(&diagnostics);
        }

        let mir_module = mir_builder.build_module();
        let mir = format!("=== MIR OUTPUT ===\n{}", mir_module);

        let mut bc_builder = BytecodeBuilder::new(&mir_module, diagnostics);
        let bytecode = bc_builder.build();
        let bytecode = BytecodePrinter::print_module(&bytecode);
        let result = format!("{}\n{}", mir, bytecode);
        result
    });

    match result {
        Ok(output) => output,
        Err(err) => {
            let panic_msg = if let Some(s) = err.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = err.downcast_ref::<String>() {
                s.clone()
            } else {
                "Unknown panic occurred".to_string()
            };
            format!("=== PANIC IN COMPILER PASS ===\n{}", panic_msg)
        }
    }
}

fn find_all_fixtures(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(find_all_fixtures(&path));
            } else if path.extension().and_then(|s| s.to_str()) == Some("unn") {
                files.push(path);
            }
        }
    }
    files
}

#[test]
fn test_32bit_pointer_width_layouts() {
    let source = "func get_ptr_size(): usize { return 0 }";

    let target_spec = TargetSpec::new(Some("arm".into()), Some("none".into()), Some(4), Some(4));

    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "test.unn".into(),
        source.into(),
    )));
    let mut lexer = Lexer::new(source, Rc::clone(&diagnostics));
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens, Rc::clone(&diagnostics));
    let ast = parser.parse();
    let mut lowering = Lowering::new(ast, Rc::clone(&diagnostics));
    let hir = lowering.lower();

    let _semantics = Semantics::new(hir, &target_spec);
    assert_eq!(target_spec.pointer_width, 4);
}

#[test]
fn test_all_fixtures() {
    let fixtures_dir = Path::new("tests/fixtures");
    let mut entries = find_all_fixtures(fixtures_dir);
    entries.sort();

    assert!(
        !entries.is_empty(),
        "No .unn fixture files found in tests/fixtures!"
    );

    for path in entries {
        println!("Running fixture test for: {:?}", path);
        let source = fs::read_to_string(&path).unwrap();
        let filename = path.file_name().unwrap().to_str().unwrap();

        let output = compile_source_for_test(filename, &source);

        let test_name = path.file_stem().unwrap().to_str().unwrap();
        insta::assert_snapshot!(test_name, output);
    }
}

#[test]
fn bytecode_output_is_deterministic_for_a_module_with_several_globals() {
    // Regression, and a flaky-test fix rather than a feature test.
    //
    // `BytecodePrinter` walked `module.globals` in whatever order the module was
    // assembled in, which is not stable across runs. A module with two or more
    // globals therefore produced snapshot text whose *line order* changed from
    // run to run, so the fixture suite failed intermittently -- and a test that
    // fails one run in five is worse than no test, because it trains you to
    // re-run. No existing fixture had two globals until
    // `mut/const_binding_mut_type.unn` did, which is what finally exposed it.
    //
    // Globals are now printed in `GlobalId` order, so the output depends only on
    // the ids and not on the vec's provenance. Compiling the same source twice
    // must now be byte-identical.
    let source =
        "const var x: i32 = 1i32;\nconst var y: i32 = 2i32;\nfunc f(): i32 { return x + y; }\n";
    let first = compile_source_for_test("globals.unn", source);
    for _ in 0..8 {
        assert_eq!(
            first,
            compile_source_for_test("globals.unn", source),
            "bytecode output must be deterministic across compilations"
        );
    }
    // Guard against the test passing vacuously: both globals must be printed.
    assert!(
        first.contains("\"x\""),
        "expected global x in bytecode output, got:\n{first}"
    );
    assert!(
        first.contains("\"y\""),
        "expected global y in bytecode output, got:\n{first}"
    );
}

#[test]
fn pointer_cannot_be_returned_as_a_reference() {
    // The forgery through a return type. This check lives in the control-flow
    // checker, not the type checker, so it needs the full pipeline -- hence being
    // here rather than in `typechecker_tests`. `return @x;` remains legal; only
    // returning a `ptr` *value* as a `ref` is refused.
    let out = compile_source_for_test(
        "forged_ref_return.unn",
        "func mk(p: ptr<i32>): ref<i32> { return p; }\nfunc main(): i32 { return 0i32; }\n",
    );
    assert!(
        out.contains("Expected type 'ref<i32>' but got 'ptr<i32>'"),
        "expected the forged return to be refused, got:\n{out}"
    );
}

#[test]
fn address_of_is_returnable_as_a_reference() {
    let out = compile_source_for_test(
        "ref_return.unn",
        "func mk(): ref<i32> { var x: i32 = 7i32; return @x; }\nfunc main(): i32 { return 0i32; }\n",
    );
    assert!(
        !out.contains("COMPILATION FAILED"),
        "returning an address as a ref should compile, got:\n{out}"
    );
}

#[test]
fn impl_members_accept_qualifiers_so_they_can_be_dollar_marked() {
    // Regression, found by auditing the method work against AGENTS.md 2.10.
    //
    // `parse_impl` called `parse_func` directly, so a member could never carry a
    // qualifier. A method is emitted as a plain `Counter_get` with no dollar
    // marker, and the dollar verifier then refuses every attempt to call it from
    // a scope -- so `impl` methods were entirely unreachable in the VM, even
    // though the byte-identical hand-written `$ func Counter_get(self: Counter)`
    // executed fine. Methods were only ever verified natively as a result.
    let out = compile_source_for_test(
        "impl_qualifier.unn",
        "struct Counter { n: i32 }\n\
         impl Counter {\n  $ func get(self: Counter): i32 { return self.n; }\n}\n\
         func main(): i32 { var c = .Counter{.n = 1i32}; return c.get(); }\n",
    );
    assert!(
        !out.contains("COMPILATION FAILED") && !out.contains("Expected Func"),
        "an `impl` member must accept a `$` qualifier, got:\n{out}"
    );
}

#[test]
fn impl_still_rejects_a_bodyless_member() {
    // The qualifier change must not have weakened the body requirement: a
    // contract-style signature inside an `impl` would satisfy the contract
    // verifier by name while doing nothing.
    let out = compile_source_for_test(
        "impl_bodyless.unn",
        "struct P { x: i32 }\nimpl P { func sum(self: P): i32 }\n",
    );
    assert!(
        out.contains("Only function definitions are allowed in an impl block"),
        "expected the bodyless-member report, got:\n{out}"
    );
}

#[test]
fn qualified_impl_block_lowers_identically_to_a_hand_written_function() {
    // The load-bearing property, and the one worth being pedantic about: a
    // qualifier on an `impl` block must not leave a trace. `$ impl C { func f }`
    // has to produce the same MIR, byte for byte, as `$ func C_f(...)`. If a
    // block ever lowered to a distinct node, or carried extra structure, the
    // middle and backends would learn that methods exist -- which they must not.
    let via_qualifier = compile_source_for_test(
        "qualified_impl.unn",
        "struct C { n: i32 }\n\
         $ impl C {\n  func f(self: C): i32 { return self.n; }\n}\n\
         func main(): i32 { var c = .C{.n = 1i32}; return c.f(); }\n",
    );
    let by_hand = compile_source_for_test(
        "hand_qualified.unn",
        "struct C { n: i32 }\n\
         $ func C_f(self: C): i32 { return self.n; }\n\
         func main(): i32 { var c = .C{.n = 1i32}; return C_f(c); }\n",
    );
    assert_eq!(
        via_qualifier, by_hand,
        "a qualified impl block must lower to exactly a hand-written qualified function"
    );
}

#[test]
fn extern_qualifier_on_an_impl_block_reaches_its_members() {
    let via_block = compile_source_for_test(
        "extern_impl.unn",
        "struct C { n: i32 }\n\
         extern impl C {\n  func f(self: C): i32 { return self.n; }\n}\n\
         func main(): i32 { var c = .C{.n = 1i32}; return c.f(); }\n",
    );
    let by_hand = compile_source_for_test(
        "hand_extern.unn",
        "struct C { n: i32 }\n\
         extern func C_f(self: C): i32 { return self.n; }\n\
         func main(): i32 { var c = .C{.n = 1i32}; return C_f(c); }\n",
    );
    assert_eq!(
        via_block, by_hand,
        "an `extern` impl block must lower to a hand-written extern function"
    );
    assert!(
        via_block.contains("{c}"),
        "expected the extern-C convention marker on the emitted function, got:\n{via_block}"
    );
}

#[test]
fn block_and_member_qualifiers_both_apply() {
    // `$ impl` marks every member; a member's own qualifier applies to that
    // member. They merge rather than clobber, so `impl { $ func ... }` keeps its
    // own `$` even though the block supplies none.
    let out = compile_source_for_test(
        "impl_qualifier_mix.unn",
        "struct C { n: i32 }\n\
         $ impl C {\n  func a(self: C): i32 { return self.n; }\n  \
         $ func b(self: C): i32 { return self.n; }\n}\n\
         func main(): i32 { var c = .C{.n = 1i32}; return c.a() + c.b(); }\n",
    );
    assert!(
        !out.contains("COMPILATION FAILED"),
        "block and member qualifiers must both be accepted, got:\n{out}"
    );
    // Both members are dollar-marked, since the block said so.
    assert_eq!(
        out.matches("$func @C_a").count(),
        1,
        "block `$` should reach member a:\n{out}"
    );
    assert_eq!(
        out.matches("$func @C_b").count(),
        1,
        "member's own `$` should survive:\n{out}"
    );
}

#[test]
fn qualified_seal_block_lowers_identically_to_a_hand_written_function() {
    // `seal` had the same defect `impl` had, in the same two places, and for the
    // same reason: `parse_seal` called `parse_func` directly so a member could
    // never take a qualifier, and `lower_seals` copied `exposed` and `conv` from
    // the block but **not** `dollar_read` -- so a `$ seal` could not make its
    // members dollar-callable however it was written. A seal was native-only for
    // the same reason methods were.
    let via_block = compile_source_for_test(
        "qualified_seal.unn",
        "$ seal Math {\n  func twice(v: i32): i32 { return v * 2i32; }\n}\n\
         func main(): i32 { return Math_twice(21i32); }\n",
    );
    let by_hand = compile_source_for_test(
        "hand_seal.unn",
        "$ func Math_twice(v: i32): i32 { return v * 2i32; }\n\
         func main(): i32 { return Math_twice(21i32); }\n",
    );
    assert_eq!(
        via_block, by_hand,
        "a qualified seal block must lower to exactly a hand-written qualified function"
    );
}

#[test]
fn seal_member_keeps_its_own_dollar_qualifier() {
    // Merging, not overwriting: the seal supplies no qualifier, so a member's own
    // `$` must survive rather than being clobbered.
    let out = compile_source_for_test(
        "seal_member_qualifier.unn",
        "seal Math {\n  $ func twice(v: i32): i32 { return v * 2i32; }\n}\n\
         func main(): i32 { return Math_twice(21i32); }\n",
    );
    assert!(
        !out.contains("COMPILATION FAILED") && !out.contains("Expected Func"),
        "a seal member must accept a `$` qualifier, got:\n{out}"
    );
    assert!(
        out.contains("$func @Math_twice"),
        "expected the member to be dollar-marked, got:\n{out}"
    );
}

#[test]
fn extern_seal_block_lowers_identically_to_a_hand_written_function() {
    let via_block = compile_source_for_test(
        "extern_seal.unn",
        "extern seal Math {\n  func twice(v: i32): i32 { return v * 2i32; }\n}\n\
         func main(): i32 { return Math_twice(21i32); }\n",
    );
    let by_hand = compile_source_for_test(
        "hand_extern_seal.unn",
        "extern func Math_twice(v: i32): i32 { return v * 2i32; }\n\
         func main(): i32 { return Math_twice(21i32); }\n",
    );
    assert_eq!(
        via_block, by_hand,
        "an `extern` seal block must lower to a hand-written extern function"
    );
}

#[test]
fn seal_still_rejects_a_bodyless_member() {
    // The qualifier change must not have weakened the body requirement.
    let out = compile_source_for_test(
        "seal_bodyless.unn",
        "seal Math {\n  func twice(v: i32): i32\n}\n",
    );
    assert!(
        out.contains("Only function definitions are allowed in a seal"),
        "expected the bodyless-member report, got:\n{out}"
    );
}
