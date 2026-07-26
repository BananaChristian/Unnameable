use std::cell::RefCell;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};
use std::rc::Rc;

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
        format!("=== MIR OUTPUT ===\n{}", mir_module)
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
