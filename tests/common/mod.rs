pub use std::{cell::RefCell, rc::Rc};

pub use unnc::diagnostics::Diagnostics;
pub use unnc::hir::HirStmt;
pub use unnc::import::ImportEngine;
pub use unnc::indexer::NodeIndex;
pub use unnc::lexer::Lexer;
pub use unnc::lowering::Lowering;
pub use unnc::parser::Parser;
pub use unnc::semantics::Semantics;
pub use unnc::serializer::Serializer;
pub use unnc::target::TargetSpec;

pub fn messages(diag: &Rc<RefCell<Diagnostics>>) -> Vec<String> {
    diag.borrow()
        .errors
        .iter()
        .map(|e| e.message.clone())
        .collect()
}

pub fn pipe_lowered(src: &str) -> (Vec<HirStmt>, Rc<RefCell<Diagnostics>>) {
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "test.unn".to_string(),
        src.to_string(),
    )));
    let mut lexer = Lexer::new(src, diagnostics.clone());
    let tokens = lexer.tokenize();
    assert!(!lexer.corrupted, "lexer corrupted for:\n{src}");
    let mut parser = Parser::new(tokens, diagnostics.clone());
    let ast = parser.parse();
    assert!(!parser.corrupted, "parser corrupted for:\n{src}");
    let mut lowering = Lowering::new(ast, diagnostics.clone());
    let hir = lowering.lower();
    assert!(!lowering.corrupted, "lowering corrupted for:\n{src}");
    (hir, diagnostics)
}

pub fn pipe_imported(
    src: &str,
    stub_paths: &[&str],
) -> (Vec<HirStmt>, ImportEngine, Rc<RefCell<Diagnostics>>) {
    let (hir, diag) = pipe_lowered(src);
    let mut hir = hir;
    let mut importer = ImportEngine::new(diag.clone());
    let paths: Vec<String> = stub_paths.iter().map(|s| s.to_string()).collect();
    importer.import(&mut hir, &paths);
    (hir, importer, diag)
}

pub fn analyze(src: &str, stub_paths: &[&str]) -> (Semantics<'static>, Rc<RefCell<Diagnostics>>) {
    let (hir, importer, diag) = pipe_imported(src, stub_paths);
    let target: &'static TargetSpec = Box::leak(Box::new(TargetSpec::new(None, None, None, None)));
    let mut semantics = Semantics::new(hir, target);
    semantics.analyze(diag.clone(), &importer);
    (semantics, diag)
}

pub fn write_stub_for_module(module_name: &str, src: &str, path: &std::path::Path) {
    let diagnostics = Rc::new(RefCell::new(Diagnostics::new(
        "mod.unn".to_string(),
        src.to_string(),
    )));
    let mut lexer = Lexer::new(src, diagnostics.clone());
    let tokens = lexer.tokenize();
    assert!(!lexer.corrupted, "lexer corrupted for:\n{src}");
    let mut parser = Parser::new(tokens, diagnostics.clone());
    let ast = parser.parse();
    assert!(!parser.corrupted, "parser corrupted for:\n{src}");
    let mut lowering = Lowering::new(ast, diagnostics.clone());
    let mut hir = lowering.lower();
    assert!(!lowering.corrupted, "lowering corrupted for:\n{src}");
    let no_stubs: Vec<String> = vec![];
    let mut importer = ImportEngine::new(diagnostics.clone());
    importer.import(&mut hir, &no_stubs);
    assert!(
        !importer.corrupted,
        "module failed import: {:?}",
        diagnostics.borrow().errors
    );
    let target: &'static TargetSpec = Box::leak(Box::new(TargetSpec::new(None, None, None, None)));
    let mut semantics = Semantics::new(hir, target);
    semantics.analyze(diagnostics.clone(), &importer);
    assert!(
        !semantics.corrupted,
        "analyze failed: {:?}",
        diagnostics.borrow().errors
    );
    let monomorph_hir = semantics.generate_monormophizer_hir();
    let hir_index = NodeIndex::build(&monomorph_hir);
    let serializer = Serializer::new(
        module_name.to_string(),
        &semantics.ctxt,
        &hir_index,
        diagnostics.clone(),
    );
    let stub = serializer.serialize();
    let bytes = bincode::serialize(&stub).expect("stub serialization failed");
    std::fs::write(path, bytes).expect("stub write failed");
}
