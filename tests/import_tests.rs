use std::{cell::RefCell, rc::Rc};

use unnc::hir::{HirStmt, HirStmtKind};
use unnc::import::ImportEngine;
use unnc::lowering::NodeId;

mod common;

use common::{messages, pipe_imported};

struct Imported {
    importer: ImportEngine,
    hir: Vec<common::HirStmt>,
    diagnostics: Rc<RefCell<common::Diagnostics>>,
    src: String,
}

fn run_import(src: &str, stub_paths: Vec<&str>) -> Imported {
    let (hir, importer, diagnostics) = pipe_imported(src, &stub_paths);
    Imported {
        importer,
        hir,
        diagnostics,
        src: src.to_string(),
    }
}

// plain (non-mangled) names
#[test]
fn plain_import_keeps_name_and_swaps_to_external_id() {
    let r = run_import("import foo", vec![]);
    assert_eq!(
        r.importer.resolved_imports,
        [("foo".to_string(), None)].into_iter().collect()
    );
    assert!(r.importer.symbol_aliases.is_empty());
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "foo".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
    assert_eq!(
        r.hir[0].hir_id,
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert!(
        matches!(&r.hir[0].kind, HirStmtKind::HirImport { name, alias: None } if name == "foo")
    );
    assert_eq!(
        messages(&r.diagnostics),
        vec!["Module 'foo' was imported but not provided"]
    );
}

#[test]
fn plain_import_with_alias_records_alias_and_maps_declaration() {
    let r = run_import("import foo as f", vec![]);
    assert_eq!(
        r.importer.resolved_imports,
        [("foo".to_string(), Some("f".to_string()))]
            .into_iter()
            .collect()
    );
    assert!(r.importer.symbol_aliases.is_empty());
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "f".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
}

#[test]
fn multiple_plain_imports_get_distinct_external_ids() {
    let r = run_import("import foo\nimport bar", vec![]);
    assert_eq!(
        r.importer.symbol_declarations["foo"],
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert_eq!(
        r.importer.symbol_declarations["bar"],
        NodeId {
            local: 0,
            external: 1
        }
    );
    assert_eq!(
        r.hir[0].hir_id,
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert!(
        matches!(&r.hir[0].kind, HirStmtKind::HirImport { name, alias: None } if name == "foo")
    );
    assert_eq!(
        r.hir[1].hir_id,
        NodeId {
            local: 0,
            external: 1
        }
    );
    assert!(
        matches!(&r.hir[1].kind, HirStmtKind::HirImport { name, alias: None } if name == "bar")
    );
    let mut errs = messages(&r.diagnostics);
    let mut expected = vec![
        "Module 'foo' was imported but not provided",
        "Module 'bar' was imported but not provided",
    ];
    errs.sort();
    expected.sort();
    assert_eq!(errs, expected);
}

#[test]
fn duplicate_plain_import_is_rejected_no_id_swap() {
    let r = run_import("import foo\nimport foo", vec![]);
    assert_eq!(
        r.importer.symbol_declarations["foo"],
        NodeId {
            local: 0,
            external: 0
        },
        "first import still wins"
    );
    assert_eq!(
        r.hir[1].hir_id,
        NodeId {
            local: 1,
            external: 0
        },
        "second import not swapped"
    );
    assert_eq!(messages(&r.diagnostics), vec!["Already imported 'foo'"]);
}

#[test]
fn duplicate_plain_alias_is_rejected() {
    let r = run_import("import foo as f\nimport bar as f", vec![]);
    assert_eq!(
        r.importer.symbol_declarations["f"],
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert_eq!(
        r.hir[1].hir_id,
        NodeId {
            local: 1,
            external: 0
        }
    );
    assert_eq!(messages(&r.diagnostics), vec!["Already used alias 'f' "]);
}

// ---------------------------------------------------------------------------
// mangled ::-resolved names
// ---------------------------------------------------------------------------

#[test]
fn mangled_import_demangles_to_module_and_symbol() {
    let r = run_import("import A::B", vec![]);
    assert_eq!(
        r.importer.resolved_imports,
        [("A".to_string(), None)].into_iter().collect()
    );
    assert_eq!(
        r.importer.symbol_aliases,
        [("B".to_string(), "A_B".to_string())].into_iter().collect()
    );
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "B".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
    assert_eq!(
        r.hir,
        vec![HirStmt {
            hir_id: NodeId {
                local: 0,
                external: 0
            },
            kind: HirStmtKind::HirImport {
                name: "A_B".to_string(),
                alias: None
            },
            span: r.hir[0].span.clone()
        }]
    );
    assert_eq!(
        messages(&r.diagnostics),
        vec!["Module 'A' was imported but not provided"]
    );
}

#[test]
fn mangled_import_with_alias_keyed_on_alias() {
    let r = run_import("import A::B as x", vec![]);
    assert_eq!(
        r.importer.resolved_imports,
        [("A".to_string(), None)].into_iter().collect()
    );
    assert_eq!(
        r.importer.symbol_aliases,
        [("x".to_string(), "A_B".to_string())].into_iter().collect()
    );
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "x".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
}

#[test]
fn multi_underscore_symbol_splits_on_first_underscore() {
    let r = run_import("import a_b::c_d", vec![]);
    assert_eq!(
        r.importer.resolved_imports,
        [("a".to_string(), None)].into_iter().collect()
    );
    assert_eq!(
        r.importer.symbol_aliases,
        [("b_c_d".to_string(), "a_b_c_d".to_string())]
            .into_iter()
            .collect()
    );
    assert_eq!(
        messages(&r.diagnostics),
        vec!["Module 'a' was imported but not provided"]
    );
}

#[test]
fn leading_underscore_name_is_plain_module() {
    let r = run_import("import _foo", vec![]);
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "_foo".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
    assert_eq!(
        messages(&r.diagnostics),
        vec!["Module '_foo' was imported but not provided"]
    );
}

#[test]
fn trailing_underscore_name_is_plain_module() {
    let r = run_import("import foo_", vec![]);
    assert_eq!(
        r.importer.symbol_declarations,
        [(
            "foo_".to_string(),
            NodeId {
                local: 0,
                external: 0
            }
        )]
        .into_iter()
        .collect()
    );
    assert_eq!(
        messages(&r.diagnostics),
        vec!["Module 'foo_' was imported but not provided"]
    );
}

// ---------------------------------------------------------------------------
// quirk-documenting tests (current behavior locked in)
// ---------------------------------------------------------------------------

#[test]
fn duplicate_mangled_import_is_rejected_first_wins() {
    let r = run_import("import a_b\nimport a_b", vec![]);
    // first import wins; second is rejected
    assert_eq!(r.importer.symbol_aliases["b"], "a_b");
    assert_eq!(
        r.importer.symbol_declarations["b"],
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert_eq!(messages(&r.diagnostics), vec!["Already imported 'b'"],);
}

#[test]
fn mangled_alias_conflict_is_rejected_first_wins() {
    let r = run_import("import a_b as x\nimport c_d as x", vec![]);
    // first alias wins; second is rejected
    assert_eq!(r.importer.symbol_aliases["x"], "a_b");
    assert_eq!(
        r.importer.symbol_declarations["x"],
        NodeId {
            local: 0,
            external: 0
        }
    );
    assert_eq!(messages(&r.diagnostics), vec!["Already used alias 'x' "],);
}

// stub loading error paths (no positive stub round-trip without bincode in dev-deps)
#[test]
fn missing_stub_file_reports_read_error() {
    let r = run_import("import foo", vec!["/nonexistent/__stub_not_here__.bin"]);
    let errs = messages(&r.diagnostics);
    assert_eq!(errs.len(), 1, "got: {errs:?}");
    assert!(
        errs[0].starts_with("Failed to read stub file"),
        "got: {}",
        errs[0]
    );
}

#[test]
fn resolve_imported_name_returns_none_without_stub() {
    let r = run_import("import foo", vec![]);
    assert_eq!(r.importer.resolve_imported_name("foo"), None);
}

#[test]
fn resolve_external_ty_returns_none_without_stub() {
    let r = run_import("import A::B", vec![]);
    let decl_id = r.importer.symbol_declarations["B"];
    assert_eq!(r.importer.resolve_external_ty(&decl_id), None);
}

// ---------------------------------------------------------------------------
// positive stub round-trip
// ---------------------------------------------------------------------------

#[test]
fn exposed_module_roundtrip_resolves_imported_symbol() {
    let stub_path = std::env::temp_dir().join(format!("unnc_test_foo_{}.bin", std::process::id()));
    common::write_stub_for_module("foo", "expose struct Point { x: i32, y: i32 }", &stub_path);

    let r = run_import("import foo::Point", vec![stub_path.to_str().unwrap()]);
    assert!(
        !r.importer.corrupted,
        "stubless error should not fire: {:?}",
        messages(&r.diagnostics)
    );
    assert!(
        messages(&r.diagnostics).is_empty(),
        "got: {:?}",
        messages(&r.diagnostics)
    );
    assert_eq!(r.importer.symbol_aliases["Point"], "foo_Point".to_string());

    let decl_id = r.importer.symbol_declarations["Point"];
    assert_eq!(r.importer.resolve_imported_name("Point"), Some(decl_id));

    let ty = r.importer.resolve_external_ty(&decl_id);
    assert!(ty.is_some(), "struct Point should resolve to its TypeInfo");
    std::fs::remove_file(&stub_path).ok();
}

#[test]
fn exposed_module_roundtrip_keeps_module_and_symbol_entry() {
    let stub_path = std::env::temp_dir().join(format!("unnc_test_bar_{}.bin", std::process::id()));
    common::write_stub_for_module(
        "math",
        "expose func add(a: i32, b: i32): i32 {\n  return a + b;\n}",
        &stub_path,
    );

    let r = run_import(
        "import math::add as plus",
        vec![stub_path.to_str().unwrap()],
    );
    assert!(!r.importer.corrupted, "got: {:?}", messages(&r.diagnostics));
    assert!(messages(&r.diagnostics).is_empty());
    assert_eq!(r.importer.resolved_imports["math"], None);
    assert_eq!(r.importer.symbol_aliases["plus"], "math_add".to_string());

    let decl_id = r.importer.symbol_declarations["plus"];
    assert_eq!(r.importer.resolve_imported_name("plus"), Some(decl_id));
    assert!(r.importer.resolve_external_ty(&decl_id).is_some());
    std::fs::remove_file(&stub_path).ok();
}
