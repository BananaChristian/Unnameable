use core::fmt;

use crate::hir::{
    HirEnumMember, HirParam, HirStmt, HirStmtKind, HirVariantMember,
    expressions::{HirExpr, HirExprKind, HirInstParam},
    types::{HirAnonStructField, HirType, HirTypeNode},
};

/// Pretty printer for HIR (High-level Intermediate Representation) nodes.
#[derive(Default)]
pub struct HirPrinter {
    output: String,
    indent_level: usize,
}

impl HirPrinter {
    pub fn new() -> Self {
        Self::default()
    }

    /// Entry point to print a full slice of HIR statements (a lowered AST module).
    pub fn print_hir(hir: &[HirStmt]) -> String {
        let mut printer = Self::new();
        for stmt in hir {
            printer.fmt_stmt(stmt);
        }
        printer.output
    }

    pub fn print_stmt(stmt: &HirStmt) -> String {
        let mut printer = Self::new();
        printer.fmt_stmt(stmt);
        printer.output
    }

    pub fn print_expr(expr: &HirExpr) -> String {
        let mut printer = Self::new();
        printer.fmt_expr(expr);
        printer.output
    }

    pub fn print_type(ty: &HirTypeNode) -> String {
        let mut printer = Self::new();
        printer.fmt_type(ty);
        printer.output
    }

    fn indent(&mut self) {
        for _ in 0..self.indent_level {
            self.output.push_str("  ");
        }
    }

    fn write_line(&mut self, line: &str) {
        self.indent();
        self.output.push_str(line);
        self.output.push('\n');
    }

    fn with_indent<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.indent_level += 1;
        f(self);
        self.indent_level -= 1;
    }

    pub fn fmt_stmt(&mut self, stmt: &HirStmt) {
        let id = stmt.hir_id;
        match &stmt.kind {
            HirStmtKind::HirExpr(expr) => {
                self.write_line(&format!("HirExprStmt [id: {id:?}]"));
                self.with_indent(|p| p.fmt_expr(expr));
            }
            HirStmtKind::HirReturn(val) => {
                self.write_line(&format!("HirReturn [id: {id:?}]"));
                if let Some(e) = val {
                    self.with_indent(|p| p.fmt_expr(e));
                }
            }
            HirStmtKind::HirBreak => self.write_line(&format!("HirBreak [id: {id:?}]")),
            HirStmtKind::HirContinue => self.write_line(&format!("HirContinue [id: {id:?}]")),
            HirStmtKind::HirVarDecl {
                name,
                mutable,
                constant,
                dollar_read,
                exposed,
                ty,
                init,
            } => {
                let flags = format_flags(*mutable, *constant, *dollar_read, *exposed);
                self.write_line(&format!("HirVarDecl \"{name}\" {flags} [id: {id:?}]"));
                self.with_indent(|p| {
                    if let Some(t) = ty {
                        p.write_line("Type:");
                        p.with_indent(|p2| p2.fmt_type(t));
                    }
                    p.write_line("Init:");
                    p.with_indent(|p2| p2.fmt_expr(init));
                });
            }
            HirStmtKind::HirFunctionDef {
                name,
                params,
                return_type,
                generic_type_params,
                exposed,
                dollar_read,
                body,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                let dollar = if *dollar_read { "$" } else { "" };
                self.write_line(&format!(
                    "HirFunctionDef \"{dollar}{name}\"{exp} [id: {id:?}]"
                ));
                self.with_indent(|p| {
                    if !generic_type_params.is_empty() {
                        p.write_line("GenericTypeParams:");
                        p.with_indent(|p2| {
                            for g in generic_type_params {
                                p2.fmt_type(g);
                            }
                        });
                    }

                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_param(param);
                            }
                        });
                    }

                    p.write_line("ReturnType:");
                    p.with_indent(|p2| p2.fmt_type(return_type));

                    p.write_line("Body:");
                    p.with_indent(|p2| {
                        for body_stmt in body {
                            p2.fmt_stmt(body_stmt);
                        }
                    });
                });
            }
            HirStmtKind::HirFunctionDecl {
                name,
                params,
                return_type,
                generic_type_params,
                exposed,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                self.write_line(&format!("HirFunctionDecl \"{name}\"{exp} [id: {id:?}]"));
                self.with_indent(|p| {
                    if !generic_type_params.is_empty() {
                        p.write_line("GenericTypeParams:");
                        p.with_indent(|p2| {
                            for g in generic_type_params {
                                p2.fmt_type(g);
                            }
                        });
                    }

                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_param(param);
                            }
                        });
                    }

                    p.write_line("ReturnType:");
                    p.with_indent(|p2| p2.fmt_type(return_type));
                });
            }
            HirStmtKind::HirStructDecl {
                name,
                contracts,
                generic_type_params,
                fields,
                exposed,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                self.write_line(&format!("HirStructDecl \"{name}\"{exp} [id: {id:?}]"));
                self.with_indent(|p| {
                    if !generic_type_params.is_empty() {
                        p.write_line("GenericTypeParams:");
                        p.with_indent(|p2| {
                            for g in generic_type_params {
                                p2.fmt_type(g);
                            }
                        });
                    }

                    if !contracts.is_empty() {
                        p.write_line("Contracts:");
                        p.with_indent(|p2| {
                            for c in contracts {
                                p2.fmt_type(c);
                            }
                        });
                    }

                    p.write_line("Fields:");
                    p.with_indent(|p2| {
                        for field in fields {
                            p2.fmt_param(field);
                        }
                    });
                });
            }
            HirStmtKind::HirIf {
                condition,
                body,
                else_body,
            } => {
                self.write_line(&format!("HirIf [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Condition:");
                    p.with_indent(|p2| p2.fmt_expr(condition));

                    p.write_line("Then:");
                    p.with_indent(|p2| {
                        for s in body {
                            p2.fmt_stmt(s);
                        }
                    });

                    if let Some(eb) = else_body {
                        p.write_line("Else:");
                        p.with_indent(|p2| {
                            for s in eb {
                                p2.fmt_stmt(s);
                            }
                        });
                    }
                });
            }
            HirStmtKind::HirContractDecl {
                name,
                functions,
                generic_type_params,
                exposed,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                self.write_line(&format!("HirContractDecl \"{name}\"{exp} [id: {id:?}]"));
                self.with_indent(|p| {
                    if !generic_type_params.is_empty() {
                        p.write_line("GenericTypeParams:");
                        p.with_indent(|p2| {
                            for g in generic_type_params {
                                p2.fmt_type(g);
                            }
                        });
                    }

                    p.write_line("Functions:");
                    p.with_indent(|p2| {
                        for f in functions {
                            p2.fmt_stmt(f);
                        }
                    });
                });
            }
            HirStmtKind::HirAlias { original, alias } => {
                self.write_line(&format!("HirAlias \"{alias}\" [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("OriginalType:");
                    p.with_indent(|p2| p2.fmt_type(original));
                });
            }
            HirStmtKind::HirImport { name, alias } => {
                let alias_str = alias
                    .as_ref()
                    .map(|a| format!(" as \"{a}\""))
                    .unwrap_or_default();
                self.write_line(&format!("HirImport \"{name}\"{alias_str} [id: {id:?}]"));
            }
            HirStmtKind::HirWhile { condition, body } => {
                self.write_line(&format!("HirWhile [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Condition:");
                    p.with_indent(|p2| p2.fmt_expr(condition));

                    p.write_line("Body:");
                    p.with_indent(|p2| {
                        for s in body {
                            p2.fmt_stmt(s);
                        }
                    });
                });
            }
            HirStmtKind::HirEnumDecl {
                name,
                underlying,
                members,
                exposed,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                self.write_line(&format!("HirEnumDecl \"{name}\"{exp} [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Underlying:");
                    p.with_indent(|p2| p2.fmt_type(underlying));

                    p.write_line("Members:");
                    p.with_indent(|p2| {
                        for m in members {
                            p2.fmt_enum_member(m);
                        }
                    });
                });
            }
            HirStmtKind::HirVariantDecl {
                name,
                contracts,
                members,
                generic_type_params,
                exposed,
            } => {
                let exp = if *exposed { " (exposed)" } else { "" };
                self.write_line(&format!("HirVariantDecl \"{name}\"{exp} [id: {id:?}]"));
                self.with_indent(|p| {
                    if !generic_type_params.is_empty() {
                        p.write_line("GenericTypeParams:");
                        p.with_indent(|p2| {
                            for g in generic_type_params {
                                p2.fmt_type(g);
                            }
                        });
                    }

                    if !contracts.is_empty() {
                        p.write_line("Contracts:");
                        p.with_indent(|p2| {
                            for c in contracts {
                                p2.fmt_type(c);
                            }
                        });
                    }

                    p.write_line("Members:");
                    p.with_indent(|p2| {
                        for m in members {
                            p2.fmt_variant_member(m);
                        }
                    });
                });
            }
        }
    }

    fn fmt_param(&mut self, param: &HirParam) {
        let dollar_str = if param.dollar_read { "$" } else { "" };
        let mut_str = if param.mutable { "mut " } else { "" };
        self.write_line(&format!(
            "Param \"{dollar_str}{mut_str}{}\" [id: {:?}]",
            param.name, param.hir_id
        ));
        self.with_indent(|p| {
            p.write_line("Type:");
            p.with_indent(|p2| p2.fmt_type(&param.ty));

            if let Some(def) = &param.default {
                p.write_line("Default:");
                p.with_indent(|p2| p2.fmt_expr(def));
            }
        });
    }

    fn fmt_enum_member(&mut self, member: &HirEnumMember) {
        self.write_line(&format!(
            "EnumMember \"{}\" = {} [id: {:?}]",
            member.name, member.value, member.hir_id
        ));
    }

    fn fmt_variant_member(&mut self, member: &HirVariantMember) {
        self.write_line(&format!(
            "VariantMember \"{}\" (tag: {}) [id: {:?}]",
            member.name, member.tag, member.hir_id
        ));
        if !member.member_types.is_empty() {
            self.with_indent(|p| {
                p.write_line("TupleTypes:");
                p.with_indent(|p2| {
                    for ty in &member.member_types {
                        p2.fmt_type(ty);
                    }
                });
            });
        }
    }

    pub fn fmt_expr(&mut self, expr: &HirExpr) {
        let id = expr.hir_id;
        match &expr.kind {
            HirExprKind::Literal(lit) => self.write_line(&format!("Literal({lit:?}) [id: {id:?}]")),
            HirExprKind::Identifier(name) => {
                self.write_line(&format!("Identifier(\"{name}\") [id: {id:?}]"))
            }
            HirExprKind::Binary(left, op, right) => {
                self.write_line(&format!("BinaryOp({op:?}) [id: {id:?}]"));
                self.with_indent(|p| {
                    p.fmt_expr(left);
                    p.fmt_expr(right);
                });
            }
            HirExprKind::Unary(op, operand) => {
                self.write_line(&format!("UnaryOp({op:?}) [id: {id:?}]"));
                self.with_indent(|p| p.fmt_expr(operand));
            }
            HirExprKind::GenericInstantion { name, type_params } => {
                self.write_line(&format!("GenericInstantiation \"{name}\" [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("TypeParams:");
                    p.with_indent(|p2| {
                        for tp in type_params {
                            p2.fmt_type(tp);
                        }
                    });
                });
            }
            HirExprKind::Call(callee, args) => {
                self.write_line(&format!("Call [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Callee:");
                    p.with_indent(|p2| p2.fmt_expr(callee));

                    if !args.is_empty() {
                        p.write_line("Args:");
                        p.with_indent(|p2| {
                            for arg in args {
                                p2.fmt_expr(arg);
                            }
                        });
                    }
                });
            }
            HirExprKind::Unwrap(operand) => {
                self.write_line(&format!("Unwrap [id: {id:?}]"));
                self.with_indent(|p| p.fmt_expr(operand));
            }
            HirExprKind::Postfix(operand, op) => {
                self.write_line(&format!("PostfixOp({op:?}) [id: {id:?}]"));
                self.with_indent(|p| p.fmt_expr(operand));
            }
            HirExprKind::SizeOf(ty) => {
                self.write_line(&format!("SizeOf [id: {id:?}]"));
                self.with_indent(|p| p.fmt_type(ty));
            }
            HirExprKind::StaticCast(ty, operand) => {
                self.write_line(&format!("StaticCast [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("ToType:");
                    p.with_indent(|p2| p2.fmt_type(ty));
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(operand));
                });
            }
            HirExprKind::BitCast(ty, operand) => {
                self.write_line(&format!("BitCast [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("ToType:");
                    p.with_indent(|p2| p2.fmt_type(ty));
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(operand));
                });
            }
            HirExprKind::Instantiation { init_ty, body } => {
                self.write_line(&format!("Instantiation [id: {id:?}]"));
                self.with_indent(|p| {
                    if let Some(ty) = init_ty {
                        p.write_line("Type:");
                        p.with_indent(|p2| p2.fmt_type(ty));
                    }
                    p.write_line("Fields:");
                    p.with_indent(|p2| {
                        for param in body {
                            p2.fmt_inst_param(param);
                        }
                    });
                });
            }
            HirExprKind::DollarScope {
                params,
                body,
                result,
            } => {
                self.write_line(&format!("DollarScope [id: {id:?}]"));
                self.with_indent(|p| {
                    if !params.is_empty() {
                        p.write_line("Captures:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_expr(param);
                            }
                        });
                    }

                    p.write_line("Statements:");
                    p.with_indent(|p2| {
                        for st in body {
                            p2.fmt_stmt(st);
                        }
                    });

                    if let Some(res) = result {
                        p.write_line("Result:");
                        p.with_indent(|p2| p2.fmt_expr(res));
                    }
                });
            }
            HirExprKind::Index { target, index } => {
                self.write_line(&format!("IndexAccess [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(target));
                    p.write_line("Index:");
                    p.with_indent(|p2| p2.fmt_expr(index));
                });
            }
        }
    }

    fn fmt_inst_param(&mut self, param: &HirInstParam) {
        self.write_line(&format!(
            "Field \"{}\" [id: {:?}]",
            param.name, param.hir_id
        ));
        self.with_indent(|p| {
            p.write_line("Value:");
            p.with_indent(|p2| p2.fmt_expr(&param.value));
        });
    }

    pub fn fmt_type(&mut self, ty: &HirTypeNode) {
        let id = ty.hir_id;
        match &ty.kind {
            HirType::I8 => self.write_line(&format!("Type(i8) [id: {id:?}]")),
            HirType::I16 => self.write_line(&format!("Type(i16) [id: {id:?}]")),
            HirType::I32 => self.write_line(&format!("Type(i32) [id: {id:?}]")),
            HirType::I64 => self.write_line(&format!("Type(i64) [id: {id:?}]")),
            HirType::I128 => self.write_line(&format!("Type(i128) [id: {id:?}]")),
            HirType::U8 => self.write_line(&format!("Type(u8) [id: {id:?}]")),
            HirType::U16 => self.write_line(&format!("Type(u16) [id: {id:?}]")),
            HirType::U32 => self.write_line(&format!("Type(u32) [id: {id:?}]")),
            HirType::U64 => self.write_line(&format!("Type(u64) [id: {id:?}]")),
            HirType::U128 => self.write_line(&format!("Type(u128) [id: {id:?}]")),
            HirType::ISize => self.write_line(&format!("Type(isize) [id: {id:?}]")),
            HirType::USize => self.write_line(&format!("Type(usize) [id: {id:?}]")),
            HirType::F32 => self.write_line(&format!("Type(f32) [id: {id:?}]")),
            HirType::F64 => self.write_line(&format!("Type(f64) [id: {id:?}]")),
            HirType::Str => self.write_line(&format!("Type(str) [id: {id:?}]")),
            HirType::Char8 => self.write_line(&format!("Type(char8) [id: {id:?}]")),
            HirType::Char16 => self.write_line(&format!("Type(char16) [id: {id:?}]")),
            HirType::Char32 => self.write_line(&format!("Type(char32) [id: {id:?}]")),

            HirType::Bool => self.write_line(&format!("Type(bool) [id: {id:?}]")),
            HirType::Unit => self.write_line(&format!("Type(()) [id: {id:?}]")),

            HirType::Ptr(inner) => {
                self.write_line(&format!("PtrTo [id: {id:?}]"));
                self.with_indent(|p| p.fmt_type(inner));
            }
            HirType::Ref(inner) => {
                self.write_line(&format!("RefTo [id: {id:?}]"));
                self.with_indent(|p| p.fmt_type(inner));
            }
            HirType::Array(inner, sz) => {
                let size_str = sz.map_or(String::new(), |s| format!(" len: {s}"));
                self.write_line(&format!("ArrayType{size_str} [id: {id:?}]"));
                self.with_indent(|p| p.fmt_type(inner));
            }
            HirType::Func(params, ret) => {
                self.write_line(&format!("FuncPtrType [id: {id:?}]"));
                self.with_indent(|p| {
                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_type(param);
                            }
                        });
                    }
                    p.write_line("ReturnType:");
                    p.with_indent(|p2| p2.fmt_type(ret));
                });
            }
            HirType::CustomType(name) => {
                self.write_line(&format!("CustomType(\"{name}\") [id: {id:?}]"))
            }
            HirType::GenericPlaceHolder(name) => {
                self.write_line(&format!("GenericPlaceHolder(\"{name}\") [id: {id:?}]"))
            }
            HirType::GenericType { name, type_params } => {
                self.write_line(&format!("GenericType \"{name}\" [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("TypeParams:");
                    p.with_indent(|p2| {
                        for tp in type_params {
                            p2.fmt_type(tp);
                        }
                    });
                });
            }
            HirType::Nullable(inner) => {
                self.write_line(&format!("NullableType [id: {id:?}]"));
                self.with_indent(|p| p.fmt_type(inner));
            }
            HirType::Failable(ok, err) => {
                self.write_line(&format!("FailableType [id: {id:?}]"));
                self.with_indent(|p| {
                    p.write_line("Ok:");
                    p.with_indent(|p2| p2.fmt_type(ok));
                    p.write_line("Err:");
                    p.with_indent(|p2| p2.fmt_type(err));
                });
            }
            HirType::Tuple(types) => {
                self.write_line(&format!("TupleType [id: {id:?}]"));
                self.with_indent(|p| {
                    for ty in types {
                        p.fmt_type(ty);
                    }
                });
            }
            HirType::AnonymousStruct(fields) => {
                self.write_line(&format!("AnonymousStructType [id: {id:?}]"));
                self.with_indent(|p| {
                    for field in fields {
                        p.fmt_anon_field(field);
                    }
                });
            }
        }
    }

    fn fmt_anon_field(&mut self, field: &HirAnonStructField) {
        self.write_line(&format!("Field \"{}\":", field.name));
        self.with_indent(|p| p.fmt_type(&field.ty));
    }
}

fn format_flags(mutable: bool, constant: bool, heap: bool, exposed: bool) -> String {
    let mut flags = Vec::new();
    if mutable {
        flags.push("mut");
    }
    if constant {
        flags.push("const");
    }
    if heap {
        flags.push("heap");
    }
    if exposed {
        flags.push("expose");
    }

    if flags.is_empty() {
        String::new()
    } else {
        format!("[{}]", flags.join(", "))
    }
}

impl fmt::Display for HirStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", HirPrinter::print_stmt(self))
    }
}

impl fmt::Display for HirExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", HirPrinter::print_expr(self))
    }
}

impl fmt::Display for HirTypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", HirPrinter::print_type(self))
    }
}
