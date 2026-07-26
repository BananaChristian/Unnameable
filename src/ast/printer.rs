use core::fmt;

use crate::ast::{
    AnonStructField, Elif, EnumMember, Expr, ExprKind, InstParam, Literal, Qualifier,
    QualifierKind, Stmt, StmtKind, Type, TypeKind, VariantMember,
};

/// Pretty printer for AST nodes that renders human-readable tree structures.
pub struct AstPrinter {
    output: String,
    indent_level: usize,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
        }
    }

    /// Helper to print any AST node directly to a String.
    pub fn print_stmt(stmt: &Stmt) -> String {
        let mut printer = Self::new();
        printer.fmt_stmt(stmt);
        printer.output
    }

    pub fn print_expr(expr: &Expr) -> String {
        let mut printer = Self::new();
        printer.fmt_expr(expr);
        printer.output
    }

    pub fn print_type(ty: &Type) -> String {
        let mut printer = Self::new();
        printer.fmt_type(ty);
        printer.output
    }

    // --- Formatting Core ---

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

    // --- Statement Formatting ---

    pub fn fmt_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.write_line("ExprStmt");
                self.with_indent(|p| p.fmt_expr(expr));
            }
            StmtKind::Return(val) => {
                self.write_line("Return");
                if let Some(e) = val {
                    self.with_indent(|p| p.fmt_expr(e));
                }
            }
            StmtKind::Break => self.write_line("Break"),
            StmtKind::Continue => self.write_line("Continue"),
            StmtKind::VarDecl {
                qualifiers,
                type_annotation,
                name,
                init,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("VarDecl {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if let Some(ty) = type_annotation {
                        p.write_line("Type:");
                        p.with_indent(|p2| p2.fmt_type(ty));
                    }

                    p.write_line("Init:");
                    p.with_indent(|p2| p2.fmt_expr(init));
                });
            }
            StmtKind::ParamDecl {
                qualifiers,
                name,
                type_annotation,
                def,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("ParamDecl {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("Type:");
                    p.with_indent(|p2| p2.fmt_type(type_annotation));

                    if let Some(default_val) = def {
                        p.write_line("Default:");
                        p.with_indent(|p2| p2.fmt_expr(default_val));
                    }
                });
            }
            StmtKind::Block { content } => {
                self.write_line("Block");
                self.with_indent(|p| {
                    for stmt in content {
                        p.fmt_stmt(stmt);
                    }
                });
            }
            StmtKind::FunctionDef {
                qualifiers,
                name,
                params,
                type_annotation,
                body,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("FunctionDef {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_stmt(param);
                            }
                        });
                    }

                    if let Some(ret_ty) = type_annotation {
                        p.write_line("ReturnType:");
                        p.with_indent(|p2| p2.fmt_type(ret_ty));
                    }

                    p.write_line("Body:");
                    p.with_indent(|p2| p2.fmt_stmt(body));
                });
            }
            StmtKind::FunctionDecl {
                qualifiers,
                name,
                params,
                type_annotation,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("FunctionDecl {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_stmt(param);
                            }
                        });
                    }

                    if let Some(ret_ty) = type_annotation {
                        p.write_line("ReturnType:");
                        p.with_indent(|p2| p2.fmt_type(ret_ty));
                    }
                });
            }
            StmtKind::StructDecl {
                qualifiers,
                name,
                contracts,
                contents,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("StructDecl {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if !contracts.is_empty() {
                        p.write_line("Contracts:");
                        p.with_indent(|p2| {
                            for c in contracts {
                                p2.fmt_type(c);
                            }
                        });
                    }

                    p.write_line("Fields:");
                    p.with_indent(|p2| p2.fmt_stmt(contents));
                });
            }
            StmtKind::SealStmt { qualifiers, name, contents } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("SealStmt {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("Contents:");
                    p.with_indent(|p2| {
                        for stmt in contents {
                            p2.fmt_stmt(stmt);
                        }
                    });
                });
            }
            StmtKind::MethodsStmt { name, contents } => {
                self.write_line("MethodsStmt");
                self.with_indent(|p| {
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("Methods:");
                    p.with_indent(|p2| {
                        for stmt in contents {
                            p2.fmt_stmt(stmt);
                        }
                    });
                });
            }
            StmtKind::AliasStmt { original, new } => {
                self.write_line("AliasStmt");
                self.with_indent(|p| {
                    p.write_line("AliasName:");
                    p.with_indent(|p2| p2.fmt_expr(new));

                    p.write_line("OriginalType:");
                    p.with_indent(|p2| p2.fmt_type(original));
                });
            }
            StmtKind::ImportStmt { name, alias } => {
                self.write_line("ImportStmt");
                self.with_indent(|p| {
                    p.write_line("Path:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if let Some(al) = alias {
                        p.write_line("Alias:");
                        p.with_indent(|p2| p2.fmt_expr(al));
                    }
                });
            }
            StmtKind::IfStmt {
                condition,
                body,
                elifs,
                else_body,
            } => {
                self.write_line("IfStmt");
                self.with_indent(|p| {
                    p.write_line("Condition:");
                    p.with_indent(|p2| p2.fmt_expr(condition));

                    p.write_line("Then:");
                    p.with_indent(|p2| p2.fmt_stmt(body));

                    for elif in elifs {
                        p.fmt_elif(elif);
                    }

                    if let Some(el) = else_body {
                        p.write_line("Else:");
                        p.with_indent(|p2| p2.fmt_stmt(el));
                    }
                });
            }
            StmtKind::GenericBlock { params, body } => {
                self.write_line("GenericBlock");
                self.with_indent(|p| {
                    p.write_line("TypeParams:");
                    p.with_indent(|p2| {
                        for param in params {
                            p2.fmt_type(param);
                        }
                    });

                    p.write_line("Body:");
                    p.with_indent(|p2| p2.fmt_stmt(body));
                });
            }
            StmtKind::ContractBlock { qualifiers, name, body } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("ContractBlock {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("Body:");
                    p.with_indent(|p2| {
                        for stmt in body {
                            p2.fmt_stmt(stmt);
                        }
                    });
                });
            }
            StmtKind::WhileStmt { condition, body } => {
                self.write_line("WhileStmt");
                self.with_indent(|p| {
                    p.write_line("Condition:");
                    p.with_indent(|p2| p2.fmt_expr(condition));

                    p.write_line("Body:");
                    p.with_indent(|p2| p2.fmt_stmt(body));
                });
            }
            StmtKind::ForStmt {
                init,
                condition,
                update,
                body,
            } => {
                self.write_line("ForStmt");
                self.with_indent(|p| {
                    p.write_line("Init:");
                    p.with_indent(|p2| p2.fmt_stmt(init));

                    p.write_line("Condition:");
                    p.with_indent(|p2| p2.fmt_expr(condition));

                    p.write_line("Update:");
                    p.with_indent(|p2| p2.fmt_expr(update));

                    p.write_line("Body:");
                    p.with_indent(|p2| p2.fmt_stmt(body));
                });
            }
            StmtKind::EachStmt {
                item,
                collection,
                body,
            } => {
                self.write_line("EachStmt");
                self.with_indent(|p| {
                    p.write_line("Item:");
                    p.with_indent(|p2| p2.fmt_expr(item));

                    p.write_line("Collection:");
                    p.with_indent(|p2| p2.fmt_expr(collection));

                    p.write_line("Body:");
                    p.with_indent(|p2| p2.fmt_stmt(body));
                });
            }
            StmtKind::EnumStmt {
                qualifiers,
                name,
                underlying,
                content,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("EnumStmt {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if let Some(underlying_ty) = underlying {
                        p.write_line("UnderlyingType:");
                        p.with_indent(|p2| p2.fmt_type(underlying_ty));
                    }

                    p.write_line("Members:");
                    p.with_indent(|p2| {
                        for member in content {
                            p2.fmt_enum_member(member);
                        }
                    });
                });
            }
            StmtKind::VariantStmt {
                qualifiers,
                name,
                contracts,
                body,
            } => {
                let quals = format_qualifiers(qualifiers);
                self.write_line(&format!("VariantStmt {quals}"));
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    if !contracts.is_empty() {
                        p.write_line("Contracts:");
                        p.with_indent(|p2| {
                            for c in contracts {
                                p2.fmt_type(c);
                            }
                        });
                    }

                    p.write_line("Variants:");
                    p.with_indent(|p2| {
                        for variant in body {
                            p2.fmt_variant_member(variant);
                        }
                    });
                });
            }
        }
    }

    fn fmt_elif(&mut self, elif: &Elif) {
        self.write_line("Elif:");
        self.with_indent(|p| {
            p.write_line("Condition:");
            p.with_indent(|p2| p2.fmt_expr(&elif.condition));

            p.write_line("Body:");
            p.with_indent(|p2| p2.fmt_stmt(&elif.body));
        });
    }

    fn fmt_enum_member(&mut self, member: &EnumMember) {
        self.write_line("EnumMember");
        self.with_indent(|p| {
            p.write_line("Name:");
            p.with_indent(|p2| p2.fmt_expr(&member.name));

            if let Some(val) = &member.value {
                p.write_line("Value:");
                p.with_indent(|p2| p2.fmt_expr(val));
            }
        });
    }

    fn fmt_variant_member(&mut self, member: &VariantMember) {
        self.write_line("VariantMember");
        self.with_indent(|p| {
            p.write_line("Name:");
            p.with_indent(|p2| p2.fmt_expr(&member.name));

            if !member.member_types.is_empty() {
                p.write_line("Types:");
                p.with_indent(|p2| {
                    for ty in &member.member_types {
                        p2.fmt_type(ty);
                    }
                });
            }
        });
    }

    // --- Expression Formatting ---

    pub fn fmt_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Literal(lit) => self.write_line(&format!("Literal({:?})", lit)),
            ExprKind::Identifier(id) => self.write_line(&format!("Identifier(\"{}\")", id)),
            ExprKind::Binary(left, op, right) => {
                self.write_line(&format!("BinaryOp({:?})", op));
                self.with_indent(|p| {
                    p.fmt_expr(left);
                    p.fmt_expr(right);
                });
            }
            ExprKind::Path(left, right) => {
                self.write_line("PathAccess");
                self.with_indent(|p| {
                    p.fmt_expr(left);
                    p.fmt_expr(right);
                });
            }
            ExprKind::Unary(op, operand) => {
                self.write_line(&format!("UnaryOp({:?})", op));
                self.with_indent(|p| p.fmt_expr(operand));
            }
            ExprKind::Unwrap(operand) => {
                self.write_line("Unwrap");
                self.with_indent(|p| p.fmt_expr(operand));
            }
            ExprKind::GenericInstantion { name, type_params } => {
                self.write_line("GenericInstantiation");
                self.with_indent(|p| {
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("TypeParams:");
                    p.with_indent(|p2| {
                        for tp in type_params {
                            p2.fmt_type(tp);
                        }
                    });
                });
            }
            ExprKind::Call(callee, args) => {
                self.write_line("Call");
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
            ExprKind::Postfix(operand, op) => {
                self.write_line(&format!("PostfixOp({:?})", op));
                self.with_indent(|p| p.fmt_expr(operand));
            }
            ExprKind::SizeOfExpr(ty) => {
                self.write_line("SizeOf");
                self.with_indent(|p| p.fmt_type(ty));
            }
            ExprKind::BitcastExpr(ty, operand) => {
                self.write_line("Bitcast");
                self.with_indent(|p| {
                    p.write_line("ToType:");
                    p.with_indent(|p2| p2.fmt_type(ty));

                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(operand));
                });
            }
            ExprKind::StaticCast(ty, operand) => {
                self.write_line("StaticCast");
                self.with_indent(|p| {
                    p.write_line("ToType:");
                    p.with_indent(|p2| p2.fmt_type(ty));

                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(operand));
                });
            }
            ExprKind::Instantiation { init_ty, body } => {
                self.write_line("Instantiation");
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
            ExprKind::Index { target, index } => {
                self.write_line("IndexAccess");
                self.with_indent(|p| {
                    p.write_line("Target:");
                    p.with_indent(|p2| p2.fmt_expr(target));

                    p.write_line("Index:");
                    p.with_indent(|p2| p2.fmt_expr(index));
                });
            }
        }
    }

    fn fmt_inst_param(&mut self, param: &InstParam) {
        self.write_line("Field:");
        self.with_indent(|p| {
            p.write_line("Name:");
            p.with_indent(|p2| p2.fmt_expr(&param.name));

            p.write_line("Value:");
            p.with_indent(|p2| p2.fmt_expr(&param.value));
        });
    }

    // --- Type Formatting ---

    pub fn fmt_type(&mut self, ty: &Type) {
        match &ty.kind {
            TypeKind::I8 => self.write_line("Type(i8)"),
            TypeKind::I16 => self.write_line("Type(i16)"),
            TypeKind::I32 => self.write_line("Type(i32)"),
            TypeKind::I64 => self.write_line("Type(i64)"),
            TypeKind::I128 => self.write_line("Type(i128)"),
            TypeKind::U8 => self.write_line("Type(u8)"),
            TypeKind::U16 => self.write_line("Type(u16)"),
            TypeKind::U32 => self.write_line("Type(u32)"),
            TypeKind::U64 => self.write_line("Type(u64)"),
            TypeKind::U128 => self.write_line("Type(u128)"),
            TypeKind::ISIZE => self.write_line("Type(isize)"),
            TypeKind::USIZE => self.write_line("Type(usize)"),
            TypeKind::F32 => self.write_line("Type(f32)"),
            TypeKind::F64 => self.write_line("Type(f64)"),
            TypeKind::Bool => self.write_line("Type(bool)"),
            TypeKind::Unit => self.write_line("Type(())"),
            TypeKind::None => self.write_line("Type(None)"),

            TypeKind::Ptr(inner) => {
                self.write_line("PtrTo:");
                self.with_indent(|p| p.fmt_type(inner));
            }
            TypeKind::Ref(inner) => {
                self.write_line("RefTo:");
                self.with_indent(|p| p.fmt_type(inner));
            }
            TypeKind::Array(inner, sz) => {
                self.write_line("ArrayType");
                self.with_indent(|p| {
                    p.write_line("ElementType:");
                    p.with_indent(|p2| p2.fmt_type(inner));

                    if let Some(size) = sz {
                        p.write_line("SizeExpr:");
                        p.with_indent(|p2| p2.fmt_expr(size));
                    }
                });
            }
            TypeKind::Func(params, ret) => {
                self.write_line("FuncPtrType");
                self.with_indent(|p| {
                    if !params.is_empty() {
                        p.write_line("Params:");
                        p.with_indent(|p2| {
                            for param in params {
                                p2.fmt_type(param);
                            }
                        });
                    }

                    if let Some(ret_ty) = ret.as_ref() {
                        p.write_line("ReturnType:");
                        p.with_indent(|p2| p2.fmt_type(ret_ty));
                    }
                });
            }
            TypeKind::CustomType(expr) => {
                self.write_line("CustomType");
                self.with_indent(|p| p.fmt_expr(expr));
            }
            TypeKind::GenericType { name, type_params } => {
                self.write_line("GenericType");
                self.with_indent(|p| {
                    p.write_line("Name:");
                    p.with_indent(|p2| p2.fmt_expr(name));

                    p.write_line("TypeParams:");
                    p.with_indent(|p2| {
                        for tp in type_params {
                            p2.fmt_type(tp);
                        }
                    });
                });
            }
            TypeKind::Nullable(inner) => {
                self.write_line("NullableType");
                self.with_indent(|p| p.fmt_type(inner));
            }
            TypeKind::Failable(ok, err) => {
                self.write_line("FailableType");
                self.with_indent(|p| {
                    p.write_line("Ok:");
                    p.with_indent(|p2| p2.fmt_type(ok));

                    p.write_line("Err:");
                    p.with_indent(|p2| p2.fmt_type(err));
                });
            }
            TypeKind::Tuple(types) => {
                self.write_line("TupleType");
                self.with_indent(|p| {
                    for ty in types {
                        p.fmt_type(ty);
                    }
                });
            }
            TypeKind::AnonStruct(fields) => {
                self.write_line("AnonStructType");
                self.with_indent(|p| {
                    for field in fields {
                        p.fmt_anon_field(field);
                    }
                });
            }
        }
    }

    fn fmt_anon_field(&mut self, field: &AnonStructField) {
        self.write_line("Field:");
        self.with_indent(|p| {
            p.write_line("Name:");
            p.with_indent(|p2| p2.fmt_expr(&field.name));

            p.write_line("Type:");
            p.with_indent(|p2| p2.fmt_type(&field.ty));
        });
    }
}

// --- Helpers ---

fn format_qualifiers(quals: &[Qualifier]) -> String {
    if quals.is_empty() {
        return String::new();
    }

    let names: Vec<String> = quals
        .iter()
        .map(|q| match q.kind {
            QualifierKind::Mut => "mut",
            QualifierKind::Const => "const",
            QualifierKind::Heap => "heap",
            QualifierKind::Exposed => "expose",
            QualifierKind::None => "none",
        })
        .map(String::from)
        .collect();

    format!("[{}]", names.join(", "))
}

// --- Display Implementations for Snapshot/Debug Integration ---

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", AstPrinter::print_stmt(self))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", AstPrinter::print_expr(self))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", AstPrinter::print_type(self))
    }
}
