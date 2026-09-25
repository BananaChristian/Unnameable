use crate::{
    ast::{Elif, EnumMember, Precedence, Qualifier, QualifierKind, Stmt, StmtKind, VariantMember},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl Parser {
    pub fn parse_stmt(&mut self, allow_tail: bool) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        match token.token_type {
            TType::Mut | TType::Expose | TType::Const | TType::Dollar | TType::Extern => {
                self.parse_qualified_stmt()
            }
            TType::Var => self.parse_var(),
            TType::Func => self.parse_func(),
            TType::Struct => self.parse_struct(),
            TType::Seal => self.parse_seal(),
            TType::If => self.parse_if_stmt(),
            TType::While => self.parse_while(),
            TType::Each => self.parse_each(),
            TType::For => self.parse_for(),
            TType::Generics => self.parse_generics(),
            TType::Contract => self.parse_contract(),
            TType::Enum => self.parse_enum(),
            TType::Variant => self.parse_variant(),
            TType::Return => self.parse_return(),
            TType::Alias => self.parse_alias(),
            TType::Import => self.parse_import(),
            TType::Break | TType::Continue => self.parse_break_or_cont(),
            _ => self.parse_expr_stmt(allow_tail),
        }
    }

    fn parse_expr_stmt(&mut self, allow_tail: bool) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let span = expr.clone().span;

        let is_tail = self.current_token()?.token_type != TType::Semicolon
            && allow_tail
            && matches!(self.current_token()?.token_type, TType::Rbrace | TType::End);

        if !is_tail {
            self.expect_token(TType::Semicolon)?;
        }

        let kind = if is_tail {
            StmtKind::TailExpr(Box::new(expr))
        } else {
            StmtKind::Expr(expr)
        };
        Some(Stmt::new(kind, span))
    }

    fn parse_qualified_stmt(&mut self) -> Option<Stmt> {
        let mut qualifiers = Vec::new();

        // collect all qualifiers first
        while let Some(token) = self.current_token() {
            if !Qualifier::is_valid(token) {
                break;
            }

            if token.token_type == TType::Extern {
                let start = token.span.start;
                self.advance();
                let (abi_str, end) = if self.current_token()?.token_type == TType::Identifier {
                    let ident = self.parse_identifier()?;
                    let end = ident.span.end;
                    (Some(ident), end)
                } else {
                    (None, self.current_token()?.span.end)
                };
                qualifiers.push(Qualifier {
                    kind: QualifierKind::Extern(Box::new(abi_str)),
                    span: Span::new(start, end),
                });
            } else {
                let qual = Qualifier::new(token);
                self.advance();
                qualifiers.push(qual);
            }
        }

        // parse normally
        let token = self.current_token()?;
        let mut stmt = match token.token_type {
            TType::Var
            | TType::Func
            | TType::Struct
            | TType::Enum
            | TType::Contract
            | TType::Variant
            | TType::Seal => self.parse_stmt(false)?,
            _ => {
                self.report(
                    "Expected a declaration after qualifiers".to_string(),
                    Some(token.span.clone()),
                );
                return None;
            }
        };

        // inject qualifiers into the stmt
        match &mut stmt.kind {
            StmtKind::VarDecl { qualifiers: q, .. }
            | StmtKind::FunctionDef { qualifiers: q, .. }
            | StmtKind::FunctionDecl { qualifiers: q, .. }
            | StmtKind::StructDecl { qualifiers: q, .. }
            | StmtKind::EnumStmt { qualifiers: q, .. }
            | StmtKind::ContractBlock { qualifiers: q, .. }
            | StmtKind::VariantStmt { qualifiers: q, .. }
            | StmtKind::SealStmt { qualifiers: q, .. } => {
                q.extend(qualifiers);
            }
            _ => {
                self.report(
                    "Qualifiers not valid here".to_string(),
                    Some(stmt.span.clone()),
                );
            }
        }

        Some(stmt)
    }

    fn parse_var(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Var)?;
        let name = self.parse_identifier()?;
        let ty = if self.current_token()?.token_type == TType::Colon {
            self.advance();
            let t = self.parse_type()?;
            Some(t)
        } else {
            None
        };

        self.expect_token(TType::Assign)?;
        let init = self.parse_expression(Precedence::Lowest)?;
        let end = self.current_token()?.span.end;
        self.expect_token(TType::Semicolon)?;
        let span = Span { start, end };
        Some(Stmt {
            kind: StmtKind::VarDecl {
                qualifiers: Vec::new(),
                name: Box::new(name),
                type_annotation: ty,
                init: Box::new(init),
            },
            span,
        })
    }

    pub fn parse_struct(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Struct)?;

        let name = self.parse_identifier()?;
        let mut contracts = Vec::new();
        if self.current_token()?.token_type == TType::Colon {
            self.advance();
            while self.current_token()?.token_type != TType::LBrace
                && self.current_token()?.token_type != TType::End
            {
                let contract = self.parse_type()?;
                contracts.push(contract);
                if self.current_token()?.token_type == TType::Comma {
                    self.advance();
                    continue;
                }
            }
        }

        let body = self.parse_struct_body()?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::StructDecl {
                qualifiers: Vec::new(),
                name: Box::new(name),
                contracts,
                contents: Box::new(body),
            },
            span,
        ))
    }

    fn parse_seal(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Seal)?;

        let name = self.parse_identifier()?;
        self.expect_token(TType::LBrace)?;
        let mut contents = Vec::new();
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match &stmt.kind {
                    StmtKind::FunctionDef { .. } => {
                        contents.push(stmt);
                    }
                    _ => {
                        self.report(
                            "Only function definitions are allowed in a seal".to_string(),
                            Some(stmt.span),
                        );
                    }
                }
            } else {
                self.advance();
            }
        }

        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rbrace)?;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::SealStmt {
                qualifiers: Vec::new(),
                name: Box::new(name),
                contents,
            },
            span,
        ))
    }

    fn parse_elif(&mut self) -> Option<Elif> {
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        Some(Elif {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_if_stmt(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::If)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;

        let mut elifs = Vec::new();
        if self.current_token()?.token_type == TType::Elif {
            while self.current_token()?.token_type == TType::Elif {
                self.advance(); //Consume the elif
                let elif = self.parse_elif()?;
                elifs.push(elif);
            }
        }

        let mut else_body = None;
        if self.current_token()?.token_type == TType::Else {
            self.advance(); //Consume the else
            else_body = self.parse_body();
        }

        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::IfStmt {
                condition: Box::new(condition),
                body: Box::new(body),
                elifs,
                else_body: else_body.map(Box::new),
            },
            span,
        ))
    }

    fn parse_generics(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Generics)?;
        self.expect_token(TType::Lt)?;

        let mut params = Vec::new();
        while self.current_token()?.token_type != TType::Gt
            && self.current_token()?.token_type != TType::End
        {
            let ty = self.parse_type()?;
            params.push(ty);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
        }
        self.expect_token(TType::Gt)?;

        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;

        Some(Stmt::new(
            StmtKind::GenericBlock {
                params,
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_contract(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Contract)?;
        let name = self.parse_identifier()?;
        let mut contents = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match stmt.kind {
                    StmtKind::FunctionDecl { .. } => contents.push(stmt),
                    _ => {
                        self.report(
                            "Only function declarations are allowed in a contract".to_string(),
                            Some(stmt.span),
                        );
                    }
                }
            } else {
                self.advance();
            }
        }
        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::ContractBlock {
                qualifiers: Vec::new(),
                name: Box::new(name),
                body: contents,
            },
            Span { start, end },
        ))
    }

    fn parse_while(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::While)?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::WhileStmt {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_for(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::For)?;
        let init = self.parse_var()?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_token(TType::Semicolon)?;
        let update = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::ForStmt {
                init: Box::new(init),
                condition: Box::new(condition),
                update: Box::new(update),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_each(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Each)?;
        let item = self.parse_identifier()?;
        self.expect_token(TType::In)?;
        let collection = self.parse_expression(Precedence::Lowest)?;
        let body = self.parse_body()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::EachStmt {
                item: Box::new(item),
                collection: Box::new(collection),
                body: Box::new(body),
            },
            Span { start, end },
        ))
    }

    fn parse_enum_member(&mut self) -> Option<EnumMember> {
        let start = self.current_token()?.span.start;
        let name = self.parse_identifier()?;
        let mut value = None;
        if self.current_token()?.token_type == TType::Assign {
            self.advance();
            value = self.parse_expression(Precedence::Lowest);
        }
        let end = self.current_token()?.span.end;

        Some(EnumMember {
            name,
            value,
            span: Span { start, end },
        })
    }

    fn parse_enum(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Enum)?;
        let name = self.parse_identifier()?;
        let mut underlying = None;
        if self.current_token()?.token_type == TType::Colon {
            self.advance();
            underlying = self.parse_type();
        }

        let mut members = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            let member = self.parse_enum_member()?;
            members.push(member);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
            }
        }

        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rbrace)?;
        Some(Stmt::new(
            StmtKind::EnumStmt {
                qualifiers: Vec::new(),
                name: Box::new(name),
                underlying,
                content: members,
            },
            Span { start, end },
        ))
    }

    fn parse_variant_member(&mut self) -> Option<VariantMember> {
        let start = self.current_token()?.span.start;
        let name = self.parse_identifier()?;
        let mut types = Vec::new();
        if self.current_token()?.token_type == TType::Lparen {
            self.advance();
            while self.current_token()?.token_type != TType::Rparen
                && self.current_token()?.token_type != TType::End
            {
                let ty = self.parse_type()?;
                types.push(ty);
                if self.current_token()?.token_type == TType::Comma {
                    self.advance();
                    continue;
                }
            }
            self.expect_token(TType::Rparen)?;
        }

        let end = self.current_token()?.span.end;
        Some(VariantMember {
            name,
            member_types: types,
            span: Span { start, end },
        })
    }

    fn parse_variant(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Variant)?;
        let name = self.parse_identifier()?;

        let mut contracts = Vec::new();
        if self.current_token()?.token_type == TType::Colon {
            self.advance();
            while self.current_token()?.token_type != TType::LBrace
                && self.current_token()?.token_type != TType::End
            {
                let contract = self.parse_type()?;
                contracts.push(contract);
                if self.current_token()?.token_type == TType::Comma {
                    self.advance();
                    continue;
                }
            }
        }

        let mut members = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            let member = self.parse_variant_member()?;
            members.push(member);
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
                continue;
            }
        }

        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rbrace)?;
        Some(Stmt::new(
            StmtKind::VariantStmt {
                qualifiers: Vec::new(),
                name: Box::new(name),
                contracts,
                body: members,
            },
            Span { start, end },
        ))
    }

    fn parse_return(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Return)?;

        let mut expr = None;
        if self.current_token()?.token_type == TType::Semicolon {
            let end = self.current_token()?.span.end;
            self.advance();
            return Some(Stmt::new(StmtKind::Return(expr), Span { start, end }));
        } else {
            if self.current_token()?.token_type != TType::Semicolon
                && self.current_token()?.token_type != TType::End
                && self.current_token()?.token_type != TType::Rbrace
            {
                expr = self.parse_expression(Precedence::Lowest);
            }

            self.expect_token(TType::Semicolon)?;
            let end = self.current_token()?.span.end;
            return Some(Stmt::new(StmtKind::Return(expr), Span { start, end }));
        }
    }

    fn parse_break_or_cont(&mut self) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        self.advance();
        // A trailing semicolon is optional.
        if let Some(next) = self.current_token() {
            if next.token_type == TType::Semicolon {
                self.advance();
            }
        }
        match token.token_type {
            TType::Break => Some(Stmt::new(StmtKind::Break, token.span)),
            TType::Continue => Some(Stmt::new(StmtKind::Continue, token.span)),
            _ => None,
        }
    }

    fn parse_alias(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Alias)?;
        let original = self.parse_type()?;
        self.expect_token(TType::As)?;
        let new = self.parse_identifier()?;
        let end = self.current_token()?.span.end;
        Some(Stmt::new(
            StmtKind::AliasStmt {
                original: Box::new(original),
                new: Box::new(new),
            },
            Span { start, end },
        ))
    }

    fn parse_import(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Import)?;
        //It is called path I think
        let name = self.parse_path_or_identifier()?;
        let mid = self.current_token()?.span.end;

        if self.current_token()?.token_type == TType::As {
            self.advance(); //Consume the as token
            let alias = Some(self.parse_identifier()?);
            let end = self.current_token()?.span.end;
            Some(Stmt {
                kind: StmtKind::ImportStmt {
                    name: Box::new(name),
                    alias,
                },
                span: Span { start, end },
            })
        } else {
            Some(Stmt {
                kind: StmtKind::ImportStmt {
                    name: Box::new(name),
                    alias: None,
                },
                span: Span { start, end: mid },
            })
        }
    }
}
