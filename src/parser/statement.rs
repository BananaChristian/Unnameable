use crate::{
    ast::{Elif, EnumMember, Precedence, Qualifier, Stmt, StmtKind, VariantMember},
    diagnostics::Span,
    lexer::TType,
    parser::Parser,
};

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        match token.token_type {
            TType::Mut | TType::Expose | TType::Const | TType::Heap => self.parse_qualified_stmt(),
            TType::Var => self.parse_var(),
            TType::Func => self.parse_func(),
            TType::Struct => self.parse_struct(),
            TType::Seal => self.parse_seal(),
            TType::Methods => self.parse_methods(),
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
            TType::Break | TType::Continue => self.parse_break_or_cont(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Lowest)?;
        let span = expr.clone().span;
        Some(Stmt::new(StmtKind::Expr(expr), span))
    }

    fn parse_qualified_stmt(&mut self) -> Option<Stmt> {
        let mut qualifiers = Vec::new();

        // collect all qualifiers first
        while let Some(token) = self.current_token() {
            if Qualifier::is_valid(token) {
                qualifiers.push(Qualifier::new(token));
                self.advance();
            } else {
                break;
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
            | TType::Seal => self.parse_stmt()?,
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

        let mut ty = None;
        let next_type = self.current_token()?.token_type;

        let has_type_annotation = match next_type {
            // Complex structural types definitely mean a type annotation is here
            TType::Dot
            | TType::Lparen
            | TType::Ptr
            | TType::Ref
            | TType::Arr
            | TType::Func
            | TType::DoubleExclaim => true,

            TType::ISIZEKey
            | TType::USIZEKey
            | TType::I128Key
            | TType::U128Key
            | TType::I64Key
            | TType::U64Key
            | TType::I32Key
            | TType::U32key
            | TType::I16Key
            | TType::U16Key
            | TType::I8Key
            | TType::U8Key
            | TType::BoolKey
            | TType::F32Key
            | TType::F64Key => true,

            TType::Identifier => {
                if let Some(next) = self.peek_token() {
                    next.token_type == TType::Identifier || next.token_type == TType::Lt
                } else {
                    false
                }
            }
            _ => false,
        };

        if has_type_annotation {
            ty = self.parse_type();
        }

        let name = self.parse_identifier()?;

        let init = if self.current_token()?.token_type == TType::Bind {
            self.advance(); // Consume the := token
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(
            StmtKind::VarDecl {
                qualifiers: Vec::new(),
                type_annotation: ty,
                name: Box::new(name),
                init,
            },
            span,
        ))
    }

    pub fn parse_struct_body(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        let mut fields = Vec::new();

        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(param) = self.parse_param_decl() {
                fields.push(param);
            } else {
                let token = self.current_token()?.clone();
                self.report("Expected field declaration".to_string(), Some(token.span));
                self.advance(); // Skip
            }

            // Optional comma
            if self.current_token()?.token_type == TType::Comma {
                self.advance();
            }
        }

        let end = self.current_token()?.span.end;
        self.expect_token(TType::Rbrace)?;
        let span = Span { start, end };

        Some(Stmt::new(StmtKind::Block { content: fields }, span))
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

    fn parse_methods(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        self.expect_token(TType::Methods)?;
        let name = self.parse_identifier()?;
        let mut contents = Vec::new();
        self.expect_token(TType::LBrace)?;
        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_func() {
                match stmt.kind {
                    StmtKind::FunctionDef { .. } => {
                        contents.push(stmt);
                    }
                    _ => {
                        self.report(
                            "Only function definitions are allowed in methods".to_string(),
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
        let span = Span { start, end };
        Some(Stmt::new(
            StmtKind::MethodsStmt {
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
        self.expect_token(TType::Semicolon)?;
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
        if !Stmt::is_valid(self.current_token()?)
            && self.current_token()?.token_type != TType::End
            && self.current_token()?.token_type != TType::Rbrace
        {
            expr = self.parse_expression(Precedence::Lowest);
        }

        let end = self.current_token()?.span.end;
        Some(Stmt::new(StmtKind::Return(expr), Span { start, end }))
    }

    fn parse_break_or_cont(&mut self) -> Option<Stmt> {
        let token = self.current_token()?.clone();
        self.advance();
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

    pub fn parse_body(&mut self) -> Option<Stmt> {
        let start = self.current_token()?.span.start;
        let mut stmts = Vec::new();
        self.expect_token(TType::LBrace)?;

        while self.current_token()?.token_type != TType::Rbrace
            && self.current_token()?.token_type != TType::End
        {
            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                self.advance();
            }
        }

        self.expect_token(TType::Rbrace)?;
        let end = self.current_token()?.span.end;
        let span = Span { start, end };

        Some(Stmt::new(StmtKind::Block { content: stmts }, span))
    }
}
