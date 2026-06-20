use crate::parser::{Parser, Stmt, ast::StmtKind, precedence::Precedence};

impl <'a>Parser<'a>{
    pub fn parse_stmt(&mut self) -> Option<Stmt>{
        let expr=self.parse_expression(Precedence::Lowest)?;
        let span=expr.span;
        Some(Stmt::new(StmtKind::Expr(expr), span))
    }
}
