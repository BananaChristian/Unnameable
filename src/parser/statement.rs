use crate::parser::{Parser, Stmt};

impl Parser{
    pub fn parse_stmt(&mut self) -> Result<Stmt,String>{
        let expr=self.parse_literal()?;
        Ok(Stmt::Expr(expr))
    }
}
