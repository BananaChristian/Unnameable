#[derive(Debug,Clone,PartialEq)]
pub enum Literal {
    // Integers with explicit types
    Int8(i8),
    Uint8(u8),
    Int16(i16),
    Uint16(u16),
    Int32(i32),
    Uint32(u32),
    Int64(i64),
    Uint64(u64),
    Int128(i128),
    Uint128(u128),
    IntSize(isize),  // iz
    UintSize(usize), // uz
    
    // Plain int (no suffix) - default to i64
    Int(i64),
    
    // Floats
    Float(f64),
    F32(f32),
    F64(f64),
    
    // Booleans
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
}


#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
}
