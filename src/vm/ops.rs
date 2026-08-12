#[macro_export]
macro_rules! impl_numeric_op {
    ($self:expr, $a:expr, $b:expr, $op:tt) => {
        match ($a, $b) {
            (VMValue::I8(x), VMValue::I8(y)) => VMValue::I8(x $op y),
            (VMValue::U8(x), VMValue::U8(y)) => VMValue::U8(x $op y),
            (VMValue::I16(x), VMValue::I16(y)) => VMValue::I16(x $op y),
            (VMValue::U16(x), VMValue::U16(y)) => VMValue::U16(x $op y),
            (VMValue::I32(x), VMValue::I32(y)) => VMValue::I32(x $op y),
            (VMValue::U32(x), VMValue::U32(y)) => VMValue::U32(x $op y),
            (VMValue::I64(x), VMValue::I64(y)) => VMValue::I64(x $op y),
            (VMValue::U64(x), VMValue::U64(y)) => VMValue::U64(x $op y),
            (VMValue::Int(x), VMValue::Int(y)) => VMValue::Int(x $op y),
            (VMValue::UInt(x), VMValue::UInt(y)) => VMValue::UInt(x $op y),
            (VMValue::I128(x), VMValue::I128(y)) => VMValue::I128(x $op y),
            (VMValue::U128(x), VMValue::U128(y)) => VMValue::U128(x $op y),
            (VMValue::F32(x), VMValue::F32(y)) => VMValue::F32(x $op y),
            (VMValue::F64(x), VMValue::F64(y)) => VMValue::F64(x $op y),
            (a, b) => panic!("Type mismatch in binary operation: {:?} and {:?}", a, b),
        }
    };
}

#[macro_export]
macro_rules! impl_int_op {
    ($a:expr, $b:expr, $op:tt) => {
        match ($a, $b) {
            (VMValue::I8(x), VMValue::I8(y)) => VMValue::I8(x $op y),
            (VMValue::U8(x), VMValue::U8(y)) => VMValue::U8(x $op y),
            (VMValue::I16(x), VMValue::I16(y)) => VMValue::I16(x $op y),
            (VMValue::U16(x), VMValue::U16(y)) => VMValue::U16(x $op y),
            (VMValue::I32(x), VMValue::I32(y)) => VMValue::I32(x $op y),
            (VMValue::U32(x), VMValue::U32(y)) => VMValue::U32(x $op y),
            (VMValue::I64(x), VMValue::I64(y)) => VMValue::I64(x $op y),
            (VMValue::U64(x), VMValue::U64(y)) => VMValue::U64(x $op y),
            (VMValue::Int(x), VMValue::Int(y)) => VMValue::Int(x $op y),
            (VMValue::UInt(x), VMValue::UInt(y)) => VMValue::UInt(x $op y),
            (VMValue::I128(x), VMValue::I128(y)) => VMValue::I128(x $op y),
            (VMValue::U128(x), VMValue::U128(y)) => VMValue::U128(x $op y),
            (a, b) => panic!("Invalid types for bitwise operation: {:?} and {:?}", a, b),
        }
    };
}

#[macro_export]
macro_rules! impl_cmp_op {
    ($a:expr, $b:expr, $op:tt) => {
        match ($a, $b) {
            // Signed Integers
            (VMValue::I8(x), VMValue::I8(y)) => VMValue::Bool(x $op y),
            (VMValue::I16(x), VMValue::I16(y)) => VMValue::Bool(x $op y),
            (VMValue::I32(x), VMValue::I32(y)) => VMValue::Bool(x $op y),
            (VMValue::I64(x), VMValue::I64(y)) => VMValue::Bool(x $op y),
            (VMValue::I128(x), VMValue::I128(y)) => VMValue::Bool(x $op y),
            (VMValue::Int(x), VMValue::Int(y)) => VMValue::Bool(x $op y),

            // Unsigned Integers
            (VMValue::U8(x), VMValue::U8(y)) => VMValue::Bool(x $op y),
            (VMValue::U16(x), VMValue::U16(y)) => VMValue::Bool(x $op y),
            (VMValue::U32(x), VMValue::U32(y)) => VMValue::Bool(x $op y),
            (VMValue::U64(x), VMValue::U64(y)) => VMValue::Bool(x $op y),
            (VMValue::U128(x), VMValue::U128(y)) => VMValue::Bool(x $op y),
            (VMValue::UInt(x), VMValue::UInt(y)) => VMValue::Bool(x $op y),

            // Floats
            (VMValue::F32(x), VMValue::F32(y)) => VMValue::Bool(x $op y),
            (VMValue::F64(x), VMValue::F64(y)) => VMValue::Bool(x $op y),

            // Booleans (Only for Eq and Neq)
            (VMValue::Bool(x), VMValue::Bool(y)) => VMValue::Bool(x $op y),

            (a, b) => panic!("Invalid types for comparison operation: {:?} and {:?}", a, b),
        }
    };
}
