use crate::derivatives::*;
use std::collections::HashMap;

/// Signature infomation to refer to specific method.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct MethodSignature {
    name: String,
    reciever_type: String,
    input_types: Vec<String>,
}
impl MethodSignature {
    pub fn new(name: String, reciever_type: String, input_types: Vec<String>) -> Self {
        Self {
            name,
            reciever_type,
            input_types,
        }
    }
}
impl<const N: usize> From<(&'static str, &'static str, &'static [&'static str; N])>
    for MethodSignature
{
    fn from(
        (name, reciever_type, input_types): (
            &'static str,
            &'static str,
            &'static [&'static str; N],
        ),
    ) -> Self {
        Self {
            name: String::from(name),
            reciever_type: String::from(reciever_type),
            input_types: input_types.iter().map(|s| String::from(*s)).collect(),
        }
    }
}
/// A map of method signatures to useful data (output type, etc.).
type MethodMap = HashMap<MethodSignature, ProcedureOutputs>;
/// Signature infomation to refer to specific function.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct FunctionSignature {
    name: String,
    input_types: Vec<String>,
}
impl FunctionSignature {
    pub fn new(name: String, input_types: Vec<String>) -> Self {
        Self { name, input_types }
    }
}
impl<const N: usize> From<(&'static str, &'static [&'static str; N])> for FunctionSignature {
    fn from((name, input_types): (&'static str, &'static [&'static str; N])) -> Self {
        Self {
            name: String::from(name),
            input_types: input_types.iter().map(|s| String::from(*s)).collect(),
        }
    }
}
/// A map of function signatures to useful data (output type, etc.).
type FunctionMap = HashMap<FunctionSignature, ProcedureOutputs>;

/// Infomation to relating to specific procedure, output type, etc. (including functions for transforming statements into deriatives).
pub struct ProcedureOutputs {
    /// Output type of procedure
    pub output_type: String,
    /// Transformation procedure to give the forward derivative
    pub forward_derivative: fn(&syn::Stmt, &[String]) -> syn::Stmt,
    /// Transfomation procedure to give the reverse derivative
    pub reverse_derivative: fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
}
impl ProcedureOutputs {
    pub fn new(
        output_type: &'static str,
        forward_derivative: fn(&syn::Stmt, &[String]) -> syn::Stmt,
        reverse_derivative: fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
    ) -> Self {
        Self {
            output_type: String::from(output_type),
            forward_derivative,
            reverse_derivative,
        }
    }
}
// TODO Why doesn't this work?
impl
    From<(
        &'static str,
        fn(&syn::Stmt, &[String]) -> syn::Stmt,
        fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
    )> for ProcedureOutputs
{
    fn from(
        (output_type, forward_derivative, reverse_derivative): (
            &'static str,
            fn(&syn::Stmt, &[String]) -> syn::Stmt,
            fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
        ),
    ) -> Self {
        Self {
            output_type: String::from(output_type),
            forward_derivative,
            reverse_derivative,
        }
    }
}

/// Currently supported binary operations.
#[derive(Hash, PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl TryFrom<&'static str> for BinOp {
    type Error = &'static str;
    fn try_from(symbol: &'static str) -> Result<Self, Self::Error> {
        match symbol {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mul),
            "/" => Ok(Self::Div),
            _ => Err("Unrecognized symbol"),
        }
    }
}
impl TryFrom<syn::BinOp> for BinOp {
    type Error = &'static str;
    fn try_from(op: syn::BinOp) -> Result<Self, Self::Error> {
        match op {
            syn::BinOp::Add(_) => Ok(Self::Add),
            syn::BinOp::Sub(_) => Ok(Self::Sub),
            syn::BinOp::Mul(_) => Ok(Self::Mul),
            syn::BinOp::Div(_) => Ok(Self::Div),
            _ => Err("Unrecognized syn op"),
        }
    }
}

/// Signature infomation to refer to specific binary operation.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct OperationSignature {
    /// Left-hand-side type
    lhs: String,
    /// Oeration type
    op: BinOp,
    /// Right-hand-side type
    rhs: String,
}
impl From<(&'static str, &'static str, &'static str)> for OperationSignature {
    fn from((lhs, op, rhs): (&'static str, &'static str, &'static str)) -> Self {
        Self {
            lhs: String::from(lhs),
            op: BinOp::try_from(op).expect("No symbol"),
            rhs: String::from(rhs),
        }
    }
}
impl From<(String, syn::BinOp, String)> for OperationSignature {
    fn from((lhs, op, rhs): (String, syn::BinOp, String)) -> Self {
        Self {
            lhs,
            op: BinOp::try_from(op).expect("No op"),
            rhs,
        }
    }
}
/// A map of binary operation signatures to useful data (output type, etc.).
type OperationMap = HashMap<OperationSignature, ProcedureOutputs>;

// Supported methods, functions and operations.
lazy_static::lazy_static! {
    /// Internal map of currently supported functions.
    pub static ref SUPPORTED_FUNCTIONS: FunctionMap = {
        let map = FunctionMap::new();
        map
    };
    /// Internal map of currently supported methods.
    pub static ref SUPPORTED_METHODS: MethodMap = {
        let mut map = MethodMap::new();
        // f32
        // ----------------------------
        // Exponents
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",forward_powi::<{Type::F64}>,reverse_powi::<{Type::F64}>));
        map.insert(("powf","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward_powf::<{Type::F64}>,reverse_powf::<{Type::F64}>));
        map.insert(("sqrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sqrt::<{Type::F64}>,reverse_sqrt::<{Type::F64}>));
        map.insert(("cbrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cbrt::<{Type::F64}>,reverse_cbrt::<{Type::F64}>));
        map.insert(("exp","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp::<{Type::F64}>,reverse_exp::<{Type::F64}>));
        map.insert(("exp2","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp2::<{Type::F64}>,reverse_exp2::<{Type::F64}>));
        map.insert(("exp_m1","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp_m1::<{Type::F64}>,reverse_exp_m1::<{Type::F64}>));
        // Logs
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ln::<{Type::F64}>,reverse_ln::<{Type::F64}>));
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ln_1p::<{Type::F64}>,reverse_ln_1p::<{Type::F64}>));
        map.insert(("log","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward_log::<{Type::F64}>,reverse_log::<{Type::F64}>));
        map.insert(("log10","f64",&[]).into(),ProcedureOutputs::new("f64",forward_log10::<{Type::F64}>,reverse_log10::<{Type::F64}>));
        map.insert(("log2","f64",&[]).into(),ProcedureOutputs::new("f64",forward_log2::<{Type::F64}>,reverse_log2::<{Type::F64}>));
        // Trig
        map.insert(("acos","f64",&[]).into(),ProcedureOutputs::new("f64",forward_acos::<{Type::F64}>,reverse_acos::<{Type::F64}>));
        map.insert(("acosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_acosh::<{Type::F64}>,reverse_acosh::<{Type::F64}>));
        map.insert(("asin","f64",&[]).into(),ProcedureOutputs::new("f64",forward_asin::<{Type::F64}>,reverse_asin::<{Type::F64}>));
        map.insert(("asinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_asinh::<{Type::F64}>,reverse_asinh::<{Type::F64}>));
        map.insert(("atan","f64",&[]).into(),ProcedureOutputs::new("f64",forward_atan::<{Type::F64}>,reverse_atan::<{Type::F64}>));
        map.insert(("sin","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sin::<{Type::F64}>,reverse_sin::<{Type::F64}>));
        map.insert(("atanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_atanh::<{Type::F64}>,reverse_atanh::<{Type::F64}>));
        map.insert(("cos","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cos::<{Type::F64}>,reverse_cos::<{Type::F64}>));
        map.insert(("cosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cosh::<{Type::F64}>,reverse_cosh::<{Type::F64}>));
        map.insert(("sinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sinh::<{Type::F64}>,reverse_sinh::<{Type::F64}>));
        map.insert(("tan","f64",&[]).into(),ProcedureOutputs::new("f64",forward_tan::<{Type::F64}>,reverse_tan::<{Type::F64}>));
        map.insert(("tanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_tanh::<{Type::F64}>,reverse_tanh::<{Type::F64}>));
        // Misc
        map.insert(("abs","f64",&[]).into(),ProcedureOutputs::new("f64",forward_abs::<{Type::F64}>,reverse_abs::<{Type::F64}>));
        map.insert(("ceil","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ceil::<{Type::F64}>,reverse_ceil::<{Type::F64}>));
        map.insert(("floor","f64",&[]).into(),ProcedureOutputs::new("f64",forward_floor::<{Type::F64}>,reverse_floor::<{Type::F64}>));
        map.insert(("fract","f64",&[]).into(),ProcedureOutputs::new("f64",forward_fract::<{Type::F64}>,reverse_fract::<{Type::F64}>));
        map.insert(("recip","f64",&[]).into(),ProcedureOutputs::new("f64",forward_recip::<{Type::F64}>,reverse_recip::<{Type::F64}>));
        map.insert(("round","f64",&[]).into(),ProcedureOutputs::new("f64",forward_round::<{Type::F64}>,reverse_round::<{Type::F64}>));

        // f64
        // ----------------------------
        // Exponents
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",forward_powi::<{Type::F64}>,reverse_powi::<{Type::F64}>));
        map.insert(("powf","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward_powf::<{Type::F64}>,reverse_powf::<{Type::F64}>));
        map.insert(("sqrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sqrt::<{Type::F64}>,reverse_sqrt::<{Type::F64}>));
        map.insert(("cbrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cbrt::<{Type::F64}>,reverse_cbrt::<{Type::F64}>));
        map.insert(("exp","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp::<{Type::F64}>,reverse_exp::<{Type::F64}>));
        map.insert(("exp2","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp2::<{Type::F64}>,reverse_exp2::<{Type::F64}>));
        map.insert(("exp_m1","f64",&[]).into(),ProcedureOutputs::new("f64",forward_exp_m1::<{Type::F64}>,reverse_exp_m1::<{Type::F64}>));
        // Logs
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ln::<{Type::F64}>,reverse_ln::<{Type::F64}>));
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ln_1p::<{Type::F64}>,reverse_ln_1p::<{Type::F64}>));
        map.insert(("log","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward_log::<{Type::F64}>,reverse_log::<{Type::F64}>));
        map.insert(("log10","f64",&[]).into(),ProcedureOutputs::new("f64",forward_log10::<{Type::F64}>,reverse_log10::<{Type::F64}>));
        map.insert(("log2","f64",&[]).into(),ProcedureOutputs::new("f64",forward_log2::<{Type::F64}>,reverse_log2::<{Type::F64}>));
        // Trig
        map.insert(("acos","f64",&[]).into(),ProcedureOutputs::new("f64",forward_acos::<{Type::F64}>,reverse_acos::<{Type::F64}>));
        map.insert(("acosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_acosh::<{Type::F64}>,reverse_acosh::<{Type::F64}>));
        map.insert(("asin","f64",&[]).into(),ProcedureOutputs::new("f64",forward_asin::<{Type::F64}>,reverse_asin::<{Type::F64}>));
        map.insert(("asinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_asinh::<{Type::F64}>,reverse_asinh::<{Type::F64}>));
        map.insert(("atan","f64",&[]).into(),ProcedureOutputs::new("f64",forward_atan::<{Type::F64}>,reverse_atan::<{Type::F64}>));
        map.insert(("sin","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sin::<{Type::F64}>,reverse_sin::<{Type::F64}>));
        map.insert(("atanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_atanh::<{Type::F64}>,reverse_atanh::<{Type::F64}>));
        map.insert(("cos","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cos::<{Type::F64}>,reverse_cos::<{Type::F64}>));
        map.insert(("cosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_cosh::<{Type::F64}>,reverse_cosh::<{Type::F64}>));
        map.insert(("sinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_sinh::<{Type::F64}>,reverse_sinh::<{Type::F64}>));
        map.insert(("tan","f64",&[]).into(),ProcedureOutputs::new("f64",forward_tan::<{Type::F64}>,reverse_tan::<{Type::F64}>));
        map.insert(("tanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward_tanh::<{Type::F64}>,reverse_tanh::<{Type::F64}>));
        // Misc
        map.insert(("abs","f64",&[]).into(),ProcedureOutputs::new("f64",forward_abs::<{Type::F64}>,reverse_abs::<{Type::F64}>));
        map.insert(("ceil","f64",&[]).into(),ProcedureOutputs::new("f64",forward_ceil::<{Type::F64}>,reverse_ceil::<{Type::F64}>));
        map.insert(("floor","f64",&[]).into(),ProcedureOutputs::new("f64",forward_floor::<{Type::F64}>,reverse_floor::<{Type::F64}>));
        map.insert(("fract","f64",&[]).into(),ProcedureOutputs::new("f64",forward_fract::<{Type::F64}>,reverse_fract::<{Type::F64}>));
        map.insert(("recip","f64",&[]).into(),ProcedureOutputs::new("f64",forward_recip::<{Type::F64}>,reverse_recip::<{Type::F64}>));
        map.insert(("round","f64",&[]).into(),ProcedureOutputs::new("f64",forward_round::<{Type::F64}>,reverse_round::<{Type::F64}>));

        // Return
        // ------------------------------------------------------------------------------------
        map
    };
    /// Internal map of currently supported operations.
    pub static ref SUPPORTED_OPERATIONS: OperationMap = {
        let mut map = OperationMap::new();
        // f32 arithmetics
        map.insert(("f32","+","f32").into(),ProcedureOutputs::new("f32",forward_add::<{Type::F32}>,reverse_add::<{Type::F32}>));
        map.insert(("f32","*","f32").into(),ProcedureOutputs::new("f32",forward_mul::<{Type::F32}>,reverse_mul::<{Type::F32}>));
        map.insert(("f32","/","f32").into(),ProcedureOutputs::new("f32",forward_div::<{Type::F32}>,reverse_div::<{Type::F32}>));
        map.insert(("f32","-","f32").into(),ProcedureOutputs::new("f32",forward_sub::<{Type::F32}>,reverse_sub::<{Type::F32}>));
        // f64 arithmetics
        map.insert(("f64","+","f64").into(),ProcedureOutputs::new("f64",forward_add::<{Type::F64}>,reverse_add::<{Type::F64}>));
        map.insert(("f64","*","f64").into(),ProcedureOutputs::new("f64",forward_mul::<{Type::F64}>,reverse_mul::<{Type::F64}>));
        map.insert(("f64","/","f64").into(),ProcedureOutputs::new("f64",forward_div::<{Type::F64}>,reverse_div::<{Type::F64}>));
        map.insert(("f64","-","f64").into(),ProcedureOutputs::new("f64",forward_sub::<{Type::F64}>,reverse_sub::<{Type::F64}>));
        // i8 arithmetics
        map.insert(("i8","+","i8").into(),ProcedureOutputs::new("i8",forward_add::<{Type::I8}>,reverse_add::<{Type::I8}>));
        map.insert(("i8","*","i8").into(),ProcedureOutputs::new("i8",forward_mul::<{Type::I8}>,reverse_mul::<{Type::I8}>));
        map.insert(("i8","/","i8").into(),ProcedureOutputs::new("i8",forward_div::<{Type::I8}>,reverse_div::<{Type::I8}>));
        map.insert(("i8","-","i8").into(),ProcedureOutputs::new("i8",forward_sub::<{Type::I8}>,reverse_sub::<{Type::I8}>));
        // i16 arithmetics
        map.insert(("i16","+","i16").into(),ProcedureOutputs::new("i16",forward_add::<{Type::I16}>,reverse_add::<{Type::I16}>));
        map.insert(("i16","*","i16").into(),ProcedureOutputs::new("i16",forward_mul::<{Type::I16}>,reverse_mul::<{Type::I16}>));
        map.insert(("i16","/","i16").into(),ProcedureOutputs::new("i16",forward_div::<{Type::I16}>,reverse_div::<{Type::I16}>));
        map.insert(("i16","-","i16").into(),ProcedureOutputs::new("i16",forward_sub::<{Type::I16}>,reverse_sub::<{Type::I16}>));
        // i32 arithmetics
        map.insert(("i32","+","i32").into(),ProcedureOutputs::new("i32",forward_add::<{Type::I32}>,reverse_add::<{Type::I32}>));
        map.insert(("i32","*","i32").into(),ProcedureOutputs::new("i32",forward_mul::<{Type::I32}>,reverse_mul::<{Type::I32}>));
        map.insert(("i32","/","i32").into(),ProcedureOutputs::new("i32",forward_div::<{Type::I32}>,reverse_div::<{Type::I32}>));
        map.insert(("i32","-","i32").into(),ProcedureOutputs::new("i32",forward_sub::<{Type::I32}>,reverse_sub::<{Type::I32}>));
        // i64 arithmetics
        map.insert(("i64","+","i64").into(),ProcedureOutputs::new("i64",forward_add::<{Type::I64}>,reverse_add::<{Type::I64}>));
        map.insert(("i64","*","i64").into(),ProcedureOutputs::new("i64",forward_mul::<{Type::I64}>,reverse_mul::<{Type::I64}>));
        map.insert(("i64","/","i64").into(),ProcedureOutputs::new("i64",forward_div::<{Type::I64}>,reverse_div::<{Type::I64}>));
        map.insert(("i64","-","i64").into(),ProcedureOutputs::new("i64",forward_sub::<{Type::I64}>,reverse_sub::<{Type::I64}>));
        // i128 arithmetics
        map.insert(("i128","+","i128").into(),ProcedureOutputs::new("i128",forward_add::<{Type::I128}>,reverse_add::<{Type::I128}>));
        map.insert(("i128","*","i128").into(),ProcedureOutputs::new("i128",forward_mul::<{Type::I128}>,reverse_mul::<{Type::I128}>));
        map.insert(("i128","/","i128").into(),ProcedureOutputs::new("i128",forward_div::<{Type::I128}>,reverse_div::<{Type::I128}>));
        map.insert(("i128","-","i128").into(),ProcedureOutputs::new("i128",forward_sub::<{Type::I128}>,reverse_sub::<{Type::I128}>));
        // u8 arithmetics
        map.insert(("u8","+","u8").into(),ProcedureOutputs::new("u8",forward_add::<{Type::U8}>,reverse_add::<{Type::U8}>));
        map.insert(("u8","*","u8").into(),ProcedureOutputs::new("u8",forward_mul::<{Type::U8}>,reverse_mul::<{Type::U8}>));
        map.insert(("u8","/","u8").into(),ProcedureOutputs::new("u8",forward_div::<{Type::U8}>,reverse_div::<{Type::U8}>));
        map.insert(("u8","-","u8").into(),ProcedureOutputs::new("u8",forward_sub::<{Type::U8}>,reverse_sub::<{Type::U8}>));
        // u16 arithmetics
        map.insert(("u16","+","u16").into(),ProcedureOutputs::new("u16",forward_add::<{Type::U16}>,reverse_add::<{Type::U16}>));
        map.insert(("u16","*","u16").into(),ProcedureOutputs::new("u16",forward_mul::<{Type::U16}>,reverse_mul::<{Type::U16}>));
        map.insert(("u16","/","u16").into(),ProcedureOutputs::new("u16",forward_div::<{Type::U16}>,reverse_div::<{Type::U16}>));
        map.insert(("u16","-","u16").into(),ProcedureOutputs::new("u16",forward_sub::<{Type::U16}>,reverse_sub::<{Type::U16}>));
        // u32 arithmetics
        map.insert(("u32","+","u32").into(),ProcedureOutputs::new("u32",forward_add::<{Type::U32}>,reverse_add::<{Type::U32}>));
        map.insert(("u32","*","u32").into(),ProcedureOutputs::new("u32",forward_mul::<{Type::U32}>,reverse_mul::<{Type::U32}>));
        map.insert(("u32","/","u32").into(),ProcedureOutputs::new("u32",forward_div::<{Type::U32}>,reverse_div::<{Type::U32}>));
        map.insert(("u32","-","u32").into(),ProcedureOutputs::new("u32",forward_sub::<{Type::U32}>,reverse_sub::<{Type::U32}>));
        // u64 arithmetics
        map.insert(("u64","+","u64").into(),ProcedureOutputs::new("u64",forward_add::<{Type::U64}>,reverse_add::<{Type::U64}>));
        map.insert(("u64","*","u64").into(),ProcedureOutputs::new("u64",forward_mul::<{Type::U64}>,reverse_mul::<{Type::U64}>));
        map.insert(("u64","/","u64").into(),ProcedureOutputs::new("u64",forward_div::<{Type::U64}>,reverse_div::<{Type::U64}>));
        map.insert(("u64","-","u64").into(),ProcedureOutputs::new("u64",forward_sub::<{Type::U64}>,reverse_sub::<{Type::U64}>));
        // u128 arithmetics
        map.insert(("u128","+","u128").into(),ProcedureOutputs::new("u128",forward_add::<{Type::U128}>,reverse_add::<{Type::U128}>));
        map.insert(("u128","*","u128").into(),ProcedureOutputs::new("u128",forward_mul::<{Type::U128}>,reverse_mul::<{Type::U128}>));
        map.insert(("u128","/","u128").into(),ProcedureOutputs::new("u128",forward_div::<{Type::U128}>,reverse_div::<{Type::U128}>));
        map.insert(("u128","-","u128").into(),ProcedureOutputs::new("u128",forward_sub::<{Type::U128}>,reverse_sub::<{Type::U128}>));
        map
    };
}
