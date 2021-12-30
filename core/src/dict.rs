use crate::derivatives::{forward::FgdType, reverse::*, *};
use std::collections::HashMap;

use std::fmt;
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
impl fmt::Display for MethodSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}({})",
            self.reciever_type,
            self.name,
            self.input_types
                .iter()
                .cloned()
                .intersperse(String::from(","))
                .collect::<String>()
        )
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
impl fmt::Display for FunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.input_types
                .iter()
                .cloned()
                .intersperse(String::from(","))
                .collect::<String>()
        )
    }
}
/// A map of function signatures to useful data (output type, etc.).
type FunctionMap = HashMap<FunctionSignature, ProcedureOutputs>;

/// Infomation to relating to specific procedure, output type, etc. (including functions for transforming statements into deriatives).
pub struct ProcedureOutputs {
    /// Output type of procedure
    pub output_type: String,
    /// Transformation procedure to give the forward derivative
    pub forward_derivative: FgdType,
    /// Transfomation procedure to give the reverse derivative
    pub reverse_derivative: fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
}
impl ProcedureOutputs {
    pub fn new(
        output_type: &'static str,
        forward_derivative: FgdType,
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
        FgdType,
        fn(&syn::Stmt, &mut HashMap<String, Vec<String>>) -> Option<syn::Stmt>,
    )> for ProcedureOutputs
{
    fn from(
        (output_type, forward_derivative, reverse_derivative): (
            &'static str,
            FgdType,
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
impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
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
impl fmt::Display for OperationSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.lhs, self.op, self.rhs)
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
        map.insert(("powi","f32",&["i32"]).into(),ProcedureOutputs::new("f32",forward::powi_f32,reverse_powi::<{Type::F32}>));
        map.insert(("powf","f32",&["f32"]).into(),ProcedureOutputs::new("f32",forward::powf_f32,reverse_powf::<{Type::F32}>));
        map.insert(("sqrt","f32",&[]).into(),ProcedureOutputs::new("f32",forward::sqrt_f32,reverse_sqrt::<{Type::F32}>));
        map.insert(("cbrt","f32",&[]).into(),ProcedureOutputs::new("f32",forward::cbrt_f32,reverse_cbrt::<{Type::F32}>));
        map.insert(("exp","f32",&[]).into(),ProcedureOutputs::new("f32",forward::exp_f32,reverse_exp::<{Type::F32}>));
        map.insert(("exp2","f32",&[]).into(),ProcedureOutputs::new("f32",forward::exp2_f32,reverse_exp2::<{Type::F32}>));
        map.insert(("exp_m1","f32",&[]).into(),ProcedureOutputs::new("f32",forward::exp_m1_f32,reverse_exp_m1::<{Type::F32}>));
        // Logs
        map.insert(("ln","f32",&[]).into(),ProcedureOutputs::new("f32",forward::ln_f32,reverse_ln::<{Type::F32}>));
        map.insert(("ln_1p","f32",&[]).into(),ProcedureOutputs::new("f32",forward::ln_1p_f32,reverse_ln_1p::<{Type::F32}>));
        map.insert(("log","f32",&["f32"]).into(),ProcedureOutputs::new("f32",forward::log_f32,reverse_log::<{Type::F32}>));
        map.insert(("log10","f32",&[]).into(),ProcedureOutputs::new("f32",forward::log10_f32,reverse_log10::<{Type::F32}>));
        map.insert(("log2","f32",&[]).into(),ProcedureOutputs::new("f32",forward::log2_f32,reverse_log2::<{Type::F32}>));
        // Trig
        map.insert(("acos","f32",&[]).into(),ProcedureOutputs::new("f32",forward::acos_f32,reverse_acos::<{Type::F32}>));
        map.insert(("acosh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::acosh_f32,reverse_acosh::<{Type::F32}>));
        map.insert(("asin","f32",&[]).into(),ProcedureOutputs::new("f32",forward::asin_f32,reverse_asin::<{Type::F32}>));
        map.insert(("asinh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::asinh_f32,reverse_asinh::<{Type::F32}>));
        map.insert(("atan","f32",&[]).into(),ProcedureOutputs::new("f32",forward::atan_f32,reverse_atan::<{Type::F32}>));
        map.insert(("sin","f32",&[]).into(),ProcedureOutputs::new("f32",forward::sin_f32,reverse_sin::<{Type::F32}>));
        map.insert(("atanh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::atanh_f32,reverse_atanh::<{Type::F32}>));
        map.insert(("cos","f32",&[]).into(),ProcedureOutputs::new("f32",forward::cos_f32,reverse_cos::<{Type::F32}>));
        map.insert(("cosh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::cosh_f32,reverse_cosh::<{Type::F32}>));
        map.insert(("sinh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::sinh_f32,reverse_sinh::<{Type::F32}>));
        map.insert(("tan","f32",&[]).into(),ProcedureOutputs::new("f32",forward::tan_f32,reverse_tan::<{Type::F32}>));
        // map.insert(("tanh","f32",&[]).into(),ProcedureOutputs::new("f32",forward::tanh_f32,reverse_tanh::<{Type::F32}>));
        // Misc
        map.insert(("abs","f32",&[]).into(),ProcedureOutputs::new("f32",forward::abs_f32,reverse_abs::<{Type::F32}>));
        map.insert(("ceil","f32",&[]).into(),ProcedureOutputs::new("f32",forward::ceil_f32,reverse_ceil::<{Type::F32}>));
        map.insert(("floor","f32",&[]).into(),ProcedureOutputs::new("f32",forward::floor_f32,reverse_floor::<{Type::F32}>));
        map.insert(("fract","f32",&[]).into(),ProcedureOutputs::new("f32",forward::fract_f32,reverse_fract::<{Type::F32}>));
        map.insert(("recip","f32",&[]).into(),ProcedureOutputs::new("f32",forward::recip_f32,reverse_recip::<{Type::F32}>));
        map.insert(("round","f32",&[]).into(),ProcedureOutputs::new("f32",forward::round_f32,reverse_round::<{Type::F32}>));

        // f64
        // ----------------------------
        // Exponents
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",forward::powi_f64,reverse_powi::<{Type::F32}>));
        map.insert(("powf","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward::powf_f64,reverse_powf::<{Type::F32}>));
        map.insert(("sqrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward::sqrt_f64,reverse_sqrt::<{Type::F32}>));
        map.insert(("cbrt","f64",&[]).into(),ProcedureOutputs::new("f64",forward::cbrt_f64,reverse_cbrt::<{Type::F32}>));
        map.insert(("exp","f64",&[]).into(),ProcedureOutputs::new("f64",forward::exp_f64,reverse_exp::<{Type::F32}>));
        map.insert(("exp2","f64",&[]).into(),ProcedureOutputs::new("f64",forward::exp2_f64,reverse_exp2::<{Type::F32}>));
        map.insert(("exp_m1","f64",&[]).into(),ProcedureOutputs::new("f64",forward::exp_m1_f64,reverse_exp_m1::<{Type::F32}>));
        // Logs
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",forward::ln_f64,reverse_ln::<{Type::F32}>));
        map.insert(("ln_1p","f64",&[]).into(),ProcedureOutputs::new("f64",forward::ln_1p_f64,reverse_ln_1p::<{Type::F32}>));
        map.insert(("log","f64",&["f64"]).into(),ProcedureOutputs::new("f64",forward::log_f64,reverse_log::<{Type::F32}>));
        map.insert(("log10","f64",&[]).into(),ProcedureOutputs::new("f64",forward::log10_f64,reverse_log10::<{Type::F32}>));
        map.insert(("log2","f64",&[]).into(),ProcedureOutputs::new("f64",forward::log2_f64,reverse_log2::<{Type::F32}>));
        // Trig
        map.insert(("acos","f64",&[]).into(),ProcedureOutputs::new("f64",forward::acos_f64,reverse_acos::<{Type::F32}>));
        map.insert(("acosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::acosh_f64,reverse_acosh::<{Type::F32}>));
        map.insert(("asin","f64",&[]).into(),ProcedureOutputs::new("f64",forward::asin_f64,reverse_asin::<{Type::F32}>));
        map.insert(("asinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::asinh_f64,reverse_asinh::<{Type::F32}>));
        map.insert(("atan","f64",&[]).into(),ProcedureOutputs::new("f64",forward::atan_f64,reverse_atan::<{Type::F32}>));
        map.insert(("sin","f64",&[]).into(),ProcedureOutputs::new("f64",forward::sin_f64,reverse_sin::<{Type::F32}>));
        map.insert(("atanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::atanh_f64,reverse_atanh::<{Type::F32}>));
        map.insert(("cos","f64",&[]).into(),ProcedureOutputs::new("f64",forward::cos_f64,reverse_cos::<{Type::F32}>));
        map.insert(("cosh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::cosh_f64,reverse_cosh::<{Type::F32}>));
        map.insert(("sinh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::sinh_f64,reverse_sinh::<{Type::F32}>));
        map.insert(("tan","f64",&[]).into(),ProcedureOutputs::new("f64",forward::tan_f64,reverse_tan::<{Type::F32}>));
        // map.insert(("tanh","f64",&[]).into(),ProcedureOutputs::new("f64",forward::tanh_f64,reverse_tanh::<{Type::F32}>));
        // Misc
        map.insert(("abs","f64",&[]).into(),ProcedureOutputs::new("f64",forward::abs_f64,reverse_abs::<{Type::F32}>));
        map.insert(("ceil","f64",&[]).into(),ProcedureOutputs::new("f64",forward::ceil_f64,reverse_ceil::<{Type::F32}>));
        map.insert(("floor","f64",&[]).into(),ProcedureOutputs::new("f64",forward::floor_f64,reverse_floor::<{Type::F32}>));
        map.insert(("fract","f64",&[]).into(),ProcedureOutputs::new("f64",forward::fract_f64,reverse_fract::<{Type::F32}>));
        map.insert(("recip","f64",&[]).into(),ProcedureOutputs::new("f64",forward::recip_f64,reverse_recip::<{Type::F32}>));
        map.insert(("round","f64",&[]).into(),ProcedureOutputs::new("f64",forward::round_f64,reverse_round::<{Type::F32}>));

        // Return
        // ------------------------------------------------------------------------------------
        map
    };
    /// Internal map of currently supported operations.
    pub static ref SUPPORTED_OPERATIONS: OperationMap = {
        let mut map = OperationMap::new();
        // f32 arithmetics
        map.insert(("f32","+","f32").into(),ProcedureOutputs::new("f32",forward::add_f32,reverse_add::<{Type::F32}>));
        map.insert(("f32","*","f32").into(),ProcedureOutputs::new("f32",forward::mul_f32,reverse_mul::<{Type::F32}>));
        map.insert(("f32","/","f32").into(),ProcedureOutputs::new("f32",forward::div_f32,reverse_div::<{Type::F32}>));
        map.insert(("f32","-","f32").into(),ProcedureOutputs::new("f32",forward::sub_f32,reverse_sub::<{Type::F32}>));
        // f64 arithmetics
        map.insert(("f64","+","f64").into(),ProcedureOutputs::new("f64",forward::add_f64,reverse_add::<{Type::F32}>));
        map.insert(("f64","*","f64").into(),ProcedureOutputs::new("f64",forward::mul_f64,reverse_mul::<{Type::F32}>));
        map.insert(("f64","/","f64").into(),ProcedureOutputs::new("f64",forward::div_f64,reverse_div::<{Type::F32}>));
        map.insert(("f64","-","f64").into(),ProcedureOutputs::new("f64",forward::sub_f64,reverse_sub::<{Type::F32}>));
        // i8 arithmetics
        map.insert(("i8","+","i8").into(),ProcedureOutputs::new("i8",forward::add_i8,reverse_add::<{Type::F32}>));
        map.insert(("i8","*","i8").into(),ProcedureOutputs::new("i8",forward::mul_i8,reverse_mul::<{Type::F32}>));
        map.insert(("i8","/","i8").into(),ProcedureOutputs::new("i8",forward::div_i8,reverse_div::<{Type::F32}>));
        map.insert(("i8","-","i8").into(),ProcedureOutputs::new("i8",forward::sub_i8,reverse_sub::<{Type::F32}>));
        // i16 arithmetics
        map.insert(("i16","+","i16").into(),ProcedureOutputs::new("i16",forward::add_i16,reverse_add::<{Type::F32}>));
        map.insert(("i16","*","i16").into(),ProcedureOutputs::new("i16",forward::mul_i16,reverse_mul::<{Type::F32}>));
        map.insert(("i16","/","i16").into(),ProcedureOutputs::new("i16",forward::div_i16,reverse_div::<{Type::F32}>));
        map.insert(("i16","-","i16").into(),ProcedureOutputs::new("i16",forward::sub_i16,reverse_sub::<{Type::F32}>));
        // i32 arithmetics
        map.insert(("i32","+","i32").into(),ProcedureOutputs::new("i32",forward::add_i32,reverse_add::<{Type::F32}>));
        map.insert(("i32","*","i32").into(),ProcedureOutputs::new("i32",forward::mul_i32,reverse_mul::<{Type::F32}>));
        map.insert(("i32","/","i32").into(),ProcedureOutputs::new("i32",forward::div_i32,reverse_div::<{Type::F32}>));
        map.insert(("i32","-","i32").into(),ProcedureOutputs::new("i32",forward::sub_i32,reverse_sub::<{Type::F32}>));
        // i64 arithmetics
        map.insert(("i64","+","i64").into(),ProcedureOutputs::new("i64",forward::add_i64,reverse_add::<{Type::F32}>));
        map.insert(("i64","*","i64").into(),ProcedureOutputs::new("i64",forward::mul_i64,reverse_mul::<{Type::F32}>));
        map.insert(("i64","/","i64").into(),ProcedureOutputs::new("i64",forward::div_i64,reverse_div::<{Type::F32}>));
        map.insert(("i64","-","i64").into(),ProcedureOutputs::new("i64",forward::sub_i64,reverse_sub::<{Type::F32}>));
        // i128 arithmetics
        map.insert(("i128","+","i128").into(),ProcedureOutputs::new("i128",forward::add_i128,reverse_add::<{Type::F32}>));
        map.insert(("i128","*","i128").into(),ProcedureOutputs::new("i128",forward::mul_i128,reverse_mul::<{Type::F32}>));
        map.insert(("i128","/","i128").into(),ProcedureOutputs::new("i128",forward::div_i128,reverse_div::<{Type::F32}>));
        map.insert(("i128","-","i128").into(),ProcedureOutputs::new("i128",forward::sub_i128,reverse_sub::<{Type::F32}>));
        // u8 arithmetics
        map.insert(("u8","+","u8").into(),ProcedureOutputs::new("u8",forward::add_u8,reverse_add::<{Type::F32}>));
        map.insert(("u8","*","u8").into(),ProcedureOutputs::new("u8",forward::mul_u8,reverse_mul::<{Type::F32}>));
        map.insert(("u8","/","u8").into(),ProcedureOutputs::new("u8",forward::div_u8,reverse_div::<{Type::F32}>));
        map.insert(("u8","-","u8").into(),ProcedureOutputs::new("u8",forward::sub_u8,reverse_sub::<{Type::F32}>));
        // u16 arithmetics
        map.insert(("u16","+","u16").into(),ProcedureOutputs::new("u16",forward::add_u16,reverse_add::<{Type::F32}>));
        map.insert(("u16","*","u16").into(),ProcedureOutputs::new("u16",forward::mul_u16,reverse_mul::<{Type::F32}>));
        map.insert(("u16","/","u16").into(),ProcedureOutputs::new("u16",forward::div_u16,reverse_div::<{Type::F32}>));
        map.insert(("u16","-","u16").into(),ProcedureOutputs::new("u16",forward::sub_u16,reverse_sub::<{Type::F32}>));
        // u32 arithmetics
        map.insert(("u32","+","u32").into(),ProcedureOutputs::new("u32",forward::add_u32,reverse_add::<{Type::F32}>));
        map.insert(("u32","*","u32").into(),ProcedureOutputs::new("u32",forward::mul_u32,reverse_mul::<{Type::F32}>));
        map.insert(("u32","/","u32").into(),ProcedureOutputs::new("u32",forward::div_u32,reverse_div::<{Type::F32}>));
        map.insert(("u32","-","u32").into(),ProcedureOutputs::new("u32",forward::sub_u32,reverse_sub::<{Type::F32}>));
        // u64 arithmetics
        map.insert(("u64","+","u64").into(),ProcedureOutputs::new("u64",forward::add_u64,reverse_add::<{Type::F32}>));
        map.insert(("u64","*","u64").into(),ProcedureOutputs::new("u64",forward::mul_u64,reverse_mul::<{Type::F32}>));
        map.insert(("u64","/","u64").into(),ProcedureOutputs::new("u64",forward::div_u64,reverse_div::<{Type::F32}>));
        map.insert(("u64","-","u64").into(),ProcedureOutputs::new("u64",forward::sub_u64,reverse_sub::<{Type::F32}>));
        // u128 arithmetics
        map.insert(("u128","+","u128").into(),ProcedureOutputs::new("u128",forward::add_u128,reverse_add::<{Type::F32}>));
        map.insert(("u128","*","u128").into(),ProcedureOutputs::new("u128",forward::mul_u128,reverse_mul::<{Type::F32}>));
        map.insert(("u128","/","u128").into(),ProcedureOutputs::new("u128",forward::div_u128,reverse_div::<{Type::F32}>));
        map.insert(("u128","-","u128").into(),ProcedureOutputs::new("u128",forward::sub_u128,reverse_sub::<{Type::F32}>));
        map
    };
}
