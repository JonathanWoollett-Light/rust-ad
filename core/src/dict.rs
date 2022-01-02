use crate::derivatives::*;
use rust_ad_core_macros::{f, r};
use std::{collections::HashMap, fmt};

/// Signature information to refer to specific method.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct MethodSignature {
    name: String,
    receiver_type: String,
    input_types: Vec<String>,
}
impl MethodSignature {
    pub fn new(name: String, receiver_type: String, input_types: Vec<String>) -> Self {
        Self {
            name,
            receiver_type,
            input_types,
        }
    }
}
impl<const N: usize> From<(&'static str, &'static str, &'static [&'static str; N])>
    for MethodSignature
{
    fn from(
        (name, receiver_type, input_types): (
            &'static str,
            &'static str,
            &'static [&'static str; N],
        ),
    ) -> Self {
        Self {
            name: String::from(name),
            receiver_type: String::from(receiver_type),
            input_types: input_types.iter().map(|s| String::from(*s)).collect(),
        }
    }
}
impl fmt::Display for MethodSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}({})",
            self.receiver_type,
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
/// Signature information to refer to specific function.
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

/// Information to relating to specific procedure, output type, etc. (including functions for transforming statements into derivatives).
pub struct ProcedureOutputs {
    /// Output type of procedure
    pub output_type: String,
    /// Transformation procedure to give the forward derivative
    pub forward_derivative: FgdType,
    /// Transformation procedure to give the reverse derivative
    pub reverse_derivative: RgdType,
}
impl ProcedureOutputs {
    pub fn new(
        output_type: &'static str,
        forward_derivative: FgdType,
        reverse_derivative: RgdType,
    ) -> Self {
        Self {
            output_type: String::from(output_type),
            forward_derivative,
            reverse_derivative,
        }
    }
}
// TODO Why doesn't this work?
impl From<(&'static str, FgdType, RgdType)> for ProcedureOutputs {
    fn from(
        (output_type, forward_derivative, reverse_derivative): (&'static str, FgdType, RgdType),
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

/// Signature information to refer to specific binary operation.
#[derive(Hash, PartialEq, Eq, Debug)]
pub struct OperationSignature {
    /// Left-hand-side type
    lhs: String,
    /// Operation type
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
        map.insert(("powi","f32",&["i32"]).into(),ProcedureOutputs::new("f32",f!(powi_f32),r!(powi_f32)));
        map.insert(("powf","f32",&["f32"]).into(),ProcedureOutputs::new("f32",f!(powf_f32),r!(powf_f32)));
        map.insert(("sqrt","f32",&[]).into(),ProcedureOutputs::new("f32",f!(sqrt_f32),r!(sqrt_f32)));
        map.insert(("cbrt","f32",&[]).into(),ProcedureOutputs::new("f32",f!(cbrt_f32),r!(cbrt_f32)));
        map.insert(("exp","f32",&[]).into(),ProcedureOutputs::new("f32",f!(exp_f32),r!(exp_f32)));
        map.insert(("exp2","f32",&[]).into(),ProcedureOutputs::new("f32",f!(exp2_f32),r!(exp2_f32)));
        map.insert(("exp_m1","f32",&[]).into(),ProcedureOutputs::new("f32",f!(exp_m1_f32),r!(exp_m1_f32)));
        // Logs
        map.insert(("ln","f32",&[]).into(),ProcedureOutputs::new("f32",f!(ln_f32),r!(ln_f32)));
        map.insert(("ln_1p","f32",&[]).into(),ProcedureOutputs::new("f32",f!(ln_1p_f32),r!(ln_1p_f32)));
        map.insert(("log","f32",&["f32"]).into(),ProcedureOutputs::new("f32",f!(log_f32),r!(log_f32)));
        map.insert(("log10","f32",&[]).into(),ProcedureOutputs::new("f32",f!(log10_f32),r!(log10_f32)));
        map.insert(("log2","f32",&[]).into(),ProcedureOutputs::new("f32",f!(log2_f32),r!(log2_f32)));
        // Trig
        map.insert(("acos","f32",&[]).into(),ProcedureOutputs::new("f32",f!(acos_f32),r!(acos_f32)));
        map.insert(("acosh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(acosh_f32),r!(acosh_f32)));
        map.insert(("asin","f32",&[]).into(),ProcedureOutputs::new("f32",f!(asin_f32),r!(asin_f32)));
        map.insert(("asinh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(asinh_f32),r!(asinh_f32)));
        map.insert(("atan","f32",&[]).into(),ProcedureOutputs::new("f32",f!(atan_f32),r!(atan_f32)));
        map.insert(("sin","f32",&[]).into(),ProcedureOutputs::new("f32",f!(sin_f32),r!(sin_f32)));
        map.insert(("atanh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(atanh_f32),r!(atanh_f32)));
        map.insert(("cos","f32",&[]).into(),ProcedureOutputs::new("f32",f!(cos_f32),r!(cos_f32)));
        map.insert(("cosh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(cosh_f32),r!(cosh_f32)));
        map.insert(("sinh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(sinh_f32),r!(sinh_f32)));
        map.insert(("tan","f32",&[]).into(),ProcedureOutputs::new("f32",f!(tan_f32),r!(tan_f32)));
        // map.insert(("tanh","f32",&[]).into(),ProcedureOutputs::new("f32",f!(tanh_f32),r!(tanh::<{Type::F32}>)));
        // Misc
        map.insert(("abs","f32",&[]).into(),ProcedureOutputs::new("f32",f!(abs_f32),r!(abs_f32)));
        map.insert(("ceil","f32",&[]).into(),ProcedureOutputs::new("f32",f!(ceil_f32),r!(ceil_f32)));
        map.insert(("floor","f32",&[]).into(),ProcedureOutputs::new("f32",f!(floor_f32),r!(floor_f32)));
        map.insert(("fract","f32",&[]).into(),ProcedureOutputs::new("f32",f!(fract_f32),r!(fract_f32)));
        map.insert(("recip","f32",&[]).into(),ProcedureOutputs::new("f32",f!(recip_f32),r!(recip_f32)));
        map.insert(("round","f32",&[]).into(),ProcedureOutputs::new("f32",f!(round_f32),r!(round_f32)));

        // f64
        // ----------------------------
        // Exponents
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",f!(powi_f64),r!(powi_f64)));
        map.insert(("powf","f64",&["f64"]).into(),ProcedureOutputs::new("f64",f!(powf_f64),r!(powf_f64)));
        map.insert(("sqrt","f64",&[]).into(),ProcedureOutputs::new("f64",f!(sqrt_f64),r!(sqrt_f64)));
        map.insert(("cbrt","f64",&[]).into(),ProcedureOutputs::new("f64",f!(cbrt_f64),r!(cbrt_f64)));
        map.insert(("exp","f64",&[]).into(),ProcedureOutputs::new("f64",f!(exp_f64),r!(exp_f64)));
        map.insert(("exp2","f64",&[]).into(),ProcedureOutputs::new("f64",f!(exp2_f64),r!(exp2_f64)));
        map.insert(("exp_m1","f64",&[]).into(),ProcedureOutputs::new("f64",f!(exp_m1_f64),r!(exp_m1_f64)));
        // Logs
        map.insert(("ln","f64",&[]).into(),ProcedureOutputs::new("f64",f!(ln_f64),r!(ln_f64)));
        map.insert(("ln_1p","f64",&[]).into(),ProcedureOutputs::new("f64",f!(ln_1p_f64),r!(ln_1p_f64)));
        map.insert(("log","f64",&["f64"]).into(),ProcedureOutputs::new("f64",f!(log_f64),r!(log_f64)));
        map.insert(("log10","f64",&[]).into(),ProcedureOutputs::new("f64",f!(log10_f64),r!(log10_f64)));
        map.insert(("log2","f64",&[]).into(),ProcedureOutputs::new("f64",f!(log2_f64),r!(log2_f64)));
        // Trig
        map.insert(("acos","f64",&[]).into(),ProcedureOutputs::new("f64",f!(acos_f64),r!(acos_f64)));
        map.insert(("acosh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(acosh_f64),r!(acosh_f64)));
        map.insert(("asin","f64",&[]).into(),ProcedureOutputs::new("f64",f!(asin_f64),r!(asin_f64)));
        map.insert(("asinh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(asinh_f64),r!(asinh_f64)));
        map.insert(("atan","f64",&[]).into(),ProcedureOutputs::new("f64",f!(atan_f64),r!(atan_f64)));
        map.insert(("sin","f64",&[]).into(),ProcedureOutputs::new("f64",f!(sin_f64),r!(sin_f64)));
        map.insert(("atanh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(atanh_f64),r!(atanh_f64)));
        map.insert(("cos","f64",&[]).into(),ProcedureOutputs::new("f64",f!(cos_f64),r!(cos_f64)));
        map.insert(("cosh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(cosh_f64),r!(cosh_f64)));
        map.insert(("sinh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(sinh_f64),r!(sinh_f64)));
        map.insert(("tan","f64",&[]).into(),ProcedureOutputs::new("f64",f!(tan_f64),r!(tan_f64)));
        // map.insert(("tanh","f64",&[]).into(),ProcedureOutputs::new("f64",f!(tanh_f64),r!(tanh::<{Type::F32}>)));
        // Misc
        map.insert(("abs","f64",&[]).into(),ProcedureOutputs::new("f64",f!(abs_f64),r!(abs_f64)));
        map.insert(("ceil","f64",&[]).into(),ProcedureOutputs::new("f64",f!(ceil_f64),r!(ceil_f64)));
        map.insert(("floor","f64",&[]).into(),ProcedureOutputs::new("f64",f!(floor_f64),r!(floor_f64)));
        map.insert(("fract","f64",&[]).into(),ProcedureOutputs::new("f64",f!(fract_f64),r!(fract_f64)));
        map.insert(("recip","f64",&[]).into(),ProcedureOutputs::new("f64",f!(recip_f64),r!(recip_f64)));
        map.insert(("round","f64",&[]).into(),ProcedureOutputs::new("f64",f!(round_f64),r!(round_f64)));

        // Return
        // ------------------------------------------------------------------------------------
        map
    };
    /// Internal map of currently supported operations.
    pub static ref SUPPORTED_OPERATIONS: OperationMap = {
        let mut map = OperationMap::new();
        // Primitives
        // ------------------------------------------------------------------------------------
        // f32 arithmetics
        map.insert(("f32","+","f32").into(),ProcedureOutputs::new("f32",f!(add_f32),r!(add_f32)));
        map.insert(("f32","*","f32").into(),ProcedureOutputs::new("f32",f!(mul_f32),r!(mul_f32)));
        map.insert(("f32","/","f32").into(),ProcedureOutputs::new("f32",f!(div_f32),r!(div_f32)));
        map.insert(("f32","-","f32").into(),ProcedureOutputs::new("f32",f!(sub_f32),r!(sub_f32)));
        // f64 arithmetics
        map.insert(("f64","+","f64").into(),ProcedureOutputs::new("f64",f!(add_f64),r!(add_f64)));
        map.insert(("f64","*","f64").into(),ProcedureOutputs::new("f64",f!(mul_f64),r!(mul_f64)));
        map.insert(("f64","/","f64").into(),ProcedureOutputs::new("f64",f!(div_f64),r!(div_f64)));
        map.insert(("f64","-","f64").into(),ProcedureOutputs::new("f64",f!(sub_f64),r!(sub_f64)));
        // i8 arithmetics
        map.insert(("i8","+","i8").into(),ProcedureOutputs::new("i8",f!(add_i8),r!(add_i8)));
        map.insert(("i8","*","i8").into(),ProcedureOutputs::new("i8",f!(mul_i8),r!(mul_i8)));
        map.insert(("i8","/","i8").into(),ProcedureOutputs::new("i8",f!(div_i8),r!(div_i8)));
        map.insert(("i8","-","i8").into(),ProcedureOutputs::new("i8",f!(sub_i8),r!(sub_i8)));
        // i16 arithmetics
        map.insert(("i16","+","i16").into(),ProcedureOutputs::new("i16",f!(add_i16),r!(add_i16)));
        map.insert(("i16","*","i16").into(),ProcedureOutputs::new("i16",f!(mul_i16),r!(mul_i16)));
        map.insert(("i16","/","i16").into(),ProcedureOutputs::new("i16",f!(div_i16),r!(div_i16)));
        map.insert(("i16","-","i16").into(),ProcedureOutputs::new("i16",f!(sub_i16),r!(sub_i16)));
        // i32 arithmetics
        map.insert(("i32","+","i32").into(),ProcedureOutputs::new("i32",f!(add_i32),r!(add_i32)));
        map.insert(("i32","*","i32").into(),ProcedureOutputs::new("i32",f!(mul_i32),r!(mul_i32)));
        map.insert(("i32","/","i32").into(),ProcedureOutputs::new("i32",f!(div_i32),r!(div_i32)));
        map.insert(("i32","-","i32").into(),ProcedureOutputs::new("i32",f!(sub_i32),r!(sub_i32)));
        // i64 arithmetics
        map.insert(("i64","+","i64").into(),ProcedureOutputs::new("i64",f!(add_i64),r!(add_i64)));
        map.insert(("i64","*","i64").into(),ProcedureOutputs::new("i64",f!(mul_i64),r!(mul_i64)));
        map.insert(("i64","/","i64").into(),ProcedureOutputs::new("i64",f!(div_i64),r!(div_i64)));
        map.insert(("i64","-","i64").into(),ProcedureOutputs::new("i64",f!(sub_i64),r!(sub_i64)));
        // i128 arithmetics
        map.insert(("i128","+","i128").into(),ProcedureOutputs::new("i128",f!(add_i128),r!(add_i128)));
        map.insert(("i128","*","i128").into(),ProcedureOutputs::new("i128",f!(mul_i128),r!(mul_i128)));
        map.insert(("i128","/","i128").into(),ProcedureOutputs::new("i128",f!(div_i128),r!(div_i128)));
        map.insert(("i128","-","i128").into(),ProcedureOutputs::new("i128",f!(sub_i128),r!(sub_i128)));
        // u8 arithmetics
        map.insert(("u8","+","u8").into(),ProcedureOutputs::new("u8",f!(add_u8),r!(add_u8)));
        map.insert(("u8","*","u8").into(),ProcedureOutputs::new("u8",f!(mul_u8),r!(mul_u8)));
        map.insert(("u8","/","u8").into(),ProcedureOutputs::new("u8",f!(div_u8),r!(div_u8)));
        map.insert(("u8","-","u8").into(),ProcedureOutputs::new("u8",f!(sub_u8),r!(sub_u8)));
        // u16 arithmetics
        map.insert(("u16","+","u16").into(),ProcedureOutputs::new("u16",f!(add_u16),r!(add_u16)));
        map.insert(("u16","*","u16").into(),ProcedureOutputs::new("u16",f!(mul_u16),r!(mul_u16)));
        map.insert(("u16","/","u16").into(),ProcedureOutputs::new("u16",f!(div_u16),r!(div_u16)));
        map.insert(("u16","-","u16").into(),ProcedureOutputs::new("u16",f!(sub_u16),r!(sub_u16)));
        // u32 arithmetics
        map.insert(("u32","+","u32").into(),ProcedureOutputs::new("u32",f!(add_u32),r!(add_u32)));
        map.insert(("u32","*","u32").into(),ProcedureOutputs::new("u32",f!(mul_u32),r!(mul_u32)));
        map.insert(("u32","/","u32").into(),ProcedureOutputs::new("u32",f!(div_u32),r!(div_u32)));
        map.insert(("u32","-","u32").into(),ProcedureOutputs::new("u32",f!(sub_u32),r!(sub_u32)));
        // u64 arithmetics
        map.insert(("u64","+","u64").into(),ProcedureOutputs::new("u64",f!(add_u64),r!(add_u64)));
        map.insert(("u64","*","u64").into(),ProcedureOutputs::new("u64",f!(mul_u64),r!(mul_u64)));
        map.insert(("u64","/","u64").into(),ProcedureOutputs::new("u64",f!(div_u64),r!(div_u64)));
        map.insert(("u64","-","u64").into(),ProcedureOutputs::new("u64",f!(sub_u64),r!(sub_u64)));
        // u128 arithmetics
        map.insert(("u128","+","u128").into(),ProcedureOutputs::new("u128",f!(add_u128),r!(add_u128)));
        map.insert(("u128","*","u128").into(),ProcedureOutputs::new("u128",f!(mul_u128),r!(mul_u128)));
        map.insert(("u128","/","u128").into(),ProcedureOutputs::new("u128",f!(div_u128),r!(div_u128)));
        map.insert(("u128","-","u128").into(),ProcedureOutputs::new("u128",f!(sub_u128),r!(sub_u128)));
        // Return
        // ------------------------------------------------------------------------------------
        map
    };
}
