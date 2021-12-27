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
    pub forward_derivative: Option<fn(&syn::Stmt, &[String]) -> syn::Stmt>,
    /// Transfomation procedure to give the reverse derivative
    pub reverse_derivative: Option<fn(&syn::Stmt) -> syn::Stmt>,
}
impl From<&'static str> for ProcedureOutputs {
    fn from(output_type: &'static str) -> Self {
        Self {
            output_type: String::from(output_type),
            forward_derivative: None,
            reverse_derivative: None,
        }
    }
}
impl ProcedureOutputs {
    pub fn new(
        output_type: &'static str,
        forward_derivative: Option<fn(&syn::Stmt, &[String]) -> syn::Stmt>,
        reverse_derivative: Option<fn(&syn::Stmt) -> syn::Stmt>,
    ) -> Self {
        Self {
            output_type: String::from(output_type),
            forward_derivative,
            reverse_derivative,
        }
    }
}
// TODO Why doesn't this work? (e.g. `("f32",Some(forward_add),None).into()` causes error)
impl
    From<(
        &'static str,
        Option<fn(&syn::Stmt, &[String]) -> syn::Stmt>,
        Option<fn(&syn::Stmt) -> syn::Stmt>,
    )> for ProcedureOutputs
{
    fn from(
        (output_type, forward_derivative, reverse_derivative): (
            &'static str,
            Option<fn(&syn::Stmt, &[String]) -> syn::Stmt>,
            Option<fn(&syn::Stmt) -> syn::Stmt>,
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
        let mut map = FunctionMap::new();
        map.insert(("some_function",&["f64","f64"]).into(),"f32".into());
        map
    };
    /// Internal map of currently supported methods.
    pub static ref SUPPORTED_METHODS: MethodMap = {
        let mut map = MethodMap::new();
        map.insert(("powi","f32",&["i32"]).into(),ProcedureOutputs::new("f32",Some(forward_powi::<{Type::F32}>),Some(reverse_powi_f32)));
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",Some(forward_powi::<{Type::F64}>),Some(reverse_powi_f64)));
        map
    };
    /// Internal map of currently supported operations.
    pub static ref SUPPORTED_OPERATIONS: OperationMap = {
        let mut map = OperationMap::new();
        // f32 arithmetics
        map.insert(("f32","+","f32").into(),ProcedureOutputs::new("f32",Some(forward_add::<{Type::F32}>),Some(reverse_add)));
        map.insert(("f32","*","f32").into(),ProcedureOutputs::new("f32",Some(forward_mul::<{Type::F32}>),Some(reverse_mul)));
        map.insert(("f32","/","f32").into(),ProcedureOutputs::new("f32",Some(forward_div::<{Type::F32}>),Some(reverse_div)));
        map.insert(("f32","-","f32").into(),ProcedureOutputs::new("f32",Some(forward_sub::<{Type::F32}>),Some(reverse_sub)));
        // f64 arithmetics
        map.insert(("f64","+","f64").into(),ProcedureOutputs::new("f64",Some(forward_add::<{Type::F64}>),Some(reverse_add)));
        map.insert(("f64","*","f64").into(),ProcedureOutputs::new("f64",Some(forward_mul::<{Type::F64}>),Some(reverse_mul)));
        map.insert(("f64","/","f64").into(),ProcedureOutputs::new("f64",Some(forward_div::<{Type::F64}>),Some(reverse_div)));
        map.insert(("f64","-","f64").into(),ProcedureOutputs::new("f64",Some(forward_sub::<{Type::F64}>),Some(reverse_sub)));
        // i8 arithmetics
        map.insert(("i8","+","i8").into(),ProcedureOutputs::new("i8",Some(forward_add::<{Type::I8}>),Some(reverse_add)));
        map.insert(("i8","*","i8").into(),ProcedureOutputs::new("i8",Some(forward_mul::<{Type::I8}>),Some(reverse_mul)));
        map.insert(("i8","/","i8").into(),ProcedureOutputs::new("i8",Some(forward_div::<{Type::I8}>),Some(reverse_div)));
        map.insert(("i8","-","i8").into(),ProcedureOutputs::new("i8",Some(forward_sub::<{Type::I8}>),Some(reverse_sub)));
        // i16 arithmetics
        map.insert(("i16","+","i16").into(),ProcedureOutputs::new("i16",Some(forward_add::<{Type::I16}>),Some(reverse_add)));
        map.insert(("i16","*","i16").into(),ProcedureOutputs::new("i16",Some(forward_mul::<{Type::I16}>),Some(reverse_mul)));
        map.insert(("i16","/","i16").into(),ProcedureOutputs::new("i16",Some(forward_div::<{Type::I16}>),Some(reverse_div)));
        map.insert(("i16","-","i16").into(),ProcedureOutputs::new("i16",Some(forward_sub::<{Type::I16}>),Some(reverse_sub)));
        // i32 arithmetics
        map.insert(("i32","+","i32").into(),ProcedureOutputs::new("i32",Some(forward_add::<{Type::I32}>),Some(reverse_add)));
        map.insert(("i32","*","i32").into(),ProcedureOutputs::new("i32",Some(forward_mul::<{Type::I32}>),Some(reverse_mul)));
        map.insert(("i32","/","i32").into(),ProcedureOutputs::new("i32",Some(forward_div::<{Type::I32}>),Some(reverse_div)));
        map.insert(("i32","-","i32").into(),ProcedureOutputs::new("i32",Some(forward_sub::<{Type::I32}>),Some(reverse_sub)));
        // i64 arithmetics
        map.insert(("i64","+","i64").into(),ProcedureOutputs::new("i64",Some(forward_add::<{Type::I64}>),Some(reverse_add)));
        map.insert(("i64","*","i64").into(),ProcedureOutputs::new("i64",Some(forward_mul::<{Type::I64}>),Some(reverse_mul)));
        map.insert(("i64","/","i64").into(),ProcedureOutputs::new("i64",Some(forward_div::<{Type::I64}>),Some(reverse_div)));
        map.insert(("i64","-","i64").into(),ProcedureOutputs::new("i64",Some(forward_sub::<{Type::I64}>),Some(reverse_sub)));
        // i128 arithmetics
        map.insert(("i128","+","i128").into(),ProcedureOutputs::new("i128",Some(forward_add::<{Type::I128}>),Some(reverse_add)));
        map.insert(("i128","*","i128").into(),ProcedureOutputs::new("i128",Some(forward_mul::<{Type::I128}>),Some(reverse_mul)));
        map.insert(("i128","/","i128").into(),ProcedureOutputs::new("i128",Some(forward_div::<{Type::I128}>),Some(reverse_div)));
        map.insert(("i128","-","i128").into(),ProcedureOutputs::new("i128",Some(forward_sub::<{Type::I128}>),Some(reverse_sub)));
        // u8 arithmetics
        map.insert(("u8","+","u8").into(),ProcedureOutputs::new("u8",Some(forward_add::<{Type::U8}>),Some(reverse_add)));
        map.insert(("u8","*","u8").into(),ProcedureOutputs::new("u8",Some(forward_mul::<{Type::U8}>),Some(reverse_mul)));
        map.insert(("u8","/","u8").into(),ProcedureOutputs::new("u8",Some(forward_div::<{Type::U8}>),Some(reverse_div)));
        map.insert(("u8","-","u8").into(),ProcedureOutputs::new("u8",Some(forward_sub::<{Type::U8}>),Some(reverse_sub)));
        // u16 arithmetics
        map.insert(("u16","+","u16").into(),ProcedureOutputs::new("u16",Some(forward_add::<{Type::U16}>),Some(reverse_add)));
        map.insert(("u16","*","u16").into(),ProcedureOutputs::new("u16",Some(forward_mul::<{Type::U16}>),Some(reverse_mul)));
        map.insert(("u16","/","u16").into(),ProcedureOutputs::new("u16",Some(forward_div::<{Type::U16}>),Some(reverse_div)));
        map.insert(("u16","-","u16").into(),ProcedureOutputs::new("u16",Some(forward_sub::<{Type::U16}>),Some(reverse_sub)));
        // u32 arithmetics
        map.insert(("u32","+","u32").into(),ProcedureOutputs::new("u32",Some(forward_add::<{Type::U32}>),Some(reverse_add)));
        map.insert(("u32","*","u32").into(),ProcedureOutputs::new("u32",Some(forward_mul::<{Type::U32}>),Some(reverse_mul)));
        map.insert(("u32","/","u32").into(),ProcedureOutputs::new("u32",Some(forward_div::<{Type::U32}>),Some(reverse_div)));
        map.insert(("u32","-","u32").into(),ProcedureOutputs::new("u32",Some(forward_sub::<{Type::U32}>),Some(reverse_sub)));
        // u64 arithmetics
        map.insert(("u64","+","u64").into(),ProcedureOutputs::new("u64",Some(forward_add::<{Type::U64}>),Some(reverse_add)));
        map.insert(("u64","*","u64").into(),ProcedureOutputs::new("u64",Some(forward_mul::<{Type::U64}>),Some(reverse_mul)));
        map.insert(("u64","/","u64").into(),ProcedureOutputs::new("u64",Some(forward_div::<{Type::U64}>),Some(reverse_div)));
        map.insert(("u64","-","u64").into(),ProcedureOutputs::new("u64",Some(forward_sub::<{Type::U64}>),Some(reverse_sub)));
        // u128 arithmetics
        map.insert(("u128","+","u128").into(),ProcedureOutputs::new("u128",Some(forward_add::<{Type::U128}>),Some(reverse_add)));
        map.insert(("u128","*","u128").into(),ProcedureOutputs::new("u128",Some(forward_mul::<{Type::U128}>),Some(reverse_mul)));
        map.insert(("u128","/","u128").into(),ProcedureOutputs::new("u128",Some(forward_div::<{Type::U128}>),Some(reverse_div)));
        map.insert(("u128","-","u128").into(),ProcedureOutputs::new("u128",Some(forward_sub::<{Type::U128}>),Some(reverse_sub)));
        map
    };
}
