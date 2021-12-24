//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use std::collections::HashMap;
use std::convert::TryFrom;

/// Some utility functions used for [syn].
pub mod utils;
use utils::*;

/// Prefix used for the derivatives of a variable (e.g. The derivative of `x` would be `der_x`).
pub const DERIVATIVE_PREFIX: &'static str = "__der_";
/// Prefix used to for the forward differentiation function.
pub const FORWARD_MODE_PREFIX: &'static str = "__for_";
/// Prefix used to for the reverse differentiation function.
pub const REVERSE_MODE_PREFIX: &'static str = "__rev_";
/// Prefix used for flattening binary expressions in function arguments.
pub const FUNCTION_PREFFIX: &'static str = "f";
/// Prefix used for flattening binary expressions as a reciever for a method.
pub const RECEIVER_PREFIX: &'static str = "r";

/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
#[macro_export]
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", DERIVATIVE_PREFIX, $a)
    }};
}

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
    pub forward_derivative: Option<fn(&syn::Stmt) -> syn::Stmt>,
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
        forward_derivative: Option<fn(&syn::Stmt) -> syn::Stmt>,
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
        Option<fn(&syn::Stmt) -> syn::Stmt>,
        Option<fn(&syn::Stmt) -> syn::Stmt>,
    )> for ProcedureOutputs
{
    fn from(
        (output_type, forward_derivative, reverse_derivative): (
            &'static str,
            Option<fn(&syn::Stmt) -> syn::Stmt>,
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
        map.insert(("powi","f32",&["i32"]).into(),ProcedureOutputs::new("f32",Some(forward_powi_f32),Some(reverse_powi_f32)));
        map.insert(("powi","f64",&["i32"]).into(),ProcedureOutputs::new("f64",Some(forward_powi_f64),Some(reverse_powi_f64)));
        map
    };
    /// Internal map of currently supported operations.
    pub static ref SUPPORTED_OPERATIONS: OperationMap = {
        let mut map = OperationMap::new();
        // f32 arithmetics
        map.insert(("f32","+","f32").into(),ProcedureOutputs::new("f32",Some(forward_add),Some(reverse_add)));
        map.insert(("f32","*","f32").into(),ProcedureOutputs::new("f32",Some(forward_mul),Some(reverse_mul)));
        map.insert(("f32","/","f32").into(),ProcedureOutputs::new("f32",Some(forward_div),Some(reverse_div)));
        map.insert(("f32","-","f32").into(),ProcedureOutputs::new("f32",Some(forward_sub),Some(reverse_sub)));
        // f64 arithmetics
        map.insert(("f64","+","f64").into(),ProcedureOutputs::new("f64",Some(forward_add),Some(reverse_add)));
        map.insert(("f64","*","f64").into(),ProcedureOutputs::new("f64",Some(forward_mul),Some(reverse_mul)));
        map.insert(("f64","/","f64").into(),ProcedureOutputs::new("f64",Some(forward_div),Some(reverse_div)));
        map.insert(("f64","-","f64").into(),ProcedureOutputs::new("f64",Some(forward_sub),Some(reverse_sub)));
        // i8 arithmetics
        map.insert(("i8","+","i8").into(),ProcedureOutputs::new("i8",Some(forward_add),Some(reverse_add)));
        map.insert(("i8","*","i8").into(),ProcedureOutputs::new("i8",Some(forward_mul),Some(reverse_mul)));
        map.insert(("i8","/","i8").into(),ProcedureOutputs::new("i8",Some(forward_div),Some(reverse_div)));
        map.insert(("i8","-","i8").into(),ProcedureOutputs::new("i8",Some(forward_sub),Some(reverse_sub)));
        // i16 arithmetics
        map.insert(("i16","+","i16").into(),ProcedureOutputs::new("i16",Some(forward_add),Some(reverse_add)));
        map.insert(("i16","*","i16").into(),ProcedureOutputs::new("i16",Some(forward_mul),Some(reverse_mul)));
        map.insert(("i16","/","i16").into(),ProcedureOutputs::new("i16",Some(forward_div),Some(reverse_div)));
        map.insert(("i16","-","i16").into(),ProcedureOutputs::new("i16",Some(forward_sub),Some(reverse_sub)));
        // i32 arithmetics
        map.insert(("i32","+","i32").into(),ProcedureOutputs::new("i32",Some(forward_add),Some(reverse_add)));
        map.insert(("i32","*","i32").into(),ProcedureOutputs::new("i32",Some(forward_mul),Some(reverse_mul)));
        map.insert(("i32","/","i32").into(),ProcedureOutputs::new("i32",Some(forward_div),Some(reverse_div)));
        map.insert(("i32","-","i32").into(),ProcedureOutputs::new("i32",Some(forward_sub),Some(reverse_sub)));
        // i64 arithmetics
        map.insert(("i64","+","i64").into(),ProcedureOutputs::new("i64",Some(forward_add),Some(reverse_add)));
        map.insert(("i64","*","i64").into(),ProcedureOutputs::new("i64",Some(forward_mul),Some(reverse_mul)));
        map.insert(("i64","/","i64").into(),ProcedureOutputs::new("i64",Some(forward_div),Some(reverse_div)));
        map.insert(("i64","-","i64").into(),ProcedureOutputs::new("i64",Some(forward_sub),Some(reverse_sub)));
        // i128 arithmetics
        map.insert(("i128","+","i128").into(),ProcedureOutputs::new("i128",Some(forward_add),Some(reverse_add)));
        map.insert(("i128","*","i128").into(),ProcedureOutputs::new("i128",Some(forward_mul),Some(reverse_mul)));
        map.insert(("i128","/","i128").into(),ProcedureOutputs::new("i128",Some(forward_div),Some(reverse_div)));
        map.insert(("i128","-","i128").into(),ProcedureOutputs::new("i128",Some(forward_sub),Some(reverse_sub)));
        // u8 arithmetics
        map.insert(("u8","+","u8").into(),ProcedureOutputs::new("u8",Some(forward_add),Some(reverse_add)));
        map.insert(("u8","*","u8").into(),ProcedureOutputs::new("u8",Some(forward_mul),Some(reverse_mul)));
        map.insert(("u8","/","u8").into(),ProcedureOutputs::new("u8",Some(forward_div),Some(reverse_div)));
        map.insert(("u8","-","u8").into(),ProcedureOutputs::new("u8",Some(forward_sub),Some(reverse_sub)));
        // u16 arithmetics
        map.insert(("u16","+","u16").into(),ProcedureOutputs::new("u16",Some(forward_add),Some(reverse_add)));
        map.insert(("u16","*","u16").into(),ProcedureOutputs::new("u16",Some(forward_mul),Some(reverse_mul)));
        map.insert(("u16","/","u16").into(),ProcedureOutputs::new("u16",Some(forward_div),Some(reverse_div)));
        map.insert(("u16","-","u16").into(),ProcedureOutputs::new("u16",Some(forward_sub),Some(reverse_sub)));
        // u32 arithmetics
        map.insert(("u32","+","u32").into(),ProcedureOutputs::new("u32",Some(forward_add),Some(reverse_add)));
        map.insert(("u32","*","u32").into(),ProcedureOutputs::new("u32",Some(forward_mul),Some(reverse_mul)));
        map.insert(("u32","/","u32").into(),ProcedureOutputs::new("u32",Some(forward_div),Some(reverse_div)));
        map.insert(("u32","-","u32").into(),ProcedureOutputs::new("u32",Some(forward_sub),Some(reverse_sub)));
        // u64 arithmetics
        map.insert(("u64","+","u64").into(),ProcedureOutputs::new("u64",Some(forward_add),Some(reverse_add)));
        map.insert(("u64","*","u64").into(),ProcedureOutputs::new("u64",Some(forward_mul),Some(reverse_mul)));
        map.insert(("u64","/","u64").into(),ProcedureOutputs::new("u64",Some(forward_div),Some(reverse_div)));
        map.insert(("u64","-","u64").into(),ProcedureOutputs::new("u64",Some(forward_sub),Some(reverse_sub)));
        // u128 arithmetics
        map.insert(("u128","+","u128").into(),ProcedureOutputs::new("u128",Some(forward_add),Some(reverse_add)));
        map.insert(("u128","*","u128").into(),ProcedureOutputs::new("u128",Some(forward_mul),Some(reverse_mul)));
        map.insert(("u128","/","u128").into(),ProcedureOutputs::new("u128",Some(forward_div),Some(reverse_div)));
        map.insert(("u128","-","u128").into(),ProcedureOutputs::new("u128",Some(forward_sub),Some(reverse_sub)));
        map
    };
}

/// Gets type of given expression (only supports literals and paths)
pub fn expr_type(expr: &syn::Expr, type_map: &HashMap<String, String>) -> String {
    match expr {
        syn::Expr::Path(path_expr) => type_map
            .get(&path_expr.path.segments[0].ident.to_string())
            .expect("forward_derivative: unfound variable")
            .clone(),
        syn::Expr::Lit(lit_expr) => literal_type(lit_expr),
        _ => panic!("forward_derivative: unsupported binary left expr type"),
    }
}

/// Literal prefix error
const LPR: &'static str = "All literals need a type suffix e.g. `10.2f32` -- ";
/// Gets type of literal (only supproted numerical types)
pub fn literal_type(expr_lit: &syn::ExprLit) -> String {
    match &expr_lit.lit {
        syn::Lit::Float(float_lit) => {
            // Float literal is either f32 or f64
            let float_str = float_lit.to_string();

            let n = float_str.len();
            assert!(n > 3, "{}Bad float literal (len)", LPR);
            let float_type_str = &float_str[n - 3..n];
            assert!(
                float_type_str == "f32" || float_type_str == "f64",
                "{}Bad float literal",
                LPR
            );
            String::from(float_type_str)
        }
        syn::Lit::Int(int_lit) => {
            // Integer literall could be any of the numbers, `4f32`, `16u32` etc.
            let int_str = int_lit.to_string();
            let n = int_str.len();

            // Checking if `i128` or `u128` (the 4 character length type annotations)
            let large_type = if n > 4 {
                let large_int_str = &int_str[n - 4..n];
                match large_int_str {
                    "i128" | "u128" => Some(String::from(large_int_str)),
                    _ => None,
                }
            } else {
                None
            };
            // Checking if `f32` or `u16` etc. (the 3 character length type annotations)
            let standard_type = if n > 3 {
                let standard_int_str = &int_str[n - 3..n];
                match standard_int_str {
                    "u16" | "u32" | "u64" | "i16" | "i32" | "i64" | "f32" | "f64" => {
                        Some(String::from(standard_int_str))
                    }
                    _ => None,
                }
            } else {
                None
            };
            // Checking `u8` or `i8` (2 character length type annotations)
            let short_type = if n > 2 {
                let short_int_str = &int_str[n - 2..n];
                match short_int_str {
                    "i8" | "u8" => Some(String::from(short_int_str)),
                    _ => None,
                }
            } else {
                None
            };

            let int_lit_str = match large_type.or(standard_type).or(short_type) {
                Some(int_lit_some) => int_lit_some,
                None => panic!("{}Bad integer literal", LPR),
            };
            int_lit_str
        }
        _ => panic!("Unsupported literal (only integer and float literals are supported)"),
    }
}
/// Gets given expression as string (only supports literals and paths)
fn expr_str(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(lit_expr) => match &lit_expr.lit {
            syn::Lit::Int(int_lit) => int_lit.to_string(),
            syn::Lit::Float(float_lit) => float_lit.to_string(),
            _ => panic!("expr_str: unsupported lit"),
        },
        syn::Expr::Path(path_expr) => path_expr.path.segments[0].ident.to_string(),
        _ => panic!("expr_str: unsupported expr"),
    }
}
/// Gets given literal expression as string (only supports float and integers)
fn lit_str(lit: &syn::ExprLit) -> String {
    match &lit.lit {
        syn::Lit::Int(int_lit) => int_lit.to_string(),
        syn::Lit::Float(float_lit) => float_lit.to_string(),
        _ => panic!("lit_str: unsupported lit"),
    }
}

/// Gets method signature for internal use
pub fn method_signature(
    method_expr: &syn::ExprMethodCall,
    type_map: &HashMap<String, String>,
) -> MethodSignature {
    // Gets method identifier
    let method_str = method_expr.method.to_string();
    // Gets receiver type
    let receiver_type_str = expr_type(&*method_expr.receiver, type_map);
    // Gets argument types
    let arg_types = method_expr
        .args
        .iter()
        .map(|p| expr_type(p, type_map))
        .collect::<Vec<_>>();
    MethodSignature::new(method_str, receiver_type_str, arg_types)
}
/// Gets function signature for internal use
pub fn function_signature(
    function_expr: &syn::ExprCall,
    type_map: &HashMap<String, String>,
) -> FunctionSignature {
    // Gets argument types
    let arg_types = function_expr
        .args
        .iter()
        .map(|arg| expr_type(arg, type_map))
        .collect::<Vec<_>>();
    // Gets function identifier
    let func_ident_str = function_expr
        .func
        .path()
        .expect("propagate_types: func not path")
        .path
        .segments[0]
        .ident
        .to_string();
    // Create function signature
    FunctionSignature::new(func_ident_str, arg_types)
}
/// Gets opertion signature for internal use
pub fn operation_signature(
    operation_expr: &syn::ExprBinary,
    type_map: &HashMap<String, String>,
) -> OperationSignature {
    // Gets types of lhs and rhs of expression
    let left_type = expr_type(&*operation_expr.left, type_map);
    let right_type = expr_type(&*operation_expr.right, type_map);
    // Creates operation signature struct
    OperationSignature::from((left_type, operation_expr.op, right_type))
}

fn forward_add(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_add: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_add: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_add: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {} + {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).expect("forward_add: parse fail")
}
fn forward_sub(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_sub: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_sub: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_sub: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {} - {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).expect("forward_sub: parse fail")
}
fn forward_mul(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_mul: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_mul: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_mul: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {r}*{dl} + {l}*{dr};",
        d,
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).expect("forward_mul: parse fail")
}
fn forward_div(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_div: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_div: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_div: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {dl}/{r} - {dr}*{r}*{r}/{l};",
        d,
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).expect("forward_div: parse fail")
}
fn expr_string(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
            syn::Lit::Float(lit_float) => lit_float.to_string(),
            syn::Lit::Int(lit_int) => lit_int.to_string(),
            _ => panic!("Uncovered lit in `expr_string`"),
        },
        syn::Expr::Path(expr_path) => expr_path.path.segments[0].ident.to_string(),
        _ => panic!("Uncoverd expr for `derivative_expr`"),
    }
}
/// Derivative expression string
fn derivative_expr_string(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(expr_lit) => format!("0{}",literal_type(expr_lit)),
        syn::Expr::Path(expr_path) => der!(expr_path.path.segments[0].ident.to_string()),
        _ => panic!("Uncovered expr for `derivative_expr`\n{:#?}",expr),
    }
}

fn forward_powi_f32(stmt: &syn::Stmt) -> syn::Stmt {
    forward_powi(stmt, "f32")
}
fn forward_powi_f64(stmt: &syn::Stmt) -> syn::Stmt {
    forward_powi(stmt, "f64")
}
// TODO Can we reduce/Remove code duplication between `forward_powi_f64` and `forward_powi_f32`?
/// Deriative of f32::powi(i32) operation.
/// Given:
/// ```ignore
/// x = a * b
/// dx = a*db+b*da
/// ```
/// We can for x^2, x^3 and x^4, say:
/// ```ignore
/// x2 = a*a
/// dx2 = 2*a*da
/// x3 = x2*a
/// dx3 = x2*da+a*dx2 = (a*a)*da+a*(2*a*da) = da*a^2 + 2*da*a^2 = 3*da*a^2
/// x4 = x3*a
/// dx4 = x3*da+a*dx3 = (x2*a)*da+a*(3*da*a^2) = da*a^3 + 3*da*a^3 = 4*da*a^3
/// ```
/// Therefore:
/// ```ignore
/// xn = a^n
/// dxn = n*dx*a^(n-1)
/// ```
fn forward_powi(stmt: &syn::Stmt, float: &'static str) -> syn::Stmt {
    assert!(float == "f32" || float == "f64");
    let local = stmt.local().expect("forward_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_powi: not method");

    let d = der!(local
        .pat
        .ident()
        .expect("forward_powi: not ident")
        .ident
        .to_string());

    let receiver_ident = method_expr
        .receiver
        .path()
        .expect("forward_powi: not path")
        .path
        .segments[0]
        .ident
        .to_string();
    let exponent = expr_str(&method_expr.args[0]);
    let new_str = format!(
        "let {} = {} as {} * {} * {}.powi({}-1i32);",
        d,
        exponent,
        float,
        der!(receiver_ident),
        receiver_ident,
        exponent
    );
    let new_stmt = syn::parse_str(&new_str).expect("forward_powi: parse fail");
    new_stmt
}

fn reverse_powi_f32(stmt: &syn::Stmt) -> syn::Stmt {
    reverse_powi(stmt, "f32")
}
fn reverse_powi_f64(stmt: &syn::Stmt) -> syn::Stmt {
    reverse_powi(stmt, "f64")
}
// TODO Is this wrong? (I feel like it is)
/// ```ignore
/// a = x^y
/// dx, dy = y*x^(y-1), x^y*ln(x)
/// ```
fn reverse_powi(stmt: &syn::Stmt, float: &'static str) -> syn::Stmt {
    assert!(float == "f32" || float == "f64");
    let local = stmt.local().expect("reverse_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_powi: not method");

    let receiver_ident = method_expr
        .receiver
        .path()
        .expect("reverse_powi: not path")
        .path
        .segments[0]
        .ident
        .to_string();
    let exponent = expr_str(&method_expr.args[0]);

    let new_str = format!(
        "let ({},{}) = ({exponent} as {float}*{base}.powi({exponent}-1i32),{base}.powi({exponent})*{base}.ln());",
        der!(receiver_ident),
        der!(exponent),
        float = float,
        base=receiver_ident,
        exponent=exponent,
    );
    let new_stmt = syn::parse_str(&new_str).expect("reverse_powi: parse fail");
    new_stmt
}

fn reverse_add(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("reverse_add: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_add: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_add: not ident")
        .ident
        .to_string();

    let (a, b): (String, String) = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            der!(expr_path_r.path.segments[0].ident.to_string()),
        ),
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            String::from("_"),
        ),
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => (
            String::from("_"),
            der!(expr_path_r.path.segments[0].ident.to_string()),
        ),
        _ => panic!("reverse_add: Uncovered `syn::BinOp::Add(_)` binary expression combination"),
    };
    let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, der!(lis));
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_add: parse fail");
    new_stmt
}
fn reverse_sub(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("reverse_sub: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_sub: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_sub: not ident")
        .ident
        .to_string();

    let (a, b): (String, String) = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            format!("-{}", der!(expr_path_r.path.segments[0].ident.to_string())),
        ),
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            String::from("_"),
        ),
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => (
            String::from("_"),
            format!("-{}", der!(expr_path_r.path.segments[0].ident.to_string())),
        ),
        _ => panic!("reverse_sub: Uncovered `syn::BinOp::Sub(_)` binary expression combination"),
    };
    let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, lis);
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sub: parse fail");
    new_stmt
}
fn reverse_mul(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("reverse_mul: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_mul: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_mul: not ident")
        .ident
        .to_string();

    let stmt_str = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!(
                "let ({},{}) = ({}*{},{}*{});",
                der!(l),
                der!(r),
                r,
                lis,
                l,
                lis
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r)
            );
            format!("let {} = {}*{};", der!(l), r, lis)
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!("let {} = {}*{};", der!(r), l, lis)
        }
        _ => panic!("reverse_mul: Uncovered `syn::BinOp::Mul(_)` binary expression combination"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_mul: parse fail");
    new_stmt
}
fn reverse_div(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("reverse_div: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_div: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_div: not ident")
        .ident
        .to_string();

    let stmt_str = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!(
                "let ({},{}) = ({}/{},{}*{});",
                der!(l),
                der!(r),
                lis,
                r,
                l,
                lis
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_lit_r
                    .lit
                    .float()
                    .expect("reverse_div: right not literal")
                    .to_string(),
            );
            format!("let {} = {}/{};", der!(l), lis, r)
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_lit_l
                    .lit
                    .float()
                    .expect("reverse_div: left not literal")
                    .to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!("let {} = {}*{};", der!(r), l, lis)
        }
        _ => panic!("Uncovered `syn::BinOp::Mul(_)` binary expression combination"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_div: parse fail");
    new_stmt
}
