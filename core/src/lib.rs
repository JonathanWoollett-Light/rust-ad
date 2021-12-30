#![allow(incomplete_features)]
#![feature(iter_intersperse)]
#![feature(adt_const_params)]

//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use std::collections::HashMap;

/// Functions that given statements return the statements which compute the respective derivatives.
pub mod derivatives;
pub use derivatives::*;

mod dict;
pub use dict::*;

/// Some utility functions used for [syn].
pub mod traits;
use traits::*;

mod utils;

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
/// Prefix used for flattening retrn statements;
pub const RETURN_SUFFIX: &'static str = "rtn";

/// Insert key into map with initial value element or append to existing value
pub fn append_insert(key: &str, value: String, map: &mut HashMap<String, Vec<String>>) {
    if let Some(entry) = map.get_mut(key) {
        entry.push(value);
    } else {
        map.insert(String::from(key), vec![value]);
    }
}

/// Gets type of given expression (only supports literals and paths)
pub fn expr_type(expr: &syn::Expr, type_map: &HashMap<String, String>) -> Result<String, String> {
    match expr {
        syn::Expr::Path(path_expr) => {
            let var = path_expr.path.segments[0].ident.to_string();
            match type_map.get(&var) {
                Some(ident) => Ok(ident.clone()),
                None => Err(format!(
                    "expr_type: `{}` not found in type map `{:?}`",
                    var, type_map
                )),
            }
        }
        syn::Expr::Lit(lit_expr) => literal_type(lit_expr),
        _ => panic!("expr_type: unsupported type"),
    }
}

/// Gets type of literal (only supproted numerical types)
pub fn literal_type(expr_lit: &syn::ExprLit) -> Result<String, String> {
    match &expr_lit.lit {
        syn::Lit::Float(float_lit) => {
            // Float literal is either f32 or f64
            let float_str = float_lit.to_string();

            let n = float_str.len();
            if !(n > 3) {
                return Err(
                    "All literals need a type suffix e.g. `10.2f32` -- Bad float literal (len)"
                        .into(),
                );
            }
            let float_type_str = &float_str[n - 3..n];
            if !(float_type_str == "f32" || float_type_str == "f64") {
                return Err(
                    "All literals need a type suffix e.g. `10.2f32` -- Bad float literal (type)"
                        .into(),
                );
            }
            Ok(String::from(float_type_str))
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

            match large_type.or(standard_type).or(short_type) {
                Some(int_lit_some) => Ok(int_lit_some),
                None => Err(
                    "All literals need a type suffix e.g. `10.2f32` -- Bad integer literal".into(),
                ),
            }
        }
        _ => Err("Unsupported literal (only integer and float literals are supported)".into()),
    }
}

/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
#[macro_export]
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", crate::DERIVATIVE_PREFIX, $a)
    }};
}
/// With-Respect-To
///
/// wrt!(a,b) = δa/δb
#[macro_export]
macro_rules! wrt {
    ($a:expr,$b:expr) => {{
        format!("{}_wrt_{}", $a, $b)
    }};
}

/// Gets method signature for internal use
pub fn method_signature(
    method_expr: &syn::ExprMethodCall,
    type_map: &HashMap<String, String>,
) -> MethodSignature {
    // Gets method identifier
    let method_str = method_expr.method.to_string();
    // Gets receiver type
    let receiver_type_str =
        expr_type(&*method_expr.receiver, type_map).expect("method_signature: bad expr");
    // Gets argument types
    let arg_types = method_expr
        .args
        .iter()
        .map(|p| expr_type(p, type_map).expect("method_signature: bad arg type"))
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
        .map(|arg| expr_type(arg, type_map).expect("function_signature: bad arg type"))
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
    let left_type =
        expr_type(&*operation_expr.left, type_map).expect("operation_signature: bad left");
    let right_type =
        expr_type(&*operation_expr.right, type_map).expect("operation_signature: bad right");
    // Creates operation signature struct
    OperationSignature::from((left_type, operation_expr.op, right_type))
}
