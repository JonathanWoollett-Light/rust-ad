#![allow(incomplete_features)]
#![feature(iter_intersperse)]
#![feature(adt_const_params)]
#![feature(proc_macro_diagnostic)]

//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**
//!
//! Internal non-proc-macro functionality.

extern crate proc_macro;
use proc_macro::Diagnostic;
use std::collections::HashMap;
use syn::spanned::Spanned;

/// Functions to compute derivatives (specific function support).
///
/// A function name may look like `__f_internal_powi_f32`:
/// 1. `__f` represents a forward auto-diff function
/// 2. `_internal_` is just the internal identifier.
/// 3. `powi_` is the function being supported.
/// 4. `f32` is the general type (while this doesn't technically enforce anything it will typically be the respective `Self`).
pub mod derivatives;
pub use derivatives::*;

mod dict;
pub use dict::*;

/// Some utility functions used for [syn].
pub mod traits;
use traits::*;

/// Prefix used for flattening binary expressions in function arguments.
pub const FUNCTION_PREFIX: &'static str = "f";
/// Prefix used for flattening binary expressions as a receiver for a method.
pub const RECEIVER_PREFIX: &'static str = "r";
/// Prefix used for flattening return statements;
pub const RETURN_SUFFIX: &'static str = "rtn";

/// Insert key into map with initial value element or append to existing value
pub fn append_insert(key: &str, value: String, map: &mut HashMap<String, Vec<String>>) {
    if let Some(entry) = map.get_mut(key) {
        entry.push(value);
    } else {
        map.insert(String::from(key), vec![value]);
    }
}
/// Gets type of a given expression as a string
///
/// e.g. for `let a = b + c`, given `b` and `c` are in `type_map` (lets say they are `f32` and `f64`) then we look for the operation `f32+f64` in supported operations, then we know the output type and we return this type.
pub fn expr_type(
    expr: &syn::Expr,
    type_map: &HashMap<String, String>,
) -> Result<String, PassError> {
    match expr {
        syn::Expr::Path(path_expr) => {
            let var = path_expr.path.segments[0].ident.to_string();
            match type_map.get(&var) {
                Some(ident) => Ok(ident.clone()),
                None => {
                    Diagnostic::spanned(
                        path_expr.span().unwrap(),
                        proc_macro::Level::Error,
                        format!("variable not found in type map ({:?})", type_map),
                    )
                    .emit();
                    return Err(String::from("expr_type"));
                }
            }
        }
        syn::Expr::Lit(lit_expr) => literal_type(lit_expr),
        syn::Expr::Call(call_expr) => {
            let function_sig = pass!(function_signature(call_expr, type_map), "expr_type");
            let func_out_type = match SUPPORTED_FUNCTIONS.get(&function_sig) {
                Some(out_sig) => out_sig,
                None => {
                    let error = format!("expr_type: unsupported function: {}", function_sig);
                    Diagnostic::spanned(call_expr.span().unwrap(), proc_macro::Level::Error, error)
                        .emit();
                    return Err(String::from("expr_type"));
                }
            };
            // Sets result type
            Ok(func_out_type.output_type.clone())
        }
        syn::Expr::MethodCall(method_expr) => {
            let method_sig = pass!(method_signature(method_expr, type_map), "expr_type");
            // Searches for supported function signature by function identifier and argument types.
            let method_out_type = match SUPPORTED_METHODS.get(&method_sig) {
                Some(out_sig) => out_sig,
                None => {
                    let error = format!("unsupported method: {}", method_sig);
                    Diagnostic::spanned(
                        method_expr.span().unwrap(),
                        proc_macro::Level::Error,
                        error,
                    )
                    .emit();
                    return Err(String::from("expr_type"));
                }
            };
            // Sets result type
            Ok(method_out_type.output_type.clone())
        }
        syn::Expr::Binary(bin_expr) => {
            let operation_sig = match operation_signature(bin_expr, type_map) {
                Ok(types) => types,
                Err(e) => return Err(e),
            };
            // I think this is cleaner than embedding a `format!` within an `.expect`
            let out_sig = match SUPPORTED_OPERATIONS.get(&operation_sig) {
                Some(out_sig) => out_sig,
                None => {
                    Diagnostic::spanned(
                        bin_expr.span().unwrap(),
                        proc_macro::Level::Error,
                        format!("expr_type: unsupported binary operation: {}", operation_sig),
                    )
                    .emit();
                    return Err(String::from("expr_type"));
                }
            };
            Ok(out_sig.output_type.clone())
        }
        _ => {
            Diagnostic::spanned(
                expr.span().unwrap(),
                proc_macro::Level::Error,
                "expr_type: unsupported expression type",
            )
            .emit();
            return Err(String::from("expr_type"));
        }
    }
}

/// Gets type of literal (only supported numerical types)
pub fn literal_type(expr_lit: &syn::ExprLit) -> Result<String, PassError> {
    match &expr_lit.lit {
        syn::Lit::Float(float_lit) => {
            // Float literal is either f32 or f64
            let float_str = float_lit.to_string();

            let n = float_str.len();
            if !(n > 3) {
                Diagnostic::spanned(
                    expr_lit.span().unwrap(),
                    proc_macro::Level::Error,
                    "All literals need a type suffix e.g. `10.2f32` -- Bad float literal (len)",
                )
                .emit();
                return Err(String::from("literal_type"));
            }
            let float_type_str = &float_str[n - 3..n];
            if !(float_type_str == "f32" || float_type_str == "f64") {
                Diagnostic::spanned(
                    expr_lit.span().unwrap(),
                    proc_macro::Level::Error,
                    "All literals need a type suffix e.g. `10.2f32` -- Bad float literal (type)",
                )
                .emit();
                return Err(String::from("literal_type"));
            }
            Ok(String::from(float_type_str))
        }
        syn::Lit::Int(int_lit) => {
            // Integer literal could be any of the numbers, `4f32`, `16u32` etc.
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
                None => {
                    Diagnostic::spanned(
                        expr_lit.span().unwrap(),
                        proc_macro::Level::Error,
                        "All literals need a type suffix e.g. `10.2f32` -- Bad integer literal",
                    )
                    .emit();
                    return Err(String::from("literal_type"));
                }
            }
        }
        _ => {
            Diagnostic::spanned(
                expr_lit.span().unwrap(),
                proc_macro::Level::Error,
                "Unsupported literal (only integer and float literals are supported)",
            )
            .emit();
            return Err(String::from("literal_type"));
        }
    }
}

/// Given an index (e.g. `1`) appends `REVERSE_JOINED_DERIVATIVE` (e.g. `der_a`).
#[macro_export]
macro_rules! rtn {
    ($a:expr) => {{
        format!("{}{}", rust_ad_consts::REVERSE_RETURN_DERIVATIVE, $a)
    }};
}
/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
#[macro_export]
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", rust_ad_consts::DERIVATIVE_PREFIX, $a)
    }};
}
/// With-Respect-To Nth
///
/// wrt!(a,b,1) = δa/δb_1
#[macro_export]
macro_rules! wrtn {
    ($a:expr, $b:expr, $c: expr) => {{
        format!("{}_wrt_{}_{}", $a, $b, $c)
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
// TODO Is there not a nice inbuilt way to do this?
#[macro_export]
macro_rules! pass {
    ($result: expr,$prefix:expr) => {
        match $result {
            Ok(res) => res,
            Err(err) => {
                return Err(format!("{}->{}", $prefix, err));
            }
        }
    };
}
/// Used so its easier to change return error type.
pub type PassError = String;

/// Gets method signature for internal use
pub fn method_signature(
    method_expr: &syn::ExprMethodCall,
    type_map: &HashMap<String, String>,
) -> Result<MethodSignature, PassError> {
    // Gets method identifier
    let method_str = method_expr.method.to_string();
    // Gets receiver type
    let receiver_type_str = pass!(
        expr_type(&*method_expr.receiver, type_map),
        "method_signature"
    );
    // Gets argument types
    let arg_types_res = method_expr
        .args
        .iter()
        .map(|p| expr_type(p, type_map))
        .collect::<Result<Vec<_>, _>>();
    let arg_types = pass!(arg_types_res, "method_signature");
    Ok(MethodSignature::new(
        method_str,
        receiver_type_str,
        arg_types,
    ))
}
/// Gets function signature for internal use
pub fn function_signature(
    function_expr: &syn::ExprCall,
    type_map: &HashMap<String, String>,
) -> Result<FunctionSignature, PassError> {
    // Gets argument types

    let arg_types_res = function_expr
        .args
        .iter()
        .map(|arg| expr_type(arg, type_map))
        .collect::<Result<Vec<_>, _>>();
    let arg_types = pass!(arg_types_res, "function_signature");
    // Gets function identifier1
    let func_ident_str = function_expr
        .func
        .path()
        .expect("function_signature: func not path")
        .path
        .segments[0]
        .ident
        .to_string();
    // Create function signature
    Ok(FunctionSignature::new(func_ident_str, arg_types))
}

/// Gets operation signature for internal use
pub fn operation_signature(
    operation_expr: &syn::ExprBinary,
    type_map: &HashMap<String, String>,
) -> Result<OperationSignature, PassError> {
    // Gets types of lhs and rhs of expression
    let (left, right) = (
        expr_type(&*operation_expr.left, type_map),
        expr_type(&*operation_expr.right, type_map),
    );
    if left.is_err() {
        Diagnostic::spanned(
            operation_expr.left.span().unwrap(),
            proc_macro::Level::Error,
            "operation_signature: unsupported left type",
        )
        .emit();
    }
    if right.is_err() {
        Diagnostic::spanned(
            operation_expr.right.span().unwrap(),
            proc_macro::Level::Error,
            "operation_signature: unsupported right type",
        )
        .emit();
    }
    match (left, right) {
        (Ok(l), Ok(r)) => Ok(OperationSignature::from((l, operation_expr.op, r))),
        _ => Err(String::from("operation_signature")),
    }
}
