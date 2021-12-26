use rust_ad_core::*;
use rust_ad_core::traits::*;
use std::collections::HashMap;

pub fn update_forward_return(s: Option<&mut syn::Stmt>, function_inputs: &[String]) {
    *s.unwrap() = match s {
        Some(syn::Stmt::Semi(syn::Expr::Return(expr_return), _)) => {
            let b = expr_return.expr.as_ref().expect("update_forward_return: No return expression");
            let expr = &**b;
            let expr_path = expr.path().expect("update_forward_return: No return path");

            let ident = &expr_path.path.segments[0].ident;
            let return_str = format!(
                "return ({},{});",
                ident,
                function_inputs
                    .iter()
                    .map(|input| wrt!(ident, input))
                    .intersperse(String::from(","))
                    .collect::<String>()
            );
            syn::parse_str(&return_str).expect("update_forward_return: parse fail")
        }
        _ => panic!("update_forward_return: No retun statement:\n{:#?}", s),
    }
}

/// Insperses values with respect to the preceding values.
pub fn interspese_succedding_stmts<K>(
    x: Vec<syn::Stmt>,
    extra: K,
    f: fn(&syn::Stmt, &K) -> Option<syn::Stmt>,
) -> Vec<syn::Stmt> {
    let len = x.len();
    let new_len = len * 2 - 1;
    let mut y = Vec::with_capacity(new_len);
    let mut x_iter = x.into_iter().rev();
    if let Some(last) = x_iter.next() {
        y.push(last);
    }
    for a in x_iter {
        if let Some(b) = f(&a, &extra) {
            for c in crate::unwrap_statement(&b).into_iter() {
                y.push(c);
            }
        }
        y.push(a);
    }
    y.into_iter().rev().collect()
}

// TODO Reduce code duplication between `reverse_derivative` and `forward_derivative`
pub fn forward_derivative(
    stmt: &syn::Stmt,
    (type_map, function_inputs): &(&HashMap<String, String>, &[String]),
) -> Option<syn::Stmt> {
    if let syn::Stmt::Local(local) = stmt {
        if let Some(init) = &local.init {
            if let syn::Expr::Binary(bin_expr) = &*init.1 {
                // Creates operation signature struct
                let operation_sig = operation_signature(bin_expr, type_map);
                // Looks up operation with the given lhs type and rhs type and BinOp.
                let operation_out_signature = SUPPORTED_OPERATIONS
                    .get(&operation_sig)
                    .expect("forward_derivative: unsupported operation");
                // Applies the forward deriative function for the found operation.
                let new_stmt = operation_out_signature.forward_derivative.expect(
                    "forward_derivative: binary expression unimplemented forward deriative",
                )(&stmt, function_inputs);
                return Some(new_stmt);
            } else if let syn::Expr::Call(call_expr) = &*init.1 {
                // Create function in signature
                let function_in_signature = function_signature(call_expr, type_map);
                // Gets function out signature
                let function_out_signature = SUPPORTED_FUNCTIONS
                    .get(&function_in_signature)
                    .expect("forward_derivative: unsupported function");
                // Gets new stmt
                let new_stmt = function_out_signature
                    .forward_derivative
                    .expect("forward_derivative: binary unimplemented forward")(
                    &stmt,
                    function_inputs,
                );

                return Some(new_stmt);
            } else if let syn::Expr::MethodCall(method_expr) = &*init.1 {
                let method_sig = method_signature(method_expr, type_map);
                let method_out = SUPPORTED_METHODS
                    .get(&method_sig)
                    .expect("forward_derivative: unsupported method");
                let new_stmt = method_out
                    .forward_derivative
                    .expect("forward_derivative: method unimplemented forward")(
                    &stmt,
                    function_inputs,
                );
                return Some(new_stmt);
            }
        }
    }
    None
}
