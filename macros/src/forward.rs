use proc_macro::Diagnostic;
use quote::ToTokens;
use rust_ad_core::traits::*;
use rust_ad_core::Arg;
use rust_ad_core::*;
use std::collections::HashMap;
#[cfg(not(debug_assertions))]
use std::collections::HashSet;
use syn::spanned::Spanned;

pub fn update_forward_return(
    block: &mut syn::Block,
    function_inputs: &[String],
    #[cfg(not(debug_assertions))] type_map: HashMap<String, String>,
    #[cfg(not(debug_assertions))] non_zero_derivatives: HashSet<String>,
) -> Result<(), PassError> {
    match block.stmts.last_mut() {
        Some(last_stmt) => {
            *last_stmt = match last_stmt {
                syn::Stmt::Semi(syn::Expr::Return(expr_return_opt), _) => {
                    let expr_return = match expr_return_opt.expr.as_ref() {
                        Some(e) => e,
                        None => {
                            Diagnostic::spanned(
                                expr_return_opt.span().unwrap(),
                                proc_macro::Level::Error,
                                "No return expression",
                            )
                            .emit();
                            return Err("update_forward_return: No return expression".into());
                        }
                    };
                    match &**expr_return {
                        syn::Expr::Tuple(expr_tuple) => {
                            let return_idents = expr_tuple.to_token_stream().to_string();
                            eprintln!("return_idents: {}", return_idents);
                            let return_str = format!("return ({},({}));",
                                return_idents,
                                expr_tuple.elems.iter()
                                    .map(|e|
                                        match e {
                                            syn::Expr::Path(ep) => {
                                                let ep_str = ep.to_token_stream().to_string();
                                                format!("({})",function_inputs
                                                    .iter()
                                                    .map(|input| if ep_str == *input {
                                                        der!(input)
                                                    } else {
                                                        let der = wrt!(ep_str, input);
                                                        #[cfg(not(debug_assertions))]
                                                        match non_zero_derivatives.contains(&der) {
                                                            true => der,
                                                            false => format!("0{}", type_map.get(input).unwrap()),
                                                        }
                                                        #[cfg(debug_assertions)]
                                                        der
                                                    })
                                                    .intersperse(String::from(","))
                                                    .collect::<String>()
                                                )
                                            }
                                            _ => panic!("update_forward_return: Unsupported inner tuple type (e.g. `return (x,y)` is supported, `return  (x,(a,b))` is not supported)")
                                    })
                                    .intersperse(String::from(","))
                                    .collect::<String>()
                            );
                            syn::parse_str(&return_str)
                                .expect("update_forward_return: tuple parse fail")
                        }
                        syn::Expr::Path(expr_path) => {
                            let return_ident = expr_path.to_token_stream().to_string();

                            // The if case where `ident == input` is for when you are returning an input.
                            let return_str = format!(
                                "return ({},{});",
                                return_ident,
                                match function_inputs.len() {
                                    0 => String::new(),
                                    1 => {
                                        let input = &function_inputs[0];
                                        if return_ident == *input {
                                            der!(input)
                                        } else {
                                            let der = wrt!(return_ident, input);
                                            #[cfg(not(debug_assertions))]
                                            match non_zero_derivatives.contains(&der) {
                                                true => der,
                                                false => {
                                                    format!("0{}", type_map.get(input).unwrap())
                                                }
                                            }
                                            #[cfg(debug_assertions)]
                                            der
                                        }
                                    }
                                    _ => format!(
                                        "({})",
                                        function_inputs
                                            .iter()
                                            .map(|input| if return_ident == *input {
                                                der!(input)
                                            } else {
                                                let der = wrt!(return_ident, input);
                                                #[cfg(not(debug_assertions))]
                                                match non_zero_derivatives.contains(&der) {
                                                    true => der,
                                                    false => {
                                                        format!("0{}", type_map.get(input).unwrap())
                                                    }
                                                }
                                                #[cfg(debug_assertions)]
                                                der
                                            })
                                            .intersperse(String::from(","))
                                            .collect::<String>()
                                    ),
                                }
                            );
                            syn::parse_str(&return_str)
                                .expect("update_forward_return: path parse fail")
                        }
                        _ => {
                            Diagnostic::spanned(
                                expr_return_opt.span().unwrap(),
                                proc_macro::Level::Error,
                                "Unsupported return expression",
                            )
                            .emit();
                            return Err(
                                "update_forward_return: unsupported return expression".into()
                            );
                        }
                    }
                }
                _ => {
                    Diagnostic::spanned(
                        block.span().unwrap(),
                        proc_macro::Level::Error,
                        "Unsupported return statement",
                    )
                    .emit();
                    return Err("update_forward_return: Unsupported return statement".into());
                }
            };
        }
        _ => {
            Diagnostic::spanned(
                block.span().unwrap(),
                proc_macro::Level::Error,
                "No return statement",
            )
            .emit();
            return Err("update_forward_return: No return statement".into());
        }
    };
    Ok(())
}

/// Intersperses values with respect to the preceding values.
pub fn intersperse_succeeding_stmts<K>(
    mut x: Vec<syn::Stmt>,
    mut extra: K,
    f: fn(&syn::Stmt, &mut K) -> Result<Option<syn::Stmt>, PassError>,
) -> Result<Vec<syn::Stmt>, PassError> {
    let len = x.len();
    let new_len = len * 2 - 1;
    let mut y = Vec::with_capacity(new_len);

    while x.len() > 1 {
        y.push(x.remove(0));
        let after_opt = pass!(
            f(y.last().unwrap(), &mut extra),
            "intersperse_succeeding_stmts"
        );
        if let Some(after) = after_opt {
            y.push(after);
        }
    }
    y.push(x.remove(0));
    Ok(y)
}

// TODO Reduce code duplication between `reverse_derivative` and `forward_derivative`
pub fn forward_derivative(
    stmt: &syn::Stmt,
    #[cfg(not(debug_assertions))] (type_map, function_inputs, non_zero_derivatives): &mut (
        &HashMap<String, String>,
        &[String],
        &mut HashSet<String>,
    ),
    #[cfg(debug_assertions)] (type_map, function_inputs): &mut (
        &HashMap<String, String>,
        &[String],
    ),
) -> Result<Option<syn::Stmt>, PassError> {
    if let syn::Stmt::Local(local) = stmt {
        let local_ident = local
            .pat
            .ident()
            .expect("forward_derivative: not ident")
            .ident
            .to_string();
        if let Some(init) = &local.init {
            // eprintln!("init: {:#?}",init);
            if let syn::Expr::Binary(bin_expr) = &*init.1 {
                // Creates operation signature struct
                let operation_sig = pass!(
                    operation_signature(bin_expr, type_map),
                    "forward_derivative"
                );
                // Looks up operation with the given lhs type and rhs type and BinOp.
                let operation_out_signature = match SUPPORTED_OPERATIONS.get(&operation_sig) {
                    Some(sig) => sig,
                    None => {
                        let error = format!("unsupported derivative for {}", operation_sig);
                        Diagnostic::spanned(
                            bin_expr.span().unwrap(),
                            proc_macro::Level::Error,
                            error,
                        )
                        .emit();
                        return Err(String::from("forward_derivative"));
                    }
                };
                // Applies the forward derivative function for the found operation.
                let new_stmt = (operation_out_signature.forward_derivative)(
                    local_ident,
                    &[
                        Arg::try_from(&*bin_expr.left).expect("forward_derivative: bin left"),
                        Arg::try_from(&*bin_expr.right).expect("forward_derivative: bin right"),
                    ],
                    function_inputs,
                    #[cfg(not(debug_assertions))]
                    non_zero_derivatives,
                );
                return Ok(Some(new_stmt));
            } else if let syn::Expr::Call(call_expr) = &*init.1 {
                // Create function in signature
                let function_in_signature = pass!(
                    function_signature(call_expr, type_map),
                    "forward_derivative"
                );
                // Gets function out signature
                let function_out_signature = match SUPPORTED_FUNCTIONS.get(&function_in_signature) {
                    Some(sig) => sig,
                    None => {
                        let error = format!("unsupported derivative for {}", function_in_signature);
                        Diagnostic::spanned(
                            call_expr.span().unwrap(),
                            proc_macro::Level::Error,
                            error,
                        )
                        .emit();
                        return Err(String::from("forward_derivative"));
                    }
                };
                let args = call_expr
                    .args
                    .iter()
                    .map(|a| Arg::try_from(a).expect("forward_derivative: call arg"))
                    .collect::<Vec<_>>();
                // Gets new stmt
                let new_stmt = (function_out_signature.forward_derivative)(
                    local_ident,
                    args.as_slice(),
                    function_inputs,
                    #[cfg(not(debug_assertions))]
                    non_zero_derivatives,
                );

                return Ok(Some(new_stmt));
            } else if let syn::Expr::MethodCall(method_expr) = &*init.1 {
                let method_sig = pass!(
                    method_signature(method_expr, type_map),
                    "forward_derivative"
                );
                let method_out = match SUPPORTED_METHODS.get(&method_sig) {
                    Some(sig) => sig,
                    None => {
                        let error = format!("unsupported derivative for {}", method_sig);
                        Diagnostic::spanned(
                            method_expr.span().unwrap(),
                            proc_macro::Level::Error,
                            error,
                        )
                        .emit();
                        return Err(String::from("forward_derivative"));
                    }
                };
                let args = {
                    let mut base = Vec::new();
                    let receiver = Arg::try_from(&*method_expr.receiver)
                        .expect("forward_derivative: method receiver");
                    base.push(receiver);
                    let mut args = method_expr
                        .args
                        .iter()
                        .map(|a| Arg::try_from(a).expect("forward_derivative: method arg"))
                        .collect::<Vec<_>>();
                    base.append(&mut args);
                    base
                };

                let new_stmt = (method_out.forward_derivative)(
                    local_ident,
                    args.as_slice(),
                    function_inputs,
                    #[cfg(not(debug_assertions))]
                    non_zero_derivatives,
                );
                return Ok(Some(new_stmt));
            } else if let syn::Expr::Path(expr_path) = &*init.1 {
                // Given `let x = y;`

                // This is `x`
                let out_ident = local
                    .pat
                    .ident()
                    .expect("forward_derivative: not ident")
                    .ident
                    .to_string();
                // This `y`
                let in_ident = expr_path.path.segments[0].ident.to_string();
                // This is type of `y`
                let out_type = type_map
                    .get(&in_ident)
                    .expect("forward_derivative: return not found type");
                let return_type = rust_ad_core::Type::try_from(out_type.as_str())
                    .expect("forward_derivative: unsupported return type");

                let idents = function_inputs
                    .iter()
                    .map(|input| wrt!(out_ident, input))
                    .intersperse(String::from(","))
                    .collect::<String>();
                let derivatives = function_inputs
                    .iter()
                    .map(|input| {
                        cumulative_derivative_wrt_rt(&*init.1, input, function_inputs, &return_type)
                    })
                    .intersperse(String::from(","))
                    .collect::<String>();
                let stmt_str = format!("let ({}) = ({});", idents, derivatives);
                let new_stmt: syn::Stmt =
                    syn::parse_str(&stmt_str).expect("forward_derivative: parse fail");

                return Ok(Some(new_stmt));
            } else if let syn::Expr::Lit(expr_lit) = &*init.1 {
                // Given `let x = y;`

                // This is `x`
                let out_ident = local
                    .pat
                    .ident()
                    .expect("forward_derivative: not ident")
                    .ident
                    .to_string();
                // This is type of `y`
                let out_type = literal_type(expr_lit).expect("forward_derivative: bad lit type");
                let return_type = rust_ad_core::Type::try_from(out_type.as_str())
                    .expect("forward_derivative: unsupported return type");

                let idents = function_inputs
                    .iter()
                    .map(|input| wrt!(out_ident, input))
                    .intersperse(String::from(","))
                    .collect::<String>();
                let derivatives = function_inputs
                    .iter()
                    .map(|input| {
                        cumulative_derivative_wrt_rt(&*init.1, input, function_inputs, &return_type)
                    })
                    .intersperse(String::from(","))
                    .collect::<String>();
                let stmt_str = format!("let ({}) = ({});", idents, derivatives);
                let new_stmt: syn::Stmt =
                    syn::parse_str(&stmt_str).expect("forward_derivative: parse fail");

                return Ok(Some(new_stmt));
            }
        }
    }
    Ok(None)
}
