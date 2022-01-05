extern crate proc_macro;
use proc_macro::Diagnostic;

use proc_macro::Level::Error;
use quote::ToTokens;
use rust_ad_core::traits::*;
use rust_ad_core::*;
use std::collections::HashMap;
use syn::spanned::Spanned;

pub fn reverse_accumulate_inputs(
    function_inputs: &[String],
    component_map: &HashMap<String, Vec<String>>,
    type_map: &HashMap<String, String>,
) -> syn::Stmt {
    let stmt_str = format!(
        "let ({}) = ({});",
        function_inputs
            .iter()
            .map(|input| der!(input))
            .intersperse(String::from(","))
            .collect::<String>(),
        function_inputs
            .iter()
            .map(|input| {
                if let Some(components) = component_map.get(input) {
                    components
                        .iter()
                        .map(|c| wrt!(input, c))
                        .intersperse(String::from("+"))
                        .collect::<String>()
                } else {
                    format!(
                        "1{}",
                        type_map
                            .get(input)
                            .expect("reverse_accumulate_inputs: no input type")
                    )
                }
            })
            .intersperse(String::from(","))
            .collect::<String>()
    );
    syn::parse_str(&stmt_str).expect("reverse_accumulate_inputs: parse fail")
}
pub fn reverse_accumulate_derivative(
    stmt: &syn::Stmt,
    component_map: &HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    if let syn::Stmt::Local(local) = stmt {
        // eprintln!("local: {:#?}",local);
        // panic!("just stop here");
        let local_ident = local
            .pat
            .ident()
            .expect("reverse_accumulate_derivative: not ident")
            .ident
            .to_string();
        let acc_der_stmt_str = format!(
            "let {} = {};",
            der!(local_ident),
            component_map
                .get(&local_ident)
                .expect("reverse_accumulate_derivative: ident not in map")
                .iter()
                .map(|d| wrt!(local_ident, d))
                .intersperse(String::from("+"))
                .collect::<String>()
        );
        let acc_der_stmt = syn::parse_str(&acc_der_stmt_str)
            .expect("reverse_accumulate_derivative: acc parse fail");
        Some(acc_der_stmt)
    } else {
        None
    }
}
pub fn reverse_derivative(
    stmt: &syn::Stmt,
    type_map: &HashMap<String, String>,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Result<Option<syn::Stmt>, PassError> {
    const NAME: &'static str = "reverse_derivative";
    match stmt {
        // If we have a local variable declaration e.g. `let a;`
        syn::Stmt::Local(local_stmt) => {
            let local_ident = local_stmt.pat.to_token_stream().to_string();
            match local_stmt.init {
                // If there is some intialization e.g. `let a = ... ;`
                Some((_, init)) => {
                    match &*init {
                        // If we have local variable declaration with a binary expression as initialization e.g. `let a = b + c;`.
                        syn::Expr::Binary(bin_init_expr) => {
                            // Create binary operation signature (formed with the lhs type, rhs type and operation symbol (`+`, `-` etc.)).
                            let op_sig = pass!(operation_signature(bin_init_expr, type_map), NAME);
                            // Looks up binary operation of the formed signature in our supported operations map.
                            match SUPPORTED_OPERATIONS.get(&op_sig) {
                                // If we find an entry for an output signature, this means the operation is supported.
                                // Applies the reverse derivative function for the found operation.
                                Some(out_sig) => Ok(Some((out_sig.reverse_derivative)(
                                    local_ident,
                                    &[
                                        pass!(Arg::try_from(&*bin_init_expr.left), NAME),
                                        pass!(Arg::try_from(&*bin_init_expr.right), NAME),
                                    ],
                                    component_map,
                                ))),
                                // If we don't find an entry, this means the operation is not supported.
                                None => {
                                    // Since we do not support this operation and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported operation: {}", op_sig);
                                    Diagnostic::spanned(bin_init_expr.span().unwrap(), Error, err)
                                        .emit();
                                    Err(format!("{}: {}", NAME, err))
                                }
                            }
                        }
                        // If we have local variable declaration with a function call expression as initialization e.g. `let a = f(b,c);`.
                        syn::Expr::Call(call_init_expr) => {
                            // Create function signature (formed with function identifier and argument types)
                            let fn_sig = pass!(function_signature(call_init_expr, type_map), NAME);
                            // Looks up function of our formed function signature in our supported functions map.
                            match SUPPORTED_FUNCTIONS.get(&fn_sig) {
                                // If we find an entry for an output signature, this means the function is supported.
                                Some(out_sig) => {
                                    // Collects arguments
                                    let args = pass!(
                                        call_init_expr
                                            .args
                                            .iter()
                                            .map(|a| {
                                                Diagnostic::spanned(
                                                    a.span().unwrap(),
                                                    Error,
                                                    "Unsupported function argument",
                                                )
                                                .emit();
                                                Arg::try_from(a)
                                            })
                                            .collect::<Result<Vec<_>, _>>(),
                                        NAME
                                    );
                                    // Applies the reverse derivative function for the found function.
                                    let new_stmt = (out_sig.reverse_derivative)(
                                        local_ident,
                                        args.as_slice(),
                                        component_map,
                                    );
                                    Ok(Some(new_stmt))
                                }
                                // If we don't find an entry, this means the function is not supported.
                                None => {
                                    // Since we do not support this function and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported function: {}", fn_sig);
                                    Diagnostic::spanned(call_init_expr.span().unwrap(), Error, err)
                                        .emit();
                                    Err(format!("{}: {}", NAME, err))
                                }
                            }
                        }
                        // If we have local variable declaration with a method call expression as initialization e.g. `let a = b.f(c);`.
                        syn::Expr::MethodCall(method_init_expr) => {
                            // Create function signature (formed with function identifier and argument types)
                            let mt_sig = pass!(method_signature(method_init_expr, type_map), NAME);
                            // Looks up function of our formed function signature in our supported functions map.
                            match SUPPORTED_METHODS.get(&mt_sig) {
                                // If we find an entry for an output signature, this means the function is supported.
                                Some(out_sig) => {
                                    // Collects arguments
                                    let mut args = pass!(
                                        method_init_expr
                                            .args
                                            .iter()
                                            .map(|a| {
                                                Diagnostic::spanned(
                                                    a.span().unwrap(),
                                                    Error,
                                                    "Unsupported method argument",
                                                )
                                                .emit();
                                                Arg::try_from(a)
                                            })
                                            .collect::<Result<Vec<_>, _>>(),
                                        NAME
                                    );
                                    // Inserts receiver argument as first argument (the receiver argument is the respectivel `self` in `let a = b.f(c)` it would be `b`).
                                    let receiver =
                                        pass!(Arg::try_from(&*method_init_expr.receiver), NAME);
                                    args.insert(0, receiver);
                                    // Applies the reverse derivative function for the found function.
                                    let new_stmt = (out_sig.reverse_derivative)(
                                        local_ident,
                                        args.as_slice(),
                                        component_map,
                                    );
                                    Ok(Some(new_stmt))
                                }
                                // If we don't find an entry, this means the method is not supported.
                                None => {
                                    // Since we do not support this method and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported method: {}", mt_sig);
                                    Diagnostic::spanned(
                                        method_init_expr.span().unwrap(),
                                        Error,
                                        err,
                                    )
                                    .emit();
                                    Err(format!("{}: {}", NAME, err))
                                }
                            }
                        }
                        // If we have local variable declaration with an assignment expression as initialization e.g. `let a = b;`.
                        syn::Expr::Path(path_init_expr) => {
                            // Variable being assigned (e.g. `b`).
                            let in_ident = path_init_expr.to_token_stream().to_string();
                            // We insert a derivative b wrt a which notes that b affects (and that we have defined this variable).
                            append_insert(&in_ident, local_ident.clone(), component_map);
                            // TODO This will require adaptation for reverse autodiff with multiple outputs.
                            // In this simple case b is identical to a, so the accumulative derivative is identical to a (this being `der!(local_ident)`).
                            let stmt_str = format!(
                                "let {} = {};",
                                wrt!(in_ident, local_ident),
                                der!(local_ident)
                            );
                            let new_stmt = pass!(syn::parse_str(&stmt_str), NAME);
                            Ok(Some(new_stmt))
                        }
                        _ => Ok(None),
                    }
                }
                None => Ok(None),
            }
        },
        // TODO If return statement we need to set the accumulative derivative of the return component as the input return derivatives.
        syn::Stmt::Semi 
        _ => Ok(None),
    }
}
