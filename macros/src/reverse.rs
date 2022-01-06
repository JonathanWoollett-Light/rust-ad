extern crate proc_macro;
use proc_macro::Diagnostic;

use proc_macro::Level::Error;
use quote::ToTokens;
use rust_ad_core::*;
use std::collections::HashMap;
use syn::spanned::Spanned;

#[cfg(not(debug_assertions))]
use std::collections::HashSet;

// Given return statement outputs return statement with appended derivatives
pub fn reverse_append_derivatives(
    stmt: syn::Stmt,
    function_input_identifiers: &Vec<String>,
) -> Result<syn::Stmt, PassError> {
    const NAME: &'static str = "reverse_append_derivatives";
    if let syn::Stmt::Semi(syn::Expr::Return(return_struct), _) = stmt {
        if let Some(return_expr) = &return_struct.expr {
            match &**return_expr {
                // If return expression is tuple e.g. `return (a,b);`
                syn::Expr::Tuple(return_tuple) => {
                    let return_idents_res = return_tuple.elems.iter().enumerate().map(|(index,e)| match e {
                        syn::Expr::Path(p) => {
                            let path_ident = p.to_token_stream().to_string();
                            let rtn_ident = rtn!(index);

                            let (ident,der) = (wrt!(path_ident,rtn_ident),format!("({})",
                            function_input_identifiers
                                .iter()
                                .map(|input|wrt!(input,rtn_ident))
                                .intersperse(String::from(","))
                                .collect::<String>()
                            ));
                            // eprintln!("tuple_str: {:?}",tuple_str);
                            Ok((ident,der))
                        },
                        syn::Expr::Lit(l) => {
                            let rtn_ident = rtn!(index);
                            #[cfg(debug_assertions)]
                            let der = function_input_identifiers
                                .iter()
                                .map(|input|wrt!(input,rtn_ident))
                                .intersperse(String::from(","))
                                .collect::<String>();
                            #[cfg(not(debug_assertions))]
                            let der = function_input_identifiers
                                .iter()
                                .map(|_|format!("0{}",literal_type(l).expect("reverse_append_derivatives: unsupported literal type")))
                                .intersperse(String::from(","))
                                .collect::<String>();

                            Ok((
                                l.to_token_stream().to_string(),
                                format!("({})",der)
                            ))

                        },
                        _ => {
                            let err = "Unsupported return tuple element. Elements in a returned tuple must be paths or literals (e.g. `return (a,b,2f32))` is supported, `return (a,(b,c))` is not supported).";
                            Diagnostic::spanned(
                                return_struct.span().unwrap(),
                                proc_macro::Level::Error,
                                err,
                            )
                            .emit();
                            Err(err.to_string())
                        }
                    }).collect::<Result<Vec<_>,_>>();
                    let return_idents = pass!(return_idents_res, NAME);
                    let (ident, der) = return_idents.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

                    let new_return_stmt_str = format!(
                        "return (({}),({}));",
                        ident
                            .into_iter()
                            .intersperse(String::from(","))
                            .collect::<String>(),
                        der.into_iter()
                            .intersperse(String::from(","))
                            .collect::<String>(),
                    );
                    // eprintln!("new_return_stmt_str: {}",new_return_stmt_str);
                    let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                    Ok(new_return_stmt)
                }
                // If return expression is path e.g. `return a;`
                syn::Expr::Path(return_path) => {
                    let path_ident = return_path.to_token_stream().to_string();
                    let rtn_ident = rtn!(0);
                    let tuple_str = format!(
                        "({})",
                        function_input_identifiers
                            .iter()
                            .map(|input| wrt!(input, rtn_ident))
                            .intersperse(String::from(","))
                            .collect::<String>()
                    );
                    let new_return_stmt_str = format!("return ({},{});", path_ident, tuple_str);
                    let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                    Ok(new_return_stmt)
                }
                syn::Expr::Lit(l) => {
                    let new_return_stmt_str = format!(
                        "return ({},0{});",
                        l.to_token_stream(),
                        literal_type(l).expect("Unsupported literal type")
                    );
                    let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                    Ok(new_return_stmt)
                }
                _ => {
                    let err = "Unsupported return expression";
                    Diagnostic::spanned(
                        return_struct.span().unwrap(),
                        proc_macro::Level::Error,
                        err,
                    )
                    .emit();
                    Err(format!("{}: {}", NAME, err))
                }
            }
        } else {
            let err = "No return expression";
            Diagnostic::spanned(return_struct.span().unwrap(), proc_macro::Level::Error, err)
                .emit();
            Err(format!("{}: {}", NAME, err))
        }
    } else {
        let err = "Not return statement";
        Diagnostic::spanned(stmt.span().unwrap(), proc_macro::Level::Error, err).emit();
        Err(format!("{}: {}", NAME, err))
    }
}

pub fn reverse_accumulate_inputs(
    function_inputs: &[String],
    component_map: &Vec<HashMap<String, Vec<String>>>,
    type_map: &HashMap<String, String>,
    #[cfg(not(debug_assertions))] non_zero_derivatives: &mut HashSet<String>,
) -> syn::Stmt {
    let (inputs, derivative) = component_map
        .iter()
        .enumerate()
        .map(|(index, map)| {
            let rtn = rtn!(index);
            let (idents, derivatives) = function_inputs
                .iter()
                .filter_map(|input| {
                    let ident = wrt!(input, rtn);

                    map.get(input).map(|e|{
                        (
                            ident,
                            e.iter().map(|s|wrt!(s,rtn))
                            .intersperse(String::from(","))
                            .collect::<String>()
                        )
                    })
                })
                .unzip::<_, _, Vec<_>, Vec<_>>();
            (
                format!(
                    "({})",
                    idents
                        .into_iter()
                        .intersperse(String::from(","))
                        .collect::<String>()
                ),
                format!(
                    "({})",
                    derivatives
                        .into_iter()
                        .intersperse(String::from(","))
                        .collect::<String>()
                ),
            )
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let stmt_str = format!(
        "let ({}) = ({});",
        inputs
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>(),
        derivative
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>()
    );
    syn::parse_str(&stmt_str).expect("reverse_accumulate_inputs: parse fail")
}
pub fn reverse_accumulate_derivative(
    stmt: &syn::Stmt,
    component_map: &Vec<HashMap<String, Vec<String>>>,
    #[cfg(debug_assertions)] type_map: &HashMap<String, String>,
    #[cfg(not(debug_assertions))] non_zero_derivatives: &mut HashSet<String>,
) -> Result<Option<syn::Stmt>, PassError> {
    const NAME: &'static str = "reverse_accumulate_derivative";
    match stmt {
        // If we have a local variable declaration statement e.g. `let a;`.
        syn::Stmt::Local(local) => match &local.pat {
            syn::Pat::Ident(local_ident) => {
                // eprintln!("local: {:#?}",local);
                // panic!("just stop here");
                let ident_str = local_ident.to_token_stream().to_string();
                let (accumulative_derivatives, derivative_sums) = component_map
                    .iter()
                    .enumerate()
                    .filter_map(|(index, map)| {
                        let acc = wrt!(ident_str, rtn!(index));

                        #[cfg(debug_assertions)]
                        let der_res = Some(match map.get(&ident_str) {
                            Some(der) => der
                                .iter()
                                .map(|d| d.clone())
                                .intersperse(String::from("+"))
                                .collect::<String>(),
                            None => format!(
                                "0{}",
                                type_map
                                    .get(&ident_str)
                                    .expect("reverse_accumulate_derivative: not found in type map")
                            ),
                        });
                        #[cfg(not(debug_assertions))]
                        let der_res = map.get(&ident_str).map(|der| {
                            non_zero_derivatives.insert(acc.clone());
                            der.iter()
                                .map(|d| d.clone())
                                .intersperse(String::from("+"))
                                .collect::<String>()
                        });
                        der_res.map(|der| (acc, der))
                    })
                    .unzip::<_, _, Vec<_>, Vec<_>>();
                // equivalent to derivative_sums.is_empty()
                if accumulative_derivatives.is_empty() {
                    let acc_der_stmt_str = format!(
                        "let ({}) = ({});",
                        accumulative_derivatives
                            .into_iter()
                            .intersperse(String::from(","))
                            .collect::<String>(),
                        derivative_sums
                            .into_iter()
                            .intersperse(String::from(","))
                            .collect::<String>()
                    );
                    match syn::parse_str(&acc_der_stmt_str) {
                        Ok(r) => Ok(Some(r)),
                        Err(e) => Err(format!(
                            "reverse_accumulate_derivative: parse error on `{}`: {}",
                            acc_der_stmt_str, e
                        )),
                    }
                } else {
                    Ok(None)
                }
            }
            _ => {
                let err = "Unsupported local declaration type. Only path declarations are supported (e.g. `let a = ... ;`)";
                Diagnostic::spanned(local.span().unwrap(), proc_macro::Level::Error, err).emit();
                Err(format!("{}: {}", NAME, err))
            }
        },
        _ => Ok(None),
    }
}
pub fn reverse_derivative(
    stmt: &syn::Stmt,
    type_map: &HashMap<String, String>,
    component_map: &mut Vec<HashMap<String, Vec<String>>>,
    function_input_identifiers: &Vec<String>,
    #[cfg(not(debug_assertions))] non_zero_derivatives: &mut HashSet<String>,
) -> Result<Option<syn::Stmt>, PassError> {
    const NAME: &'static str = "reverse_derivative";
    match stmt {
        // If we have a local variable declaration e.g. `let a;`
        syn::Stmt::Local(local_stmt) => {
            let local_ident = local_stmt.pat.to_token_stream().to_string();
            match &local_stmt.init {
                // If there is some initialization e.g. `let a = ... ;`
                Some((_, init)) => {
                    match &**init {
                        // If we have local variable declaration with a binary expression as initialization e.g. `let a = b + c;`.
                        syn::Expr::Binary(bin_init_expr) => {
                            // Create binary operation signature (formed with the lhs type, rhs type and operation symbol (`+`, `-` etc.)).
                            let op_sig = pass!(operation_signature(bin_init_expr, type_map), NAME);
                            // Looks up binary operation of the formed signature in our supported operations map.
                            match SUPPORTED_OPERATIONS.get(&op_sig) {
                                // If we find an entry for an output signature, this means the operation is supported.
                                // Applies the reverse derivative function for the found operation.
                                Some(out_sig) => Ok((out_sig.reverse_derivative)(
                                    local_ident,
                                    &[
                                        pass!(Arg::try_from(&*bin_init_expr.left), NAME),
                                        pass!(Arg::try_from(&*bin_init_expr.right), NAME),
                                    ],
                                    component_map,
                                    #[cfg(not(debug_assertions))]
                                    non_zero_derivatives,
                                )),
                                // If we don't find an entry, this means the operation is not supported.
                                None => {
                                    // Since we do not support this operation and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported operation: {}", op_sig);
                                    Diagnostic::spanned(
                                        bin_init_expr.span().unwrap(),
                                        Error,
                                        err.clone(),
                                    )
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
                                        #[cfg(not(debug_assertions))]
                                        non_zero_derivatives,
                                    );
                                    Ok(new_stmt)
                                }
                                // If we don't find an entry, this means the function is not supported.
                                None => {
                                    // Since we do not support this function and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported function: {}", fn_sig);
                                    Diagnostic::spanned(
                                        call_init_expr.span().unwrap(),
                                        Error,
                                        err.clone(),
                                    )
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
                                    // Inserts receiver argument as first argument (the receiver argument is the respective `self` in `let a = b.f(c)` it would be `b`).
                                    let receiver =
                                        pass!(Arg::try_from(&*method_init_expr.receiver), NAME);
                                    args.insert(0, receiver);
                                    // Applies the reverse derivative function for the found function.
                                    let new_stmt = (out_sig.reverse_derivative)(
                                        local_ident,
                                        args.as_slice(),
                                        component_map,
                                        #[cfg(not(debug_assertions))]
                                        non_zero_derivatives,
                                    );
                                    Ok(new_stmt)
                                }
                                // If we don't find an entry, this means the method is not supported.
                                None => {
                                    // Since we do not support this method and without considering it the whole process will not be accurate, we throw an error.
                                    let err = format!("Unsupported method: {}", mt_sig);
                                    Diagnostic::spanned(
                                        method_init_expr.span().unwrap(),
                                        Error,
                                        err.clone(),
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
                            eprintln!("in_ident: {}",in_ident);

                            let (ident_str, der_str) = component_map
                                .iter_mut()
                                .enumerate()
                                .filter_map(|(index, map)| {
                                    let rtn = rtn!(index);
                                    let from_wrt = wrt!(local_ident, rtn);
                                    let to_wrt = wrt!(in_ident, rtn);

                                    
                                    // If component exists
                                    match map.get(&local_ident).map(|e| e.contains(&rtn)) {
                                        Some(true) => {
                                            append_insert(&in_ident, from_wrt.clone(), map);
                                            Some((to_wrt, from_wrt))
                                        }
                                        _ => None,
                                    }

                                    // #[cfg(debug_assertions)]
                                    // {
                                    //     // We insert `a` derivative `b wrt a` which notes that b affects (and that we have defined this variable).
                                    //     append_insert(&in_ident, wrt.clone(), map);
                                    //     // In this simple case `b` is identical to `a`, so the accumulative derivative is identical to `a`.
                                    //     Some((wrtn, wrt))
                                    // }
                                    // #[cfg(not(debug_assertions))]
                                    // non_zero_derivatives.contains(&wrt).then(|| {
                                    //     non_zero_derivatives.insert(wrtn.clone());
                                    //     // We insert `a` derivative `b wrt a` which notes that b affects (and that we have defined this variable).
                                    //     append_insert(&in_ident, wrt.clone(), map);
                                    //     // In this simple case `b` is identical to `a`, so the accumulative derivative is identical to `a`.
                                    //     (wrtn, wrt)
                                    // })
                                })
                                .unzip::<_, _, Vec<String>, Vec<String>>();

                            let stmt_str = format!(
                                "let ({}) = ({});",
                                ident_str
                                    .into_iter()
                                    .intersperse(String::from(","))
                                    .collect::<String>(),
                                der_str
                                    .into_iter()
                                    .intersperse(String::from(","))
                                    .collect::<String>()
                            );
                            let new_stmt = pass!(syn::parse_str(&stmt_str), NAME);
                            Ok(Some(new_stmt))
                        }
                        _ => Ok(None),
                    }
                }
                None => Ok(None),
            }
        }
        // If we have a return statement e.g. `return (a,b);`
        // TODO If return statement we need to set the accumulative derivative of the return component as the input return derivatives.
        syn::Stmt::Semi(syn::Expr::Return(return_struct), _) => {
            match &return_struct.expr {
                // If there is some return expression e.g. `return (a,b);`
                Some(return_expr) => match &**return_expr {
                    // If return expression is tuple e.g. `return (a,b);`
                    syn::Expr::Tuple(return_tuple) => {
                        let return_idents_res = return_tuple.elems.iter().enumerate().filter_map(|(index,e)| match e {
                            syn::Expr::Path(p) => {
                                let path_ident = p.to_token_stream().to_string();
                                let rtn_ident = rtn!(index);

                                append_insert(&path_ident,rtn_ident.clone(), &mut component_map[index]);

                                let (ident,der) = (wrt!(path_ident,rtn_ident),rtn_ident);
                                // eprintln!("tuple_str: {:?}",tuple_str);
                                Some(Ok((ident,der)))
                            },
                            syn::Expr::Lit(_) => None,
                            _ => {
                                let err = "Unsupported return tuple element. Elements in a returned tuple must be paths or literals (e.g. `return (a,b,2f32))` is supported, `return (a,(b,c))` is not supported).";
                                Diagnostic::spanned(
                                    return_struct.span().unwrap(),
                                    proc_macro::Level::Error,
                                    err,
                                )
                                .emit();
                                Some(Err(err.to_string()))
                            }
                        }).collect::<Result<Vec<_>,_>>();
                        let return_idents = pass!(return_idents_res, NAME);
                        let (ident, der) =
                            return_idents.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

                        let new_return_stmt_str = format!(
                            "let ({}) = ({});",
                            ident
                                .into_iter()
                                .intersperse(String::from(","))
                                .collect::<String>(),
                            der.into_iter()
                                .intersperse(String::from(","))
                                .collect::<String>(),
                        );
                        eprintln!("new_return_stmt_str: {}", new_return_stmt_str);
                        let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                        Ok(Some(new_return_stmt))
                    }
                    // If return expression is path e.g. `return a;`
                    syn::Expr::Path(return_path) => {
                        let path_ident = return_path.to_token_stream().to_string();
                        let rtn_ident = rtn!(0);
                        append_insert(&rtn_ident, path_ident.clone(), &mut component_map[0]);

                        let new_stmt_str = format!("let {} = {};", path_ident, rtn_ident);
                        let new_stmt = pass!(syn::parse_str(&new_stmt_str), NAME);
                        Ok(Some(new_stmt))
                    }
                    syn::Expr::Lit(_) => Ok(None),
                    _ => {
                        let err = "Unsupported return type. Only tuples (e.g. `return (a,b,c);`), paths (e.g. `return a;`) and literals (e.g. `return 5f32;`) are supported.";
                        Diagnostic::spanned(
                            return_struct.span().unwrap(),
                            proc_macro::Level::Error,
                            err,
                        )
                        .emit();
                        panic!("{}", err);
                    }
                },
                // If there is no return expression e.g. `return;`
                None => Ok(None),
            }
        }
        _ => Ok(None),
    }
}
