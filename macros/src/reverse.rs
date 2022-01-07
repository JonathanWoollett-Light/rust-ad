extern crate proc_macro;
use proc_macro::Diagnostic;

use proc_macro::Level::Error;
use quote::ToTokens;
use rust_ad_core::*;
use std::collections::HashMap;
use std::collections::HashSet;
use syn::spanned::Spanned;

// Given return statement outputs return statement with appended derivatives
pub fn reverse_append_derivatives(
    stmt: syn::Stmt,
    function_input_identifiers: &[String],
) -> Result<syn::Stmt, PassError> {
    const NAME: &str = "reverse_append_derivatives";
    if let syn::Stmt::Semi(syn::Expr::Return(return_struct), _) = stmt {
        if let Some(return_expr) = &return_struct.expr {
            match &**return_expr {
                // If return expression is tuple e.g. `return (a,b);`
                syn::Expr::Tuple(return_tuple) => {
                    let return_idents_res = return_tuple.elems.iter().enumerate().map(|(index,e)| match e {
                        syn::Expr::Path(p) => {
                            let path_ident = p.to_token_stream().to_string();
                            let rtn_ident = rtn!(index);

                            let (ident,der) = (path_ident,format!("({})",
                            function_input_identifiers
                                .iter()
                                .map(|input|wrt!(input,rtn_ident))
                                .intersperse(String::from(","))
                                .collect::<String>()
                            ));
                            Ok((ident,der))
                        },
                        syn::Expr::Lit(l) => {
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
                    let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                    Ok(new_return_stmt)
                }
                // If return expression is path e.g. `return a;`
                syn::Expr::Path(return_path) => {
                    let path_ident = return_path.to_token_stream().to_string();
                    let rtn_ident = rtn!(0);
                    let tuple_str = match function_input_identifiers.len() {
                        0 => String::new(),
                        1 => wrt!(function_input_identifiers[0], rtn_ident),
                        _ => format!(
                            "({})",
                            function_input_identifiers
                                .iter()
                                .map(|input| wrt!(input, rtn_ident))
                                .intersperse(String::from(","))
                                .collect::<String>()
                        ),
                    };
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
    component_map: &[HashMap<String, Vec<String>>],
    type_map: &HashMap<String, String>,
    return_derivatives: &[HashSet<String>],
) -> Option<syn::Stmt> {
    debug_assert_eq!(component_map.len(), return_derivatives.len());

    let (inputs, derivative) = (0..component_map.len())
        .filter_map(|index| {
            let rtn = rtn!(index);
            let (idents, derivatives) = function_inputs
                .iter()
                .filter_map(|input| {
                    (!return_derivatives[index].contains(input)).then(|| {
                        let ident = wrt!(input, rtn);

                        let sum_str = match component_map[index].get(input) {
                            Some(component_vec) => component_vec
                                .iter()
                                .map(|component| wrtn!(input, component, rtn))
                                .intersperse(String::from("+"))
                                .collect::<String>(),
                            None => format!(
                                "0{}",
                                type_map
                                    .get(&rtn)
                                    .expect("reverse_accumulate_inputs: missed return")
                            ),
                        };
                        (ident, sum_str)
                    })
                })
                .unzip::<_, _, Vec<_>, Vec<_>>();
            match idents.len() {
                0 => None,
                _ => Some((idents, derivatives)),
            }
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let stmt_str = match inputs.len() {
        0 => String::new(),
        1 => match inputs[0].len() {
            0 => String::new(),
            1 => format!("let {} = {};", inputs[0][0], derivative[0][0]),
            _ => format!(
                "let ({}) = ({});",
                inputs[0]
                    .iter()
                    .cloned()
                    .intersperse(String::from(","))
                    .collect::<String>(),
                derivative[0]
                    .iter()
                    .cloned()
                    .intersperse(String::from(","))
                    .collect::<String>()
            ),
        },
        _ => format!(
            "let ({}) = ({});",
            inputs
                .into_iter()
                .map(|i| format!(
                    "({})",
                    i.into_iter()
                        .intersperse(String::from(","))
                        .collect::<String>()
                ))
                .intersperse(String::from(","))
                .collect::<String>(),
            derivative
                .into_iter()
                .map(|i| format!(
                    "({})",
                    i.into_iter()
                        .intersperse(String::from(","))
                        .collect::<String>()
                ))
                .intersperse(String::from(","))
                .collect::<String>()
        ),
    };
    (!stmt_str.is_empty()).then(|| {
        syn::parse_str(&stmt_str)
            .unwrap_or_else(|_| panic!("reverse_accumulate_inputs: parse fail `{}`", stmt_str))
    })
}
fn reverse_accumulate_derivative(
    stmt: &syn::Stmt,
    component_map: &[HashMap<String, Vec<String>>],
    return_derivatives: &mut Vec<HashSet<String>>,
) -> Result<Option<syn::Stmt>, PassError> {
    debug_assert_eq!(component_map.len(), return_derivatives.len());
    const NAME: &str = "reverse_accumulate_derivative";
    match stmt {
        // If we have a local variable declaration statement e.g. `let a;`.
        syn::Stmt::Local(local) => match &local.pat {
            syn::Pat::Ident(local_ident) => {
                let ident_str = local_ident.to_token_stream().to_string();
                let (accumulative_derivatives, derivative_sums) = (0..component_map.len())
                    .filter_map(|index| {
                        component_map[index].get(&ident_str).map(|components| {
                            let rtn = rtn!(index);
                            let acc = wrt!(ident_str, rtn);
                            // Inserting here, notes that now we have a derivative for `ident_str` affecting `rtn!(index)`
                            return_derivatives[index].insert(ident_str.clone());
                            (
                                acc,
                                components
                                    .iter()
                                    .map(|d| wrtn!(ident_str, d, rtn))
                                    .collect::<Vec<_>>(),
                            )
                        })
                    })
                    .unzip::<_, _, Vec<_>, Vec<_>>();

                // equivalent to `derivative_sums.len()`
                let rtn_str = match accumulative_derivatives.len() {
                    0 => Ok(None),
                    1 => match derivative_sums[0].len() {
                        0 => unreachable!(),
                        1 => Ok(Some(format!(
                            "let {} = {};",
                            accumulative_derivatives[0], derivative_sums[0][0]
                        ))),
                        _ => Ok(Some(format!(
                            "let {} = ({});",
                            accumulative_derivatives[0],
                            derivative_sums[0]
                                .iter()
                                .cloned()
                                .intersperse(String::from("+"))
                                .collect::<String>(),
                        ))),
                    },
                    _ => Ok(Some(format!(
                        "let ({}) = ({});",
                        accumulative_derivatives
                            .into_iter()
                            .intersperse(String::from(","))
                            .collect::<String>(),
                        derivative_sums
                            .into_iter()
                            .map(|d| format!(
                                "({})",
                                d.into_iter()
                                    .intersperse(String::from("+"))
                                    .collect::<String>()
                            ))
                            .intersperse(String::from(","))
                            .collect::<String>(),
                    ))),
                };
                rtn_str.map(|res| {
                    res.map(|opt| {
                        syn::parse_str(&opt).expect("reverse_accumulate_derivative: parse fail")
                    })
                })
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
    return_derivatives: &mut Vec<HashSet<String>>,
) -> Result<Vec<syn::Stmt>, PassError> {
    const NAME: &str = "reverse_derivative";
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
                            // Accumulate derivatives for multiplying by components
                            let accumulation_stmt_opt = pass!(
                                reverse_accumulate_derivative(
                                    stmt,
                                    component_map,
                                    return_derivatives
                                ),
                                NAME
                            );
                            let mut rtn_vec = vec![accumulation_stmt_opt];
                            // if let Some(accumulation_stmt)  = accumulation_stmt_opt {
                            //     rtn_vec.push(accumulation_stmt);
                            // }
                            // Create binary operation signature (formed with the lhs type, rhs type and operation symbol (`+`, `-` etc.)).
                            let op_sig = pass!(operation_signature(bin_init_expr, type_map), NAME);
                            // Looks up binary operation of the formed signature in our supported operations map.
                            match SUPPORTED_OPERATIONS.get(&op_sig) {
                                // If we find an entry for an output signature, this means the operation is supported.
                                // Applies the reverse derivative function for the found operation.
                                Some(out_sig) => {
                                    rtn_vec.push((out_sig.reverse_derivative)(
                                        local_ident,
                                        &[
                                            pass!(Arg::try_from(&*bin_init_expr.left), NAME),
                                            pass!(Arg::try_from(&*bin_init_expr.right), NAME),
                                        ],
                                        component_map,
                                        return_derivatives,
                                    ));
                                }
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
                                    return Err(format!("{}: {}", NAME, err));
                                }
                            }
                            Ok(rtn_vec.into_iter().flatten().collect::<Vec<_>>())
                        }
                        // If we have local variable declaration with a function call expression as initialization e.g. `let a = f(b,c);`.
                        syn::Expr::Call(call_init_expr) => {
                            // Accumulate derivatives for multiplying by components
                            let accumulation_stmt_opt = pass!(
                                reverse_accumulate_derivative(
                                    stmt,
                                    component_map,
                                    return_derivatives
                                ),
                                NAME
                            );
                            let mut rtn_vec = vec![accumulation_stmt_opt];
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
                                            .map(Arg::try_from)
                                            .collect::<Result<Vec<_>, _>>(),
                                        NAME
                                    );
                                    // Applies the reverse derivative function for the found function.
                                    let new_stmt = (out_sig.reverse_derivative)(
                                        local_ident,
                                        args.as_slice(),
                                        component_map,
                                        return_derivatives,
                                    );
                                    rtn_vec.push(new_stmt);
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
                                    return Err(format!("{}: {}", NAME, err));
                                }
                            }
                            Ok(rtn_vec.into_iter().flatten().collect::<Vec<_>>())
                        }
                        // If we have local variable declaration with a method call expression as initialization e.g. `let a = b.f(c);`.
                        syn::Expr::MethodCall(method_init_expr) => {
                            // Accumulate derivatives for multiplying by components
                            let accumulation_stmt_opt = pass!(
                                reverse_accumulate_derivative(
                                    stmt,
                                    component_map,
                                    return_derivatives
                                ),
                                NAME
                            );
                            let mut rtn_vec = vec![accumulation_stmt_opt];
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
                                            .map(Arg::try_from)
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
                                        return_derivatives,
                                    );
                                    rtn_vec.push(new_stmt);
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
                                    return Err(format!("{}: {}", NAME, err));
                                }
                            }
                            Ok(rtn_vec.into_iter().flatten().collect::<Vec<_>>())
                        }
                        // If we have local variable declaration with an assignment expression as initialization e.g. `let a = b;`.
                        syn::Expr::Path(path_init_expr) => {
                            // Variable being assigned (e.g. `b`).
                            let in_ident = path_init_expr.to_token_stream().to_string();

                            let (ident_str, der_str) = (0..component_map.len())
                                .filter_map(|index| {
                                    let rtn = rtn!(index);
                                    let from_wrt = wrt!(local_ident, rtn);
                                    let to_wrt = wrt!(&in_ident, rtn);

                                    // If component exists
                                    match component_map[index]
                                        .get(&local_ident)
                                        .map(|e| e.contains(&rtn))
                                    {
                                        Some(true) => {
                                            return_derivatives[index].insert(in_ident.clone());
                                            Some((to_wrt, from_wrt))
                                        }
                                        _ => None,
                                    }
                                })
                                .unzip::<_, _, Vec<String>, Vec<String>>();

                            // equivalent to `der_str.len()`
                            let stmt_str = match ident_str.len() {
                                0 => None,
                                1 => Some(format!("let {} = {};", ident_str[0], der_str[0])),
                                _ => Some(format!(
                                    "let ({}) = ({});",
                                    ident_str
                                        .into_iter()
                                        .intersperse(String::from(","))
                                        .collect::<String>(),
                                    der_str
                                        .into_iter()
                                        .intersperse(String::from(","))
                                        .collect::<String>(),
                                )),
                            };
                            Ok(match stmt_str {
                                Some(s) => {
                                    vec![syn::parse_str(&s).expect("blah blah blah parse fail")]
                                }
                                None => Vec::new(),
                            })
                        }
                        _ => Ok(Vec::new()),
                    }
                }
                None => Ok(Vec::new()),
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

                                return_derivatives[index].insert(path_ident.clone());

                                let (ident,der) = (wrt!(path_ident,rtn_ident),rtn_ident);
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

                        let new_return_stmt = pass!(syn::parse_str(&new_return_stmt_str), NAME);
                        Ok(vec![new_return_stmt])
                    }
                    // If return expression is path e.g. `return a;`
                    syn::Expr::Path(return_path) => {
                        let path_ident = return_path.to_token_stream().to_string();
                        let rtn_ident = rtn!(0);

                        return_derivatives[0].insert(path_ident.clone());

                        let new_stmt_str =
                            format!("let {} = {};", wrt!(path_ident, rtn_ident), rtn_ident);
                        let new_stmt = pass!(syn::parse_str(&new_stmt_str), NAME);
                        Ok(vec![new_stmt])
                    }
                    syn::Expr::Lit(_) => Ok(Vec::new()),
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
                None => Ok(Vec::new()),
            }
        }
        _ => Ok(Vec::new()),
    }
}
