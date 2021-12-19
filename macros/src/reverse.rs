extern crate proc_macro;
use proc_macro::TokenStream;

use rust_ad_core::utils::*;
use rust_ad_core::*;
use syn::spanned::Spanned;

use std::collections::HashMap;

pub fn add_insert(map: &mut HashMap<String, usize>, string: String) -> usize {
    if let Some(val) = map.get_mut(&string) {
        let c = *val;
        *val += 1;
        c
    } else {
        map.insert(string, 1);
        0
    }
}

pub fn reverse_derivative(stmt: &syn::Stmt) -> Option<syn::Stmt> {
    if let syn::Stmt::Local(local) = stmt {
        // eprintln!("local: {:#?}",local);
        // panic!("just stop here");
        if let Some(init) = &local.init {
            let init_expr = &*init.1;
            if let syn::Expr::Binary(bin_expr) = init_expr {
                // Local Ident String
                let lis = local.pat.ident().ident.to_string();
                return Some(match bin_expr.op {
                    syn::BinOp::Add(_) => {
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
                            _ => panic!(
                                "Uncovered `syn::BinOp::Add(_)` binary expression combination"
                            ),
                        };
                        let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, lis);
                        let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse add");
                        new_stmt
                    }
                    syn::BinOp::Sub(_) => {
                        let (a, b): (String, String) = match (&*bin_expr.left, &*bin_expr.right) {
                            (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => (
                                der!(expr_path_l.path.segments[0].ident.to_string()),
                                format!(
                                    "-{}",
                                    der!(expr_path_r.path.segments[0].ident.to_string())
                                ),
                            ),
                            (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => (
                                der!(expr_path_l.path.segments[0].ident.to_string()),
                                String::from("_"),
                            ),
                            (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => (
                                String::from("_"),
                                format!(
                                    "-{}",
                                    der!(expr_path_r.path.segments[0].ident.to_string())
                                ),
                            ),
                            _ => panic!(
                                "Uncovered `syn::BinOp::Sub(_)` binary expression combination"
                            ),
                        };
                        let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, lis);
                        let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse sub");
                        new_stmt
                    }
                    syn::BinOp::Mul(_) => {
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
                                    expr_lit_r.lit.float().to_string(),
                                );
                                format!("let {} = {}*{};", der!(l), r, lis)
                            }
                            (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
                                let (l, r) = (
                                    expr_lit_l.lit.float().to_string(),
                                    expr_path_r.path.segments[0].ident.to_string(),
                                );
                                format!("let {} = {}*{};", der!(r), l, lis)
                            }
                            _ => panic!(
                                "Uncovered `syn::BinOp::Mul(_)` binary expression combination"
                            ),
                        };
                        let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse mul");
                        new_stmt
                    }
                    syn::BinOp::Div(_) => {
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
                                    expr_lit_r.lit.float().to_string(),
                                );
                                format!("let {} = {}/{};", der!(l), lis, r)
                            }
                            (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
                                let (l, r) = (
                                    expr_lit_l.lit.float().to_string(),
                                    expr_path_r.path.segments[0].ident.to_string(),
                                );
                                format!("let {} = {}*{};", der!(r), l, lis)
                            }
                            _ => panic!(
                                "Uncovered `syn::BinOp::Mul(_)` binary expression combination"
                            ),
                        };
                        let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse div");
                        new_stmt
                    }
                    _ => panic!("Uncovered operation"),
                });
            } else if let syn::Expr::Macro(macro_expr) = init_expr {
                // eprintln!("else: {:#?}", macro_expr);
                let macro_token_stream =
                    proc_macro::TokenStream::from(macro_expr.mac.tokens.clone())
                        .into_iter()
                        .collect::<Vec<_>>();
                let ident = macro_token_stream[0].ident().to_string();
                let num: usize = macro_token_stream[2].literal().to_string().parse().unwrap();
                let token_str = format!(
                    "let {} = {};",
                    der!(ident),
                    (0..num)
                        .map(|c| format!("{}{}", der!(ident), c))
                        .intersperse(String::from("+"))
                        .collect::<String>()
                );
                let der_dup_stmt: syn::Stmt = syn::parse_str(&token_str).expect("reverse dup");
                // eprintln!("der_dup_stmt: {:?}", der_dup_stmt);
                return Some(der_dup_stmt);
            }
        }
    }
    None
}

/// Validates and updates function signature.
pub fn reverse_update_and_validate_signature(
    function: &mut syn::ItemFn,
) -> Result<syn::Stmt, TokenStream> {
    // If there is return statement, return user code, this will leader to compile error about no return function.
    match &mut function.sig.output {
        syn::ReturnType::Type(_, ref mut return_type_type) => {
            // eprintln!("here 1");
            if let Some(mut last_stmt) = function.block.stmts.pop() {
                // eprintln!("here 2: {:#?}",last_stmt);
                if last_stmt.is_semi() {
                    let expr = last_stmt.semi_mut();
                    // eprintln!("here 3");
                    if expr.is_return() {
                        let expr_return = expr.return_mut();
                        // eprintln!("here 4");
                        if expr_return.expr.is_some() {
                            let return_expr = expr_return.expr.as_mut().unwrap();
                            // eprintln!("here 5");
                            if return_expr.is_path() {
                                // eprintln!("here 6");

                                // Updates function output signature.
                                // ---------------------------------------
                                let num_inputs = function.sig.inputs.len();
                                let output = format!("(f32,{})", "f32,".repeat(num_inputs));
                                let new_rtn: syn::Type = syn::parse_str(&output).unwrap();
                                *return_type_type = Box::new(new_rtn);
                                // Updates return statement.
                                // ---------------------------------------
                                let out = return_expr.path().path.segments[0].ident.to_string();
                                // Iter over idents of inputs.
                                let input_idents_iter = function
                                    .sig
                                    .inputs
                                    .iter()
                                    .map(|fn_arg| &fn_arg.typed().pat.ident().ident);
                                let inputs_output_str = input_idents_iter
                                    .map(|ident| format!("{},", der!(ident)))
                                    .collect::<String>();
                                let return_string = format!("({},{})", out, inputs_output_str);
                                let return_tuple: syn::Expr =
                                    syn::parse_str(&return_string).expect("unique 3");
                                expr_return.expr = Some(Box::new(return_tuple));
                                // Updates function input signature.
                                // ---------------------------------------
                                let new_fn_arg_str = format!("{}: f32", der!(out));
                                let new_fn_arg: syn::FnArg =
                                    syn::parse_str(&new_fn_arg_str).unwrap();
                                function.sig.inputs.push(new_fn_arg);

                                return Ok(last_stmt);
                            }
                        }
                    }
                }
            }
            // If return statement does not match the conditions, then simply returning the function should give the user an error.
            Err(TokenStream::from(quote::quote! { #function }))
        }
        syn::ReturnType::Default => Err(TokenStream::from(quote::quote_spanned! {
            function.sig.span() => compile_error!("Expected return type `f32`");
        })),
    }
}
