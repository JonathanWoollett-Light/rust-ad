extern crate proc_macro;
use proc_macro::TokenStream;

use rust_ad_core::traits::*;
use rust_ad_core::*;
use syn::spanned::Spanned;

use std::collections::HashMap;

pub fn reverse_accumulate_inputs(function_inputs: &[String], component_map: &HashMap<String, Vec<String>>, type_map: &HashMap<String, String>) -> syn::Stmt {
    let stmt_str  = format!(
        "let ({}) = ({});",
        function_inputs.iter().map(|input|der!(input)).intersperse(String::from(",")).collect::<String>(),
        function_inputs.iter().map(|input| {
            if let Some(components) = component_map.get(input) {
                components.iter().map(|c|wrt!(input,c)).intersperse(String::from("+")).collect::<String>()
            } else { 
                format!("1{}",type_map.get(input).expect("reverse_accumulate_inputs: no input type")) 
            }
        }).intersperse(String::from(",")).collect::<String>()
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
                    .expect("reverse_derivative: not ident")
                    .ident
                    .to_string();
        let acc_der_stmt_str = format!(
            "let {} = {};",
            der!(local_ident),
            component_map
                .get(&local_ident)
                .expect("reverse_derivative: ident not in map")
                .iter()
                .map(|d|wrt!(local_ident,d))
                .intersperse(String::from("+"))
                .collect::<String>()
        );
        let acc_der_stmt = syn::parse_str(&acc_der_stmt_str).expect("reverse_derivative: acc parse fail");
        Some(acc_der_stmt)
    }
    else { None }
}
pub fn reverse_derivative(
    stmt: &syn::Stmt,
    type_map: &HashMap<String, String>,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    if let syn::Stmt::Local(local) = stmt {
        if let Some(init) = &local.init {
            let init_expr = &*init.1;
            if let syn::Expr::Binary(bin_expr) = init_expr {
                // Creates operation signature struct
                let operation_sig = operation_signature(bin_expr, type_map);
                // Looks up operation with the given lhs type and rhs type and BinOp.
                let operation_out_signature = SUPPORTED_OPERATIONS
                    .get(&operation_sig)
                    .expect("reverse_derivative: unsupported operation");
                // Applies the forward deriative function for the found operation.
                let new_stmt = operation_out_signature.reverse_derivative.expect(
                    "reverse_derivative: binary expression unimplemented forward deriative",
                )(&stmt, component_map);
                return Some(new_stmt);
            } else if let syn::Expr::Call(call_expr) = &*init.1 {
                // Create function in signature
                let function_in_signature = function_signature(call_expr, type_map);
                // Gets function out signature
                let function_out_signature = SUPPORTED_FUNCTIONS
                    .get(&function_in_signature)
                    .expect("reverse_derivative: unsupported function");
                // Gets new stmt
                let new_stmt = function_out_signature
                    .reverse_derivative
                    .expect("reverse_derivative: binary unimplemented forward")(
                    &stmt,
                    component_map,
                );
                return Some(new_stmt);
            } else if let syn::Expr::MethodCall(method_expr) = &*init.1 {
                let method_sig = method_signature(method_expr, type_map);
                let method_out = SUPPORTED_METHODS
                    .get(&method_sig)
                    .expect("reverse_derivative: unsupported method");
                let new_stmt = method_out
                    .reverse_derivative
                    .expect("reverse_derivative: method unimplemented forward")(
                    &stmt,
                    component_map,
                );
                return Some(new_stmt);
            } else if let syn::Expr::Path(expr_path) = &*init.1 {
                // Variable identifier
                let out_ident = local
                    .pat
                    .ident()
                    .expect("reverse_derivative: not ident")
                    .ident
                    .to_string();
                // Variable being assigned to `out_ident`
                let in_ident = expr_path.path.segments[0].ident.to_string();
                
                append_insert(&in_ident,out_ident.clone(),component_map);
                let stmt_str = format!("let {} = {};", wrt!(in_ident,out_ident), der!(out_ident));
                let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_derivative: parse fail");

                return Some(new_stmt);
            } else if let syn::Expr::Lit(expr_lit) = &*init.1 {
                let out_type = literal_type(expr_lit).expect("reverse_derivative: bad lit type");
                let return_type = rust_ad_core::Type::try_from(out_type.as_str())
                    .expect("reverse_derivative: unsupported return type");
                let out_ident = local
                    .pat
                    .ident()
                    .expect("reverse_derivative: not ident")
                    .ident
                    .to_string();
                
                let stmt_str = format!("let {} = 0{};", der!(out_ident),return_type.to_string());
                let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_derivative: parse fail");

                return Some(new_stmt);
            }
        }
    }
    None
}
/// Validates and updates function signature.
pub fn reverse_update_signature(function: &mut syn::ItemFn) -> Result<syn::Stmt, TokenStream> {
    // If there is return statement, return user code, this will leader to compile error about no return function.
    match &mut function.sig.output {
        syn::ReturnType::Type(_, return_type_box) => {
            // eprintln!("here 1");
            if let Some(mut last_stmt) = function.block.stmts.pop() {
                // eprintln!("here 2: {:#?}",last_stmt);
                if last_stmt.is_semi() {
                    let expr = last_stmt
                        .semi_mut()
                        .expect("reverse_update_signature: not semi");
                    // eprintln!("here 3");
                    if expr.is_return() {
                        let expr_return = expr
                            .return_mut()
                            .expect("reverse_update_signature: not return");
                        // eprintln!("here 4");
                        if expr_return.expr.is_some() {
                            let return_expr = expr_return.expr.as_mut().unwrap();
                            // eprintln!("here 5");
                            if return_expr.is_path() {
                                // eprintln!("here 6");

                                // Updates function output signature.
                                // ---------------------------------------
                                let return_type = &mut **return_type_box;
                                let return_type_ident_string = &return_type
                                    .path()
                                    .expect("reverse_update_signature: not path")
                                    .path
                                    .segments[0]
                                    .ident
                                    .to_string();
                                let function_input_types = function
                                    .sig
                                    .inputs
                                    .iter()
                                    .map(|fn_arg| {
                                        fn_arg
                                            .typed()
                                            .expect("reverse_update_signature: arg not typed 1")
                                            .ty
                                            .path()
                                            .expect("reverse_update_signature: arg not path 1")
                                            .path
                                            .segments[0]
                                            .ident
                                            .to_string()
                                    })
                                    .intersperse(String::from(","))
                                    .collect::<String>();
                                let output = format!(
                                    "({},{})",
                                    return_type_ident_string, function_input_types
                                );
                                let new_rtn: syn::Type = syn::parse_str(&output).unwrap();
                                *return_type = new_rtn;
                                // Updates return statement.
                                // ---------------------------------------
                                let out = return_expr
                                    .path()
                                    .expect("reverse_update_signature: not path")
                                    .path
                                    .segments[0]
                                    .ident
                                    .to_string();
                                // Iter over idents of inputs.
                                let input_idents_iter = function.sig.inputs.iter().map(|fn_arg| {
                                    &fn_arg
                                        .typed()
                                        .expect("reverse_update_signature: arg not typed 2")
                                        .pat
                                        .ident()
                                        .expect("reverse_update_signature: arg not path 2")
                                        .ident
                                });
                                let inputs_output_str = input_idents_iter
                                    .map(|ident| format!("{},", der!(ident)))
                                    .collect::<String>();
                                let return_string = format!("({},{})", out, inputs_output_str);
                                let return_tuple: syn::Expr =
                                    syn::parse_str(&return_string).expect("unique 3");
                                expr_return.expr = Some(Box::new(return_tuple));
                                // Updates function input signature.
                                // ---------------------------------------
                                let new_fn_arg_str =
                                    format!("{}: {}", der!(out), return_type_ident_string);
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
