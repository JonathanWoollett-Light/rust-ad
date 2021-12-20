//! A super restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.
//! ## Status
//! This library is very much a WIP and thus extremely rough, temperamental and inconsistent.
//!
//! I would not recommend you use it at the moment, it is only public to allow the possibility of collaborative work on it.

#![feature(proc_macro_span)]
#![feature(iter_intersperse)]

use rust_ad_core::utils::*;
use rust_ad_core::*;

extern crate proc_macro;
use proc_macro::TokenStream;

use std::collections::HashMap;

mod forward;
use forward::*;
mod reverse;
use reverse::*;

/// Calls forward auto-differentiation function corresponding to a given function.
///
/// E.g.:
/// ```
/// #[rust_ad::forward_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7. * x;
///     let b = 3. * y;
///     return b;
/// }
/// fn main() {
///     println!("{:?}",rust_ad::forward!(function_name,2.,4.,1.,5.))
/// }
/// ```
/// This is just a replacement for:
/// ```
/// #[macro_export]
/// macro_rules! forward {
///     ($f:ident,$($x:expr),*) => {{
///         FORWARD_MODE_PREFIX$ident($($x,)*);
///     }}
/// }
/// ```
/// Since you can't export declarative macros from a procedural macro crate.
#[proc_macro]
pub fn forward(_item: TokenStream) -> TokenStream {
    let mut items = _item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let inputs = items
        .enumerate()
        .filter_map(|(index, token)| {
            if index % 2 == 0 {
                match token {
                    proc_macro::TokenTree::Punct(_) => None,
                    _ => panic!("punctuation token out of place"),
                }
            } else {
                match token {
                    proc_macro::TokenTree::Literal(num) => Some(num.to_string()),
                    _ => panic!("literal token out of place"),
                }
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();

    let call_str = format!("{}{}({})", FORWARD_MODE_PREFIX, function_ident, inputs);
    call_str.parse().unwrap()
}
/// Calls reverse auto-differentiation function corresponding to a given function.
///
/// E.g.:
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7. * x;
///     let b = 3. * y;
///     return b;
/// }
/// fn main() {
///     println!("{:?}",rust_ad::reverse!(function_name,2.,4.,1.))
/// }
/// ```
/// This is just a replacement for:
/// ```
/// #[macro_export]
/// macro_rules! reverse {
///     ($f:ident,$($x:expr),*) => {{
///         REVERSE_MODE_PREFIX$ident($($x,)*);
///     }}
/// }
/// ```
/// Since you can't export declarative macros from a procedural macro crate.
#[proc_macro]
pub fn reverse(_item: TokenStream) -> TokenStream {
    let mut items = _item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let inputs = items
        .enumerate()
        .filter_map(|(index, token)| {
            if index % 2 == 0 {
                match token {
                    proc_macro::TokenTree::Punct(_) => None,
                    _ => panic!("punctuation token out of place"),
                }
            } else {
                match token {
                    proc_macro::TokenTree::Literal(num) => Some(num.to_string()),
                    _ => panic!("literal token out of place"),
                }
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();

    let call_str = format!("{}{}({})", REVERSE_MODE_PREFIX, function_ident, inputs);
    call_str.parse().unwrap()
}

/// Flattens nested binary expressions into separate variable assignments.
///
/// E.g.
/// ```
/// #[rust_ad::unweave]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let v = 2. * x + y / 3.;
///     return v;
/// }
/// ```
/// Expands to:
/// ```
/// fn function_name(x: f32, y: f32) -> f32 {
///     let _v = 2. * x;
///     let v_ = y / 3.;
///     let v = _v + v_;
///     return v;
/// }
/// ```
#[proc_macro_attribute]
pub fn unweave(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);

    // Checks item is impl.
    let mut ast = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Macro must be applied to a `fn`"),
    };

    let block = &mut ast.block;

    let statements = block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    block.stmts = statements;

    let new = quote::quote! { #ast };
    TokenStream::from(new)
}

/// Generates the forward auto-differentiation function for a given function.
///
/// Only works with:
/// - `f32`s
/// - Primitive operations `-`, `+`, `*`, and `/`
///
/// E.g.
/// ```
/// #[rust_ad::forward_autodiff]
/// fn function_name(x:f32, y:f32) -> f32 {
///     let p = 7. * x;
///     let r = 10. - y;
///     let q = p * x * 5.;
///     let v = 2. * p * q + 3. * r;
///     return v;
/// }
/// ```
/// Expands to:
/// ```
/// fn __for_function_name(x: f32, y: f32, der_x: f32, der_y: f32) -> (f32, f32) {
///     let a = 7. * x;
///     let der_a = x * 0f32 + 7. * der_x;
///     let b = 3. * x;
///     let der_b = x * 0f32 + 3. * der_x;
///     let c = x + b;
///     let der_c = der_x + der_b;
///     let _d = y + b;
///     let der__d = der_y + der_b;
///     let d = _d + c;
///     let der_d = der__d + der_c;
///     return (d, der_d);
/// }
/// ```
/// Much like a derive macro, this is appended to your code, the original `function_name` function remains unedited.
#[proc_macro_attribute]
pub fn forward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);
    // eprintln!("{:#?}",ast);

    // Checks item is function.
    let mut function = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };

    let function_holder = function.clone();

    // Updates signature
    // ---------------------------------------------------------------------------
    // Updates identifier
    function.sig.ident = syn::Ident::new(
        &format!("{}{}", FORWARD_MODE_PREFIX, function.sig.ident.to_string()),
        function.sig.ident.span(),
    );
    // Appends derivative inputs to function signature, `f(x,y)` -> `f(x,y,dx,dy)`
    let sig_inputs = function
        .sig
        .inputs
        .iter()
        .map(|fn_arg| {
            // eprintln!("fn_arg:\n{:#?}",fn_arg);
            let typed = fn_arg.typed();
            let val_type = &typed.ty.path().path.segments[0].ident;
            let ident = &typed.pat.ident().ident;
            let string = format!("{}:{}", der!(ident), val_type);
            let arg: syn::FnArg =
                syn::parse_str(&string).expect("`forward_autodiff` failed input parse");
            arg
        })
        .collect::<Vec<_>>();
    for input in sig_inputs.into_iter() {
        function.sig.inputs.push(input);
    }
    // Outputs output signature
    eprint!("function.sig.output: {:#?}", function.sig.output);
    let return_type = &function.sig.output.type_().path().path.segments[0].ident;
    let return_string = format!("->({},{})", return_type, return_type);
    function.sig.output =
        syn::parse_str(&return_string).expect("`forward_autodiff` failed output parse");

    // Forward autodiff
    // ---------------------------------------------------------------------------
    let block = &mut function.block;
    // eprintln!("\n\nblock:\n{:?}\n\n", block);

    let statements = block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    // block.stmts = statements;

    // Intersperses forward deriatives
    block.stmts = interspese_succedding(statements, forward_derivative);
    // Updates return statement
    update_forward_return(block.stmts.last_mut());

    let new = quote::quote! { #function_holder #function };
    TokenStream::from(new)
}

/// Clones a variable a given number of times.
///
/// Returns a tuple of clones of this variable. `dup!(x,3)` -> `(x.clone(),x.clone(),x.clone())`.
///
/// Useful internally.
#[proc_macro]
pub fn dup(_item: TokenStream) -> TokenStream {
    // eprintln!("what?: {:?}",_item);
    let vec = _item.into_iter().collect::<Vec<_>>();
    match (vec.get(0), vec.get(1), vec.get(2)) {
        (
            Some(proc_macro::TokenTree::Ident(var)),
            Some(proc_macro::TokenTree::Punct(_)),
            Some(proc_macro::TokenTree::Literal(num)),
        ) => {
            let tuple = format!(
                "({})",
                format!("{}.clone(),", var.to_string()).repeat(num.to_string().parse().unwrap())
            );
            tuple.parse().unwrap()
        }
        _ => panic!("Bad input"),
    }
}

/// Generates the reverse auto-differentiation function for a given function.
///
/// Only works with:
/// - `f32`s
/// - Primitive operations `-`, `+`, `*`, and `/`
///
/// E.g.
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7. * x;
///     let b = 3. * x;
///     let c = x + b;
///     let d = y + b + c;
///     return d;
/// }
/// ```
/// Expands to:
/// ```
/// fn __rev_function_name(x: f32, y: f32, der_d: f32) -> (f32, f32, f32) {
///     let (x0, x1, x2) = (x.clone(), x.clone(), x.clone());
///     let (y0,) = (y.clone(),);
///     let a = 7. * x0;
///     let b = 3. * x1;
///     let (b0, b1) = (b.clone(), b.clone());
///     let c = x2 + b0;
///     let (c0,) = (c.clone(),);
///     let _d = y0 + b1;
///     let (_d0,) = (_d.clone(),);
///     let d = _d0 + c0;
///     let (der__d0, der_c0) = (d.clone(), d.clone());
///     let der__d = der__d0;
///     let (der_y0, der_b1) = (_d.clone(), _d.clone());
///     let der_c = der_c0;
///     let (der_x2, der_b0) = (c.clone(), c.clone());
///     let der_b = der_b0 + der_b1;
///     let der_x1 = 3. * b;
///     let der_x0 = 7. * a;
///     let der_y = der_y0;
///     let der_x = der_x0 + der_x1 + der_x2;
///     return (d, der_x, der_y);
/// }
/// ```
/// Much like a derive macro, this is appended to your code, the original `function_name` function remains unedited.
#[proc_macro_attribute]
pub fn reverse_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);
    // Checks item is function.
    let mut function = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };

    let function_holder = function.clone();

    // Add input derivatives to output signature & validates output signature.
    // ---------------------------------------------------------------------------
    let return_stmt = match reverse_update_and_validate_signature(&mut function) {
        Ok(rtn_stmt) => rtn_stmt,
        Err(rtn) => return rtn,
    };

    // Unwraps nested binary expressions.
    // ---------------------------------------------------------------------------
    let statements = function
        .block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    function.block.stmts = statements;

    // Counts number of times each variable appears.
    // ---------------------------------------------------------------------------
    let mut counts = HashMap::new();
    for stmt in function.block.stmts.iter_mut() {
        // eprintln!("counting: {}, {:#?}",index,stmt);
        if let syn::Stmt::Local(local) = stmt {
            if let Some(init) = &mut local.init {
                if let syn::Expr::Binary(bin) = &mut *init.1 {
                    if let syn::Expr::Path(expr_path) = &mut *bin.left {
                        let ident = &expr_path.path.segments[0].ident;

                        let count = add_insert(&mut counts, format!("{}", ident));
                        expr_path.path.segments[0].ident =
                            syn::Ident::new(&format!("{}{}", ident, count), ident.span());
                    }
                    if let syn::Expr::Path(expr_path) = &mut *bin.right {
                        let ident = &expr_path.path.segments[0].ident;
                        let count = add_insert(&mut counts, format!("{}", ident));
                        expr_path.path.segments[0].ident =
                            syn::Ident::new(&format!("{}{}", ident, count), ident.span());
                    }
                }
            }
        }
    }

    // Duplicates inputs for each usage.
    // ---------------------------------------------------------------------------
    let mut dup_inputs = Vec::new();
    // `.rev()` is unnecessary here, but it declare dupes of inputs in order that inputs are declared, so it's nice.
    for input in function.sig.inputs.iter() {
        match input {
            syn::FnArg::Typed(t) => {
                match &*t.pat {
                    syn::Pat::Ident(i) => {
                        let ident_str = format!("{}", i.ident);
                        if let Some(count) = counts.remove(&ident_str) {
                            let output = format!(
                                "let ({}) = rust_ad::dup!({},{});",
                                (0..count)
                                    .map(|c| format!("{}{},", ident_str, c))
                                    .collect::<String>(),
                                ident_str,
                                count
                            );
                            // eprintln!("here? {}",output);
                            let new_stmt: syn::Stmt = syn::parse_str(&output).unwrap();
                            // eprintln!("here??");
                            dup_inputs.push(new_stmt);
                        }
                    }
                    _ => panic!("All function inputs need to be identified"),
                }
            }
            _ => panic!("All function inputs need to be typed"),
        }
    }

    // Duplicates variables for each usage.
    // ---------------------------------------------------------------------------
    let mut dup_stmts = Vec::new();
    for stmt in function.block.stmts.iter() {
        let mut joint = vec![stmt.clone()];
        if let syn::Stmt::Local(local) = stmt {
            if let syn::Pat::Ident(pat_ident) = &local.pat {
                let str_ident = pat_ident.ident.to_string();
                if let Some(count) = counts.get(&str_ident) {
                    let new_str = format!(
                        "let ({}) = rust_ad::dup!({},{});",
                        (0..*count)
                            .map(|c| format!("{}{},", str_ident, c))
                            .collect::<String>(),
                        str_ident,
                        count
                    );
                    let new_stmt: syn::Stmt = syn::parse_str(&new_str).unwrap();
                    joint.push(new_stmt);
                }
            }
        }
        dup_stmts.append(&mut joint);
    }
    dup_inputs.append(&mut dup_stmts);
    function.block.stmts = dup_inputs;

    // Generates reverse mode code
    // ---------------------------------------------------------------------------
    let mut reverse_stmts = function
        .block
        .stmts
        .iter()
        .rev()
        .filter_map(|s| reverse_derivative(s))
        .collect::<Vec<_>>();

    function.block.stmts.append(&mut reverse_stmts);
    // Appends return statement after adding reverse code.
    function.block.stmts.push(return_stmt);

    // Updates function identifier
    // ---------------------------------------------------------------------------
    function.sig.ident = syn::Ident::new(
        &format!("{}{}", REVERSE_MODE_PREFIX, function.sig.ident.to_string()),
        function.sig.ident.span(),
    );

    let new = quote::quote! { #function_holder #function };
    let rtn = TokenStream::from(new);
    // eprintln!("rtn:\n{}", rtn);
    rtn
}

fn unwrap_statement(stmt: &syn::Stmt) -> Vec<syn::Stmt> {
    let mut statements = Vec::new();

    // TODO Avoid this clone.
    let mut base_statement = stmt.clone();

    // If the statement is local variable declaration (e.g. `let ...`).
    if let syn::Stmt::Local(local) = stmt {
        let local_ident = &local.pat.ident().ident;
        // If our statement has some initialization (e.g. `let a = 3;`).
        if let Some(init) = local.init.as_ref() {
            // If initialization is a binary expression (e.g. `let a = b + c;`).
            if let syn::Expr::Binary(bin_expr) = init.1.as_ref() {
                // If left side of expression is binary expression.
                if let syn::Expr::Binary(left_bin_expr) = bin_expr.left.as_ref() {
                    // Creates new left statement.
                    let mut left_stmt = stmt.clone();
                    let left_local = left_stmt.local_mut();
                    let left_ident = format!("_{}", local_ident.to_string());
                    left_local.pat.ident_mut().ident =
                        syn::Ident::new(&left_ident, local_ident.span());
                    *left_local.init.as_mut().unwrap().1 = syn::Expr::Binary(left_bin_expr.clone());
                    // Recurse
                    statements.append(&mut unwrap_statement(&left_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let mut p = syn::punctuated::Punctuated::new();
                    p.push(syn::PathSegment {
                        ident: syn::Ident::new(&left_ident, local_ident.span()),
                        arguments: syn::PathArguments::None,
                    });
                    *base_statement
                        .local_mut()
                        .init
                        .as_mut()
                        .unwrap()
                        .1
                        .binary_mut()
                        .left = syn::Expr::Path(syn::ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: p,
                        },
                    });
                }
                // If right side of expression is binary expression.
                if let syn::Expr::Binary(right_bin_expr) = bin_expr.right.as_ref() {
                    // Creates new left statement.
                    let mut right_stmt = stmt.clone();
                    let right_local = right_stmt.local_mut();
                    let right_ident = format!("{}_", local_ident.to_string());
                    right_local.pat.ident_mut().ident =
                        syn::Ident::new(&right_ident, local_ident.span());
                    *right_local.init.as_mut().unwrap().1 =
                        syn::Expr::Binary(right_bin_expr.clone());
                    // Recurse
                    statements.append(&mut unwrap_statement(&right_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let mut p = syn::punctuated::Punctuated::new();
                    p.push(syn::PathSegment {
                        ident: syn::Ident::new(&right_ident, local_ident.span()),
                        arguments: syn::PathArguments::None,
                    });
                    *base_statement
                        .local_mut()
                        .init
                        .as_mut()
                        .unwrap()
                        .1
                        .binary_mut()
                        .right = syn::Expr::Path(syn::ExprPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: p,
                        },
                    });
                }
            }
        }
    }
    statements.push(base_statement);
    statements
}
