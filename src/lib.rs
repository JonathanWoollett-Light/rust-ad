//! A super restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.
//! ## Status
//! This library is very much a WIP and thus extremely rough, temperamental and inconsistent.
//!
//! I would not recommend you use it at the moment, it is only public to allow the possibility of collaborative work on it.

#![feature(proc_macro_span)]
#![feature(iter_intersperse)]

mod utils;
use utils::*;

extern crate proc_macro;
use proc_macro::TokenStream;

use std::collections::HashMap;
use syn::spanned::Spanned;

/// The prefix used to attached to derivatives of a variable (e.g. The derivative of `x` would be `der_x`).
const DERIVATIVE_PREFIX: &'static str = "der_";

const FORWARD_MODE_PREFIX: &'static str = "__for_";
const REVERSE_MODE_PREFIX: &'static str = "__rev_";

// TODO Do we need `#[macro_export]` here?
/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", DERIVATIVE_PREFIX, $a)
    }};
}

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
            let ident = &fn_arg.typed().pat.ident().ident;
            let string = format!("{}:f32", der!(ident));
            let arg: syn::FnArg = syn::parse_str(&string).expect("failed pass str");
            arg
        })
        .collect::<Vec<_>>();
    for input in sig_inputs.into_iter() {
        function.sig.inputs.push(input);
    }
    // Outputs output signature
    function.sig.output = syn::parse_str(&"->(f32,f32)").unwrap();

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

    let statements = statements
        .into_iter()
        .flat_map(|statement| forward_derivative(statement))
        .collect::<Vec<_>>();
    block.stmts = statements;

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

/// Validates and updates function signature.
fn reverse_update_and_validate_signature(
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

fn add_insert(map: &mut HashMap<String, usize>, string: String) -> usize {
    if let Some(val) = map.get_mut(&string) {
        let c = *val;
        *val += 1;
        c
    } else {
        map.insert(string, 1);
        0
    }
}

fn reverse_derivative(stmt: &syn::Stmt) -> Option<syn::Stmt> {
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

// http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/
fn forward_derivative(stmt: syn::Stmt) -> Vec<syn::Stmt> {
    if let syn::Stmt::Local(ref local) = stmt {
        if let Some(ref init) = local.init {
            if let syn::Expr::Binary(bin_expr) = &*init.1 {
                // eprintln!("bin_expr: {:#?}\n.\n", bin_expr);
                // panic!("stopping here");

                // TODO Do the rest of the operations.
                let new_bin_expr = match bin_expr.op {
                    // y = x1+x2 => dy = dx1 + dx2
                    syn::BinOp::Add(_) => {
                        let derivative = syn::ExprBinary {
                            attrs: Vec::new(),
                            left: Box::new(get_derivative_expr(&*bin_expr.left)),
                            op: syn::BinOp::Add(syn::token::Add {
                                spans: [local.pat.ident().ident.span()],
                            }),
                            right: Box::new(get_derivative_expr(&*bin_expr.right)),
                        };
                        derivative
                    }
                    // y = x1-x2 => dy = dx1 - dx2
                    syn::BinOp::Sub(_) => {
                        let derivative = syn::ExprBinary {
                            attrs: Vec::new(),
                            left: Box::new(get_derivative_expr(&*bin_expr.left)),
                            op: syn::BinOp::Sub(syn::token::Sub {
                                spans: [local.pat.ident().ident.span()],
                            }),
                            right: Box::new(get_derivative_expr(&*bin_expr.right)),
                        };
                        derivative
                    }
                    // y = x1*x2 => dy = (x2*dx1)+(x1*dx2)
                    syn::BinOp::Mul(_) => {
                        let lhs = &bin_expr.left;
                        let rhs = &bin_expr.right;
                        let derivative = syn::ExprBinary {
                            attrs: Vec::new(),
                            left: std::boxed::Box::new(syn::Expr::Binary(syn::ExprBinary {
                                attrs: Vec::new(),
                                left: rhs.clone(),
                                op: syn::BinOp::Mul(syn::token::Star {
                                    spans: [local.pat.ident().ident.span()],
                                }),
                                right: Box::new(get_derivative_expr(&*lhs)),
                            })),
                            op: syn::BinOp::Add(syn::token::Add {
                                spans: [local.pat.ident().ident.span()],
                            }),
                            right: std::boxed::Box::new(syn::Expr::Binary(syn::ExprBinary {
                                attrs: Vec::new(),
                                left: lhs.clone(),
                                op: syn::BinOp::Mul(syn::token::Star {
                                    spans: [local.pat.ident().ident.span()],
                                }),
                                right: Box::new(get_derivative_expr(&*rhs)), // deriv
                            })),
                        };
                        derivative
                    }
                    // y = x1 / x2 => dy = dx1/x2 - (x1/(x2*x2))*dx2
                    syn::BinOp::Div(_) => {
                        let lhs = &bin_expr.left;
                        let rhs = &bin_expr.right;
                        let derivative = syn::ExprBinary {
                            attrs: Vec::new(),
                            left: Box::new(syn::Expr::Binary(syn::ExprBinary {
                                attrs: Vec::new(),
                                left: Box::new(get_derivative_expr(&*lhs)),
                                op: syn::BinOp::Div(syn::token::Div {
                                    spans: [local.pat.ident().ident.span()],
                                }),
                                right: rhs.clone(),
                            })),
                            op: syn::BinOp::Sub(syn::token::Sub {
                                spans: [local.pat.ident().ident.span()],
                            }),
                            right: Box::new(syn::Expr::Binary(syn::ExprBinary {
                                attrs: Vec::new(),
                                left: Box::new(syn::Expr::Binary(syn::ExprBinary {
                                    attrs: Vec::new(),
                                    left: lhs.clone(),
                                    op: syn::BinOp::Div(syn::token::Div {
                                        spans: [local.pat.ident().ident.span()],
                                    }),
                                    right: Box::new(syn::Expr::Binary(syn::ExprBinary {
                                        attrs: Vec::new(),
                                        left: rhs.clone(),
                                        op: syn::BinOp::Mul(syn::token::Star {
                                            spans: [local.pat.ident().ident.span()],
                                        }),
                                        right: rhs.clone(),
                                    })),
                                })),
                                op: syn::BinOp::Mul(syn::token::Star {
                                    spans: [local.pat.ident().ident.span()],
                                }),
                                right: Box::new(get_derivative_expr(&*rhs)),
                            })),
                        };
                        derivative
                    }
                    _ => panic!("Uncovered operation"),
                };

                let rtn = syn::Stmt::Local(syn::Local {
                    attrs: Vec::new(),
                    let_token: syn::token::Let {
                        span: local.pat.ident().ident.span(),
                    },
                    pat: syn::Pat::Ident(syn::PatIdent {
                        attrs: Vec::new(),
                        by_ref: None,
                        mutability: None,
                        ident: syn::Ident::new(
                            &der!(local.pat.ident().ident.to_string()),
                            local.pat.ident().ident.span(),
                        ),
                        subpat: None,
                    }),
                    init: Some((
                        syn::token::Eq {
                            spans: [local.pat.ident().ident.span()],
                        },
                        Box::new(syn::Expr::Binary(new_bin_expr)),
                    )),
                    semi_token: syn::token::Semi {
                        spans: [local.pat.ident().ident.span()],
                    },
                });
                return vec![stmt, rtn];
            }
        }
    } else if let syn::Stmt::Semi(semi, _) = &stmt {
        if let syn::Expr::Return(rtn) = semi {
            if let Some(ref rtn_expr) = rtn.expr {
                if let syn::Expr::Path(expr_path) = &**rtn_expr {
                    let rtn = syn::Stmt::Semi(
                        syn::Expr::Return(syn::ExprReturn {
                            attrs: Vec::new(),
                            return_token: syn::token::Return {
                                span: expr_path.path.segments[0].ident.span(),
                            },
                            expr: Some(Box::new(syn::Expr::Tuple(syn::ExprTuple {
                                attrs: Vec::new(),
                                paren_token: syn::token::Paren {
                                    span: expr_path.path.segments[0].ident.span(),
                                },
                                elems: {
                                    let mut outer_p = syn::punctuated::Punctuated::new();
                                    outer_p.push(syn::Expr::Path(expr_path.clone()));
                                    outer_p.push(syn::Expr::Path(syn::ExprPath {
                                        attrs: Vec::new(),
                                        qself: None,
                                        path: syn::Path {
                                            leading_colon: None,
                                            segments: {
                                                let mut p = syn::punctuated::Punctuated::new();
                                                p.push(syn::PathSegment {
                                                    ident: syn::Ident::new(
                                                        &der!(expr_path.path.segments[0]
                                                            .ident
                                                            .to_string()),
                                                        expr_path.path.segments[0].ident.span(),
                                                    ),
                                                    arguments: syn::PathArguments::None,
                                                });
                                                p
                                            },
                                        },
                                    }));
                                    outer_p
                                },
                            }))),
                        }),
                        syn::token::Semi {
                            spans: [expr_path.path.segments[0].ident.span()],
                        },
                    );
                    return vec![rtn];
                }
            }
        }
    }
    // eprintln!("stmt: {:#?}",stmt);
    vec![stmt]
}

// Given an expr which is either a literal or a variable (e.g. `7` or `self.x`, `y`), returns the derivative of this (e.g. `0` or `d_x`, `d_y`).
fn get_derivative_expr(expr: &syn::Expr) -> syn::Expr {
    match expr {
        syn::Expr::Lit(expr_lit) => syn::Expr::Lit(syn::ExprLit {
            attrs: Vec::new(),
            // TODO use `std::f32::EPSILON` instead of `0f32` here to avoid the `nan` we would get from derivative of `y=12/x` leading to dividing zero.
            lit: syn::Lit::Float(syn::LitFloat::new("0f32", expr_lit.lit.span())),
        }),
        // syn::Expr::Field(expr_field) => syn::Expr::Lit(syn::ExprLit {
        //     attrs: Vec::new(),
        //     lit: syn::Lit::Float(syn::LitFloat::new("1f32", expr_field.member.named().span())),
        // }),
        syn::Expr::Path(expr_path) => syn::Expr::Path(syn::ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: {
                    let mut p = syn::punctuated::Punctuated::new();
                    p.push(syn::PathSegment {
                        ident: syn::Ident::new(
                            &der!(expr_path.path.segments[0].ident.to_string()),
                            expr_path.path.segments[0].ident.span(),
                        ),
                        arguments: syn::PathArguments::None,
                    });
                    p
                },
            },
        }),
        _ => panic!("Unsupported code: ({:?})", expr),
    }
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
