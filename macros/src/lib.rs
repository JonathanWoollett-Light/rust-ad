#![feature(proc_macro_span)]
#![feature(iter_intersperse)]

//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use rust_ad_core::traits::*;
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
/// E.g.
/// ```
/// #[rust_ad::forward_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7f32 * x;
///     let b = 3f32 * y;
///     return b;
/// }
/// fn main() {
///     println!("{:?}",rust_ad::forward!(function_name,2.,4.,1.,5.))
/// }
/// ```
/// This is just a procedural functional macro replacement for the declarative macro:
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
/// E.g.
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7f32 * x;
///     let b = 3f32 * y;
///     return b;
/// }
/// fn main() {
///     println!("{:?}",rust_ad::reverse!(function_name,2.,4.,1.))
/// }
/// ```
/// This is just a procedural functional macro replacement for the declarative macro:
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
///     let v = 2f32 * x + y / 3.0f32;
///     return v;
/// }
/// ```
/// Expands to:
/// ```
/// fn function_name(x: f32, y: f32) -> f32 {
///     let _v = 2f32 * x;
///     let v_ = y / 3.0f32;
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
/// E.g.
/// ```
/// #[rust_ad::forward_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let p = 7.0f32 * x;
///     let r = 10f32 - y;
///     let q = p * x * 5f32;
///     let v = 2f32 * p * q + 3.0f32 * r;
///     return v;
/// }
/// ```
/// Expands to:
/// ```
/// fn __for_function_name(x: f32, y: f32, __der_x: f32, __der_y: f32) -> (f32, f32) {
///     let p = 7.0f32 * x;
///     let __der_p = x * 0f32 + 7.0f32 * __der_x;
///     let r = 10f32 - y;
///     let __der_r = 0f32 - __der_y;
///     let _q = p * x;
///     let __der__q = x * __der_p + p * __der_x;
///     let q = _q * 5f32;
///     let __der_q = 5f32 * __der__q + _q * 0f32;
///     let __v = 2f32 * p;
///     let __der___v = p * 0f32 + 2f32 * __der_p;
///     let _v = __v * q;
///     let __der__v = q * __der___v + __v * __der_q;
///     let v_ = 3.0f32 * r;
///     let __der_v_ = r * 0f32 + 3.0f32 * __der_r;
///     let v = _v + v_;
///     let __der_v = __der__v + __der_v_;
///     return (v, __der_v);
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
    let (sig_inputs, joined) = function
        .sig
        .inputs
        .iter()
        .map(|fn_arg| {
            // eprintln!("fn_arg:\n{:#?}",fn_arg);
            let typed = fn_arg.typed().expect("forward: signatre input not typed");
            let val_type = typed
                .ty
                .path()
                .expect("forward: signature input not path")
                .path
                .segments[0]
                .ident
                .to_string();
            let ident_str = typed
                .pat
                .ident()
                .expect("forward: signature input not ident")
                .ident
                .to_string();
            let string = format!("{}:{}", der!(&ident_str), val_type);
            let arg: syn::FnArg = syn::parse_str(&string).expect("forward: failed input parse");

            (arg, (ident_str, val_type))
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    let (function_inputs, function_input_types) =
        joined.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
    for input in sig_inputs.into_iter() {
        function.sig.inputs.push(input);
    }
    // Updates output signature
    // eprint!("function.sig.output: {:#?}", function.sig.output);
    let return_type = &function
        .sig
        .output
        .type_()
        .expect("forward: return not typed")
        .path()
        .expect("forward: return not path")
        .path
        .segments[0]
        .ident;
    let return_string = format!(
        "->({},{})",
        return_type,
        function_input_types
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>()
    );
    function.sig.output = syn::parse_str(&return_string).expect("forward: failed output parse");

    // Forward autodiff
    // ---------------------------------------------------------------------------

    function.block.stmts = function
        .block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    // function.block.stmts = statements;
    let type_map = propagate_types(&function);

    // Intersperses forward deriatives
    function.block.stmts = interspese_succedding_stmts(
        function.block.stmts,
        (&type_map, function_inputs.as_slice()),
        forward_derivative,
    );
    // Updates return statement
    update_forward_return(function.block.stmts.last_mut(), function_inputs.as_slice());

    let new = quote::quote! {
        #function_holder
        use num::Zero;
        #function
    };
    TokenStream::from(new)
}

/// Returns a tuple of a given number of clones of a variable.
/// ```
/// fn main() {
///     let x = 2;
///     assert_eq!(rust_ad::dup!(x,3),(x.clone(),x.clone(),x.clone()));
/// }
/// ```
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
/// E.g.
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
///     let a = 7.0f32 * x;
///     let b = 3f32 * x;
///     let c = x + b;
///     let d = y + b + c;
///     return d;
/// }
/// ```
/// Expands to:
/// ```
/// fn __rev_function_name(x: f32, y: f32, __der_d: f32) -> (f32, f32, f32) {
///     let (x0, x1, x2) = (x.clone(), x.clone(), x.clone());
///     let (y0,) = (y.clone(),);
///     let a = 7.0f32 * x0;
///     let b = 3f32 * x1;
///     let (b0, b1) = (b.clone(), b.clone());
///     let c = x2 + b0;
///     let (c0,) = (c.clone(),);
///     let _d = y0 + b1;
///     let (_d0,) = (_d.clone(),);
///     let d = _d0 + c0;
///     let (__der__d0, __der_c0) = (__der_d.clone(), __der_d.clone());
///     let __der__d = __der__d0;
///     let (__der_y0, __der_b1) = (__der__d.clone(), __der__d.clone());
///     let __der_c = __der_c0;
///     let (__der_x2, __der_b0) = (__der_c.clone(), __der_c.clone());
///     let __der_b = __der_b0 + __der_b1;
///     let __der_x1 = 3f32 * b;
///     let __der_x0 = 7.0f32 * a;
///     let __der_y = __der_y0;
///     let __der_x = __der_x0 + __der_x1 + __der_x2;
///     return (d, __der_x, __der_y);
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
    let return_stmt = match reverse_update_signature(&mut function) {
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

    // Gets types
    let type_map = propagate_types(&function);

    // Generates reverse mode code
    // ---------------------------------------------------------------------------
    let mut reverse_stmts = function
        .block
        .stmts
        .iter()
        .rev()
        .filter_map(|s| reverse_derivative(s, &type_map))
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

/// Unwraps nested expressions into seperate variable assignments.
///
/// E.g.
/// ```ignore
/// let a = b*c + d/e;
/// ```
/// Becomes:
/// ```ignore
/// let _a = b*c;
/// let a_ = d/c;
/// let a = _a + a_;
/// ```
///
/// E.g.
/// ```ignore
/// let a = function_one(function_two(b)+c);
/// ```
/// Becomes:
/// ```ignore
/// let __a = function_two(b);
/// let _a = __a + c;
/// let a = function_one(_a);
/// ```
fn unwrap_statement(stmt: &syn::Stmt) -> Vec<syn::Stmt> {
    // eprintln!("unwrap stmt:\n{:#?}\n", stmt);

    let mut statements = Vec::new();
    // TODO Avoid this clone.
    let mut base_statement = stmt.clone();

    // If the statement is local variable declaration (e.g. `let ...`).
    if let syn::Stmt::Local(local) = stmt {
        let local_ident = &local
            .pat
            .ident()
            .expect(&format!("unwrap_statement: non-ident local pattern (must be `let x =...;`, cannot be a tuple etc.): {{\n{:#?}\n}}",local))
            .ident.to_string();
        // If our statement has some initialization (e.g. `let a = 3;`).
        if let Some(init) = local.init.as_ref() {
            // eprintln!("init: {:#?}", init);

            // If initialization is a binary expression (e.g. `let a = b + c;`).
            if let syn::Expr::Binary(bin_expr) = init.1.as_ref() {
                // If left side of expression is binary expression.
                if let syn::Expr::Binary(left_bin_expr) = bin_expr.left.as_ref() {
                    // Creates new left statement.
                    let mut left_stmt = stmt.clone();
                    let left_local = left_stmt
                        .local_mut()
                        .expect("unwrap: left statement not local");
                    let left_ident = format!("_{}", local_ident);
                    left_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: left not ident")
                        .ident = syn::parse_str(&left_ident).expect("unwrap: left ident parse fail");
                    *left_local.init.as_mut().unwrap().1 = syn::Expr::Binary(left_bin_expr.clone());
                    // Recurse
                    statements.append(&mut unwrap_statement(&left_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let left_expr: syn::Expr =
                        syn::parse_str(&left_ident).expect("unwrap: left parse fail");
                    *base_statement
                        .local_mut()
                        .expect("unwrap: 1a")
                        .init
                        .as_mut()
                        .unwrap()
                        .1
                        .binary_mut()
                        .expect("unwrap: 1b")
                        .left = left_expr;
                }
                // If right side of expression is binary expression.
                if let syn::Expr::Binary(right_bin_expr) = bin_expr.right.as_ref() {
                    // Creates new left statement.
                    let mut right_stmt = stmt.clone();
                    let right_local = right_stmt
                        .local_mut()
                        .expect("unwrap: right statement not local");
                    let right_ident = format!("{}_", local_ident);
                    right_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: right not ident")
                        .ident = syn::parse_str(&right_ident).expect("unwrap: right ident parse fail");
                    *right_local.init.as_mut().unwrap().1 =
                        syn::Expr::Binary(right_bin_expr.clone());
                    // Recurse
                    statements.append(&mut unwrap_statement(&right_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let right_expr: syn::Expr =
                        syn::parse_str(&right_ident).expect("unwrap: right parse fail");
                    *base_statement
                        .local_mut()
                        .expect("unwrap: 2a")
                        .init
                        .as_mut()
                        .unwrap()
                        .1
                        .binary_mut()
                        .expect("unwrap: 2b")
                        .right = right_expr;
                }
            }
            // If initialization is function call (e.g. `let a = my_function(b,c);`).
            else if let syn::Expr::Call(call_expr) = init.1.as_ref() {
                // eprintln!("call_expr: {:#?}",call_expr);

                // For each function argument.
                for (i, arg) in call_expr.args.iter().enumerate() {
                    // eprintln!("i: {:#?}, arg: {:#?}",i,arg);

                    // If function argument is binary expression
                    if let syn::Expr::Binary(arg_bin_expr) = arg {
                        // eprintln!("arg_bin_expr: {:#?}",arg_bin_expr);

                        // Creates new function argument statement.
                        let mut func_stmt = stmt.clone();
                        let func_local = func_stmt
                            .local_mut()
                            .expect("unwrap: function statement not local");
                        let func_ident =
                            format!("{}_{}", FUNCTION_PREFFIX.repeat(i + 1), local_ident);
                        func_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: function not ident")
                            .ident = syn::parse_str(&func_ident).expect("unwrap: function ident parse fail");
                        *func_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(arg_bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&func_stmt));

                        // Updates statement to contain reference to new variables
                        let arg_expr: syn::Expr =
                            syn::parse_str(&func_ident).expect("unwrap: funtion parse fail");
                        base_statement
                            .local_mut()
                            .expect("unwrap: function local")
                            .init
                            .as_mut()
                            .unwrap()
                            .1
                            .call_mut()
                            .expect("unwrap: function call")
                            .args[i] = arg_expr;
                    }
                }
            }
            // If initialization is method call (e.g. `let a = b.my_function(c);`).
            else if let syn::Expr::MethodCall(method_expr) = init.1.as_ref() {
                // If method is call on value in parenthesis (e.g. `(x).method()`).
                if let syn::Expr::Paren(parenthesis) = &*method_expr.receiver {
                    // If method is called on value which is binary expression (e.g. `(x+y).method()`).
                    if let syn::Expr::Binary(bin_expr) = &*parenthesis.expr {
                        // Creates new statement.
                        let mut reciver_stmt = stmt.clone();
                        let reciver_local = reciver_stmt
                            .local_mut()
                            .expect("unwrap: receiver statement not local");
                        let reciver_ident =
                            format!("{}_{}", RECEIVER_PREFIX, local_ident.to_string());
                        reciver_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: receiver not ident")
                            .ident = syn::parse_str(&reciver_ident).expect("unwrap: receiver ident parse fail");
                        *reciver_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&reciver_stmt));

                        // Updates statement to contain variable referencing new statement.
                        let receiver_expr: syn::Expr =
                            syn::parse_str(&reciver_ident).expect("unwrap: receiver parse fail");
                        *base_statement
                            .local_mut()
                            .expect("unwrap: 3a")
                            .init
                            .as_mut()
                            .unwrap()
                            .1
                            .method_call_mut()
                            .expect("unwrap: 3b")
                            .receiver = receiver_expr;
                    }
                }
                for (i, arg) in method_expr.args.iter().enumerate() {
                    // eprintln!("i: {:#?}, arg: {:#?}",i,arg);

                    // If function argument is binary expression
                    if let syn::Expr::Binary(arg_bin_expr) = arg {
                        // eprintln!("arg_bin_expr: {:#?}",arg_bin_expr);

                        // Creates new function argument statement.
                        let mut func_stmt = stmt.clone();
                        let func_local = func_stmt
                            .local_mut()
                            .expect("unwrap: method statement not local");
                        let func_ident =
                            format!("{}_{}", FUNCTION_PREFFIX.repeat(i + 1), local_ident);
                        func_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: method not ident")
                            .ident = syn::parse_str(&func_ident).expect("unwrap: method ident parse fail");
                        *func_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(arg_bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&func_stmt));

                        // Updates statement to contain reference to new variables
                        let arg_expr: syn::Expr =
                            syn::parse_str(&func_ident).expect("unwrap: method parse fail");
                        base_statement
                            .local_mut()
                            .expect("unwrap: method local")
                            .init
                            .as_mut()
                            .unwrap()
                            .1
                            .method_call_mut()
                            .expect("unwrap: method call")
                            .args[i] = arg_expr;
                    }
                }
            }
        }
    }
    else if let syn::Stmt::Semi(semi_expr,_) = stmt {
        if let syn::Expr::Return(rtn_expr) = semi_expr {
            if let Some(rtn) = &rtn_expr.expr {
                if let syn::Expr::Binary(_bin_expr) = &**rtn {
                    let new_ident = format!("__{}",RETURN_SUFFIX);
                    let new_stmt_str = format!("let {};",new_ident);
                    let mut new_stmt: syn::Stmt = syn::parse_str(&new_stmt_str).expect("unwrap: return stmt parse fail");
                    let new_local = new_stmt
                        .local_mut()
                        .expect("unwrap: return statement not local");
                    new_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: return not ident")
                        .ident = syn::parse_str(&new_ident).expect("unwrap: return ident parse fail");
                    
                    // TODO Create `eq_token` some better way.
                    let eq_token = syn::parse_str("=").expect("unwrap: fml this is dumb");

                    new_local.init = Some((eq_token,rtn.clone()));
                    // Recurse
                    statements.append(&mut unwrap_statement(&new_stmt));

                    
                    // Updates statement to contain variable referencing new statement.
                    let new_rtn_str = format!("return {};",new_ident);
                    let new_rtn_expr: syn::Stmt =
                        syn::parse_str(&new_rtn_str).expect("unwrap: return parse fail");
                    base_statement = new_rtn_expr;
                }
            }
        }
    }

    statements.push(base_statement);
    // eprintln!("statements.len(): {}", statements.len());
    statements
}

/// Gets the types of all variables in a function.
///
/// Propagates types through variables in a function from the input types.
///
/// Returns a hashmap of identifier->type.
///
/// CURRENTLY DOES NOT SUPPORT PROCEDURES WHICH RETURN MULTIPLE DIFFERENT TYPES
fn propagate_types(func: &syn::ItemFn) -> HashMap<String, String> {
    let mut map = HashMap::new();

    // Add input types
    for arg in func.sig.inputs.iter() {
        let typed = arg.typed().expect("propagate_types: not typed");
        // eprintln!("typed: {:#?}",typed);
        let ident = &typed.pat.ident().expect("propagate_types: not ident").ident;
        let type_ident = &typed
            .ty
            .path()
            .expect("propagate_types: not path")
            .path
            .segments[0]
            .ident;
        map.insert(ident.to_string(), type_ident.to_string());
    }

    // Propagates types through statements
    for stmt in func.block.stmts.iter() {
        // eprintln!("map: {:?}", map);
        if let syn::Stmt::Local(local) = stmt {
            // Gets identifier/s of variable/s being defined
            let var_idents = match &local.pat {
                syn::Pat::Ident(pat_ident) => vec![pat_ident.ident.to_string()],
                syn::Pat::Tuple(pat_tuple) => pat_tuple
                    .elems
                    .iter()
                    .map(|e| {
                        e.ident()
                            .expect("propagate_types: tuple not ident")
                            .ident
                            .to_string()
                    })
                    .collect(),
                _ => panic!("propagate_types: local pat not ident:\n{:#?}", local.pat),
            };
            if let Some(init) = &local.init {
                let output_type_opt = if let syn::Expr::Binary(bin_expr) = &*init.1 {
                    // Sets result type
                    let operation_sig = operation_signature(bin_expr, &map);
                    // I think this is cleaner than embedding a `format!` within an `.expect`
                    let out_sig = match SUPPORTED_OPERATIONS.get(&operation_sig) {
                        Some(out_sig) => out_sig,
                        None => panic!(
                            "propagate_types: unsupported operation ({:?})",
                            operation_sig
                        ),
                    };
                    Some(out_sig.output_type.clone())
                } else if let syn::Expr::Call(call_expr) = &*init.1 {
                    let function_sig = function_signature(call_expr, &map);
                    let func_out_type = SUPPORTED_FUNCTIONS
                        .get(&function_sig)
                        .expect("propagate_types: unsupported function");
                    // Sets result type
                    Some(func_out_type.output_type.clone())
                } else if let syn::Expr::MethodCall(method_expr) = &*init.1 {
                    let method_sig = method_signature(method_expr, &map);
                    // Searches for supported function signature by function identifier and argument types.
                    let method_out_type = SUPPORTED_METHODS
                        .get(&method_sig)
                        .expect("propagate_types: unsupported method");
                    // Sets result type
                    Some(method_out_type.output_type.clone())
                } else if let syn::Expr::Macro(macro_expr) = &*init.1 {
                    assert_eq!(
                        macro_expr.mac.path.segments[1].ident.to_string(),
                        "dup",
                        "Only rust_ad::dup! macro currently supported:\n{:#?}",
                        macro_expr
                    );
                    // eprintln!("macro_expr: {:#?}", macro_expr);
                    let macro_token_stream =
                        proc_macro::TokenStream::from(macro_expr.mac.tokens.clone())
                            .into_iter()
                            .collect::<Vec<_>>();
                    let dup_var = macro_token_stream[0]
                        .ident()
                        .expect("propagate_types: macro not ident")
                        .to_string();
                    let output_type = map
                        .get(&dup_var)
                        .expect("propagate_types: missing macro var")
                        .clone();
                    Some(output_type)
                } else {
                    None
                };
                // If found output type, then assign this type to all output values
                if let Some(output_type) = output_type_opt {
                    for var_ident in var_idents.into_iter() {
                        map.insert(var_ident, output_type.clone());
                    }
                }
            }
        }
    }
    // eprintln!("final map: {:?}", map);
    map
}
