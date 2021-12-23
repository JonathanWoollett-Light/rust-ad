#![feature(proc_macro_span)]
#![feature(iter_intersperse)]

//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

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
/// E.g.
/// ```
/// #[rust_ad::forward_autodiff]
/// fn function_name(x: f32, y: f32) -> f32 {
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
            let typed = fn_arg.typed().expect("forward: signatre input not typed");
            let val_type = &typed
                .ty
                .path()
                .expect("forward: signature input not path")
                .path
                .segments[0]
                .ident;
            let ident = &typed
                .pat
                .ident()
                .expect("forward: signature input not ident")
                .ident;
            let string = format!("{}:{}", der!(ident), val_type);
            let arg: syn::FnArg = syn::parse_str(&string).expect("forward: failed input parse");
            arg
        })
        .collect::<Vec<_>>();
    for input in sig_inputs.into_iter() {
        function.sig.inputs.push(input);
    }
    // Outputs output signature
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
    let return_string = format!("->({},{})", return_type, return_type);
    function.sig.output = syn::parse_str(&return_string).expect("forward: failed output parse");

    // Forward autodiff
    // ---------------------------------------------------------------------------
    // eprintln!("\n\nblock:\n{:?}\n\n", block);

    function.block.stmts = function
        .block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    // function.block.stmts = statements;
    propagate_types(&function);

    // // Intersperses forward deriatives
    // function.block.stmts = interspese_succedding(function.block.stmts, forward_derivative);
    // // Updates return statement
    // update_forward_return(function.block.stmts.last_mut());

    let new = quote::quote! { #function_holder #function };
    TokenStream::from(new)
}

/// Returns a tuple of a given number of clones of a variable.
///
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
    let mut statements = Vec::new();

    // TODO Avoid this clone.
    let mut base_statement = stmt.clone();

    // If the statement is local variable declaration (e.g. `let ...`).
    if let syn::Stmt::Local(local) = stmt {
        let local_ident = &local
            .pat
            .ident()
            .expect("unwrap: statement not ident")
            .ident;
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
                    let left_ident = format!("_{}", local_ident.to_string());
                    left_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: left not ident")
                        .ident = syn::Ident::new(&left_ident, local_ident.span());
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
                    let right_ident = format!("{}_", local_ident.to_string());
                    right_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: right not ident")
                        .ident = syn::Ident::new(&right_ident, local_ident.span());
                    *right_local.init.as_mut().unwrap().1 =
                        syn::Expr::Binary(right_bin_expr.clone());
                    // Recurse
                    statements.append(&mut unwrap_statement(&right_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let right_expr: syn::Expr =
                        syn::parse_str(&right_ident).expect("unwrap: rightparse fail");
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
                            .ident = syn::Ident::new(&func_ident, local_ident.span());
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
                            .expect("unwrap: reciver statement not local");
                        let reciver_ident =
                            format!("{}_{}", RECEIVER_PREFIX, local_ident.to_string());
                        reciver_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: reciver not ident")
                            .ident = syn::Ident::new(&reciver_ident, local_ident.span());
                        *reciver_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&reciver_stmt));

                        // Updates statement to contain variable referencing new statement.
                        let receiver_expr: syn::Expr =
                            syn::parse_str(&reciver_ident).expect("unwrap: reciver parse fail");
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
                            .ident = syn::Ident::new(&func_ident, local_ident.span());
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
    statements.push(base_statement);
    eprintln!("statements.len(): {}", statements.len());
    statements
}

/// Gets the types of all variables in a function.
///
/// Propagates types through variables in a function from the input types.
///
/// Returns a hashmap of identifier->type.
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
            // eprintln!("local:\n{:#?}\n",local);
            let var_ident = match &local.pat {
                syn::Pat::Ident(pat_ident) => &pat_ident.ident,
                _ => panic!("propagate_types: var type"),
            };
            if let Some(init) = &local.init {
                if let syn::Expr::Binary(bin_expr) = &*init.1 {
                    // eprintln!("bin_expr: {:#?}",bin_expr);

                    // Gets left var
                    let left_type = if let syn::Expr::Path(left_path) = &*bin_expr.left {
                        let left_ident = &left_path.path.segments[0].ident;
                        let l_type = map
                            .get(&left_ident.to_string())
                            .expect("propagate_types: left no type")
                            .clone();
                        Some(l_type)
                    } else {
                        None
                    };
                    // Gets right var
                    let right_type = if let syn::Expr::Path(right_path) = &*bin_expr.right {
                        let right_ident = &right_path.path.segments[0].ident;
                        let r_type = map
                            .get(&right_ident.to_string())
                            .expect("propagate_types: right no type")
                            .clone();
                        Some(r_type)
                    } else {
                        None
                    };
                    // If both are variables with types, check these types are equal
                    if left_type.is_some() && right_type.is_some() {
                        assert_eq!(
                            left_type.as_ref().unwrap(),
                            right_type.as_ref().unwrap(),
                            "non-matching types"
                        );
                    }
                    // Sets result type
                    if let Some(out_type) = left_type.or(right_type) {
                        map.insert(var_ident.to_string(), out_type.clone());
                    }
                } 
                else if let syn::Expr::Call(call_expr) = &*init.1 {
                    // eprintln!("call_expr: {:#?}",call_expr);

                    // Gets function identifier
                    let func_ident = &call_expr
                        .func
                        .path()
                        .expect("propagate_types: func not path")
                        .path
                        .segments[0]
                        .ident;
                    let func_str = func_ident.to_string();
                    // Gets type of each argument
                    let arg_types = call_expr
                        .args
                        .iter()
                        .map(|p| {
                            let ident = &p
                                .path()
                                .expect("propagate_types: func input not path")
                                .path
                                .segments[0]
                                .ident;
                            let ident_str = ident.to_string();
                            // eprintln!("ident_str: {}",ident_str);
                            map.get(&ident_str)
                                .expect("propagate_types: unfound var")
                                .clone()
                        })
                        .collect::<Vec<_>>();
                    // Searches for supported function signature by function identifier and argument types.
                    eprintln!("func_str: {}", func_str);
                    eprintln!("arg_types: {:?}", arg_types);
                    let func_out_type = SUPPORTED_FUNCTIONS
                        .get(&func_str, &arg_types)
                        .expect("propagate_types: unsupported function");
                    // Sets result type
                    map.insert(var_ident.to_string(), func_out_type);
                }
                else if let syn::Expr::MethodCall(method_expr) = &*init.1 {
                    let method_ident = &method_expr.method;
                    let method_str = method_ident.to_string();

                    let receiver_str = &method_expr.receiver.path().expect("propagate_types: method receiver not path").path.segments[0].ident.to_string();
                    let receiver_type_str = map.get(receiver_str).expect("propagate_types: unfound receiver");
                    // Gets type of each argument
                    let arg_types = method_expr
                        .args
                        .iter()
                        .map(|p| {
                            let ident = &p
                                .path()
                                .expect("propagate_types: method input not path")
                                .path
                                .segments[0]
                                .ident;
                            let ident_str = ident.to_string();
                            // eprintln!("ident_str: {}",ident_str);
                            map.get(&ident_str)
                                .expect("propagate_types: unfound var")
                                .clone()
                        })
                        .collect::<Vec<_>>();
                    // eprintln!("func_str: {}", method_str);
                    // eprintln!("arg_types: {:?}", arg_types);

                    // Searches for supported function signature by function identifier and argument types.
                    let method_out_type = SUPPORTED_METHODS
                        .get(&method_str, receiver_type_str,&arg_types)
                        .expect("propagate_types: unsupported method");
                    // Sets result type
                    map.insert(var_ident.to_string(), method_out_type);
                }
            }
        }
    }
    eprintln!("final map: {:?}", map);
    map
}
