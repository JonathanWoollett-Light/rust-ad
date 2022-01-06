#![feature(proc_macro_span)]
#![feature(iter_intersperse)]
#![feature(proc_macro_diagnostic)]
#![feature(string_remove_matches)]

//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**
//!
//! External proc-macro functionality.

use quote::ToTokens;
use rust_ad_core::traits::*;
use rust_ad_core::*;

extern crate proc_macro;
use proc_macro::{Diagnostic, TokenStream};
use syn::spanned::Spanned;

use std::collections::HashMap;
#[cfg(not(debug_assertions))]
use std::collections::HashSet;

mod forward;
use forward::*;
mod reverse;
use reverse::*;

/// Calls forward auto-differentiation function corresponding to a given function.
///
/// ```
/// #[rust_ad::forward_autodiff]
/// fn multi(x: f32, y: f32) -> f32 {
///     let a = x.powi(2i32);
///     let b = x * 2f32;
///     let c = 2f32 / y;
///     let f = a + b + c;
///     return f;
/// }
/// fn main() {
///     let (f, der_x, der_y) = rust_ad::forward!(multi, 3f32, 5f32);
///     assert_eq!(f, 15.4f32);
///     assert_eq!(der_x, 8f32);
///     assert_eq!(der_y, -0.08f32);
/// }
/// ```
#[proc_macro]
pub fn forward(_item: TokenStream) -> TokenStream {
    let mut items = _item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let vec = items.collect::<Vec<_>>();
    let items = vec.chunks_exact(2);
    let inputs = items
        .map(|item| {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (proc_macro::TokenTree::Punct(_), proc_macro::TokenTree::Literal(num)) => {
                    format!("{},", num)
                }
                _ => panic!("forward: bad"),
            }
        })
        .collect::<Vec<String>>();
    let input_derivatives = "1f32,".repeat(inputs.len());
    let inputs_str = inputs.into_iter().collect::<String>();

    let call_str = format!(
        "{}{}({}{})",
        rust_ad_consts::FORWARD_PREFIX,
        function_ident,
        inputs_str,
        input_derivatives
    );
    call_str.parse().unwrap()
}
/// Calls reverse auto-differentiation function corresponding to a given function.
///
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn multi(x: f32, y: f32) -> f32 {
///     let a = x.powi(2i32);
///     let b = x * 2f32;
///     let c = 2f32 / y;
///     let f = a + b + c;
///     return f;
/// }
/// fn main() {
///     let (f, der_x, der_y) = rust_ad::reverse!(multi, 3f32, 5f32);
///     assert_eq!(f, 15.4f32);
///     assert_eq!(der_x, 8f32);
///     assert_eq!(der_y, -0.08f32);
/// }
/// ```
#[proc_macro]
pub fn reverse(_item: TokenStream) -> TokenStream {
    let mut items = _item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let vec = items.collect::<Vec<_>>();
    let items = vec.chunks_exact(2);
    let inputs = items
        .map(|item| {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (proc_macro::TokenTree::Punct(_), proc_macro::TokenTree::Literal(num)) => {
                    format!("{},", num)
                }
                _ => panic!("reverse: bad"),
            }
        })
        .collect::<String>();

    let call_str = format!(
        "{}{}({}1f32)",
        rust_ad_consts::REVERSE_PREFIX,
        function_ident,
        inputs
    );
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
/// ```
/// #[rust_ad::forward_autodiff]
/// fn multi(x: f32, y: f32) -> f32 {
///     let a = x.powi(2i32);
///     let b = x * 2f32;
///     let c = 2f32 / y;
///     let f = a + b + c;
///     return f;
/// }
/// fn main() {
///     let (f, der_x, der_y) = __for_multi(3f32, 5f32, 1f32, 1f32);
///     assert_eq!(f, 15.4f32);
///     assert_eq!(der_x, 8f32);
///     assert_eq!(der_y, -0.08f32);
/// }
/// ```
///
/// Much like a derive macro, this is appended to your code, the original `function_name` function remains unedited.
#[proc_macro_attribute]
pub fn forward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let start_item = item.clone();
    let ast = syn::parse_macro_input!(item as syn::Item);
    // eprintln!("{:#?}",ast);

    // Checks item is function.
    let mut function = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };

    // Updates function signature
    // ---------------------------------------------------------------------------
    let function_input_identifiers = {
        // Updates identifier.
        function.sig.ident = syn::Ident::new(
            &format!(
                "{}{}",
                rust_ad_consts::FORWARD_PREFIX,
                function.sig.ident.to_string()
            ),
            function.sig.ident.span(),
        );
        // Gets function inputs in usable form `[(ident,type)]`.
        let function_inputs = function
            .sig
            .inputs
            .iter()
            .map(|fn_arg| {
                let typed = fn_arg.typed().expect("forward: signature input not typed");
                let mut arg_type = typed.ty.to_token_stream().to_string();
                arg_type.remove_matches(" "); // Remove space separators in type

                let arg_ident = typed.pat.to_token_stream().to_string();
                (arg_ident, arg_type)
            })
            .collect::<Vec<_>>();
        // Put existing inputs into tuple.
        let inputs_tuple_str = format!(
            "({})",
            function_inputs
                .iter()
                .map(|(a, b)| format!("{}:{}", a, b))
                .intersperse(String::from(","))
                .collect::<String>()
        );
        let inputs_tuple =
            syn::parse_str(&inputs_tuple_str).expect("forward: inputs tuple parse fail");
        // Gets tuple of derivatives of inputs.
        let derivative_inputs_tuple_str = format!(
            "({})",
            function_inputs
                .iter()
                .map(|(ident, arg_type)| format!("{}:{}", der!(ident), arg_type))
                .intersperse(String::from(","))
                .collect::<String>()
        );
        let derivative_inputs_tuple = syn::parse_str(&derivative_inputs_tuple_str)
            .expect("forward: derivative inputs parse fail");
        // Sets new function inputs
        let mut new_fn_inputs = syn::punctuated::Punctuated::new();
        new_fn_inputs.push(inputs_tuple);
        new_fn_inputs.push(derivative_inputs_tuple);
        function.sig.inputs = new_fn_inputs;

        // Splits inputs vec into identifiers and types.
        let (idents, types) = function_inputs.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
        // Sets new function outputs
        update_function_outputs(&mut function.sig, types).expect("forward_autodiff 0");
        // Returns input identifiers
        idents
    };

    // Forward autodiff
    // ---------------------------------------------------------------------------

    // Flattens statements
    function.block.stmts = function
        .block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();

    // Propagates types through function
    let type_map = propagate_types(&function).expect("forward_autodiff 1");

    // In release we apply optimizations which shrink the produced code (eliminating unnecessary code)
    // These are not applied in debug mode so one might use debug to give a clearer view of the fundamental process.
    #[cfg(not(debug_assertions))]
    let mut non_zero_derivatives = HashSet::<String>::new();

    #[cfg(debug_assertions)]
    let der_info = (&type_map, function_input_identifiers.as_slice());
    #[cfg(not(debug_assertions))]
    let der_info = (
        &type_map,
        function_input_identifiers.as_slice(),
        &mut non_zero_derivatives,
    );

    // Intersperses forward derivatives
    let derivative_stmts =
        intersperse_succeeding_stmts(function.block.stmts, der_info, forward_derivative)
            .expect("forward_autodiff 2");
    function.block.stmts = derivative_stmts;
    // Updates return statement
    update_forward_return(
        &mut function.block,
        function_input_identifiers.as_slice(),
        #[cfg(not(debug_assertions))]
        type_map,
        #[cfg(not(debug_assertions))]
        non_zero_derivatives,
    )
    .expect("forward_autodiff 3");

    let new = quote::quote! { #function };
    let new_stream = TokenStream::from(new);
    join_streams(start_item, new_stream)
}
fn join_streams(mut a: TokenStream, b: TokenStream) -> TokenStream {
    a.extend(b.into_iter());
    a
}

fn update_function_outputs(
    function_signature: &mut syn::Signature,
    function_input_types: Vec<String>,
) -> Result<(), PassError> {
    let function_output = &mut function_signature.output;
    // Updates output to include to derivatives for each output respective to each input
    //  e.g. `fn(x:f32,x_:f32,y:f32,y_:f32)->(f32,f32)` => `fn(x:f32,y:f32)->((f32,(f32,f32)),(f32,(f32,f32)))`
    // eprintln!("function_output:\n{:#?}",function_output);
    let function_input_string = format!(
        "({}),",
        function_input_types
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>()
    );
    let return_type_str = match function_output {
        syn::ReturnType::Type(_, return_type) => match &**return_type {
            syn::Type::Path(return_path) => {
                let return_str = return_path.to_token_stream().to_string();
                format!("->({},{})", return_str, function_input_string)
            }
            syn::Type::Tuple(return_tuple) => {
                let return_str = return_tuple.to_token_stream().to_string();
                format!(
                    "->({},({}))",
                    return_str,
                    function_input_string.repeat(return_tuple.elems.len())
                )
            }
            _ => {
                let err = "Unsupported return type (supported types are tuples (e.g. `(f32,f32)`) or paths (e.g. `f32`))";
                Diagnostic::spanned(return_type.span().unwrap(), proc_macro::Level::Error, err)
                    .emit();
                return Err(err.to_string());
            }
        },
        // TODO What does this even look like?
        syn::ReturnType::Default => {
            let err = "Unsupported return form";
            Diagnostic::spanned(
                function_output.span().unwrap(),
                proc_macro::Level::Error,
                err,
            )
            .emit();
            return Err(err.to_string());
        }
    };
    // eprintln!("return_type_str: {}",return_type_str);
    *function_output = pass!(
        syn::parse_str(&return_type_str),
        "forward: failed output parse"
    );
    Ok(())
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
/// ```
/// #[rust_ad::reverse_autodiff]
/// fn multi(x: f32, y: f32) -> f32 {
///     let a = x.powi(2i32);
///     let b = x * 2f32;
///     let c = 2f32 / y;
///     let f = a + b + c;
///     return f;
/// }
/// fn main() {
///     let (f, der_x, der_y) = __rev_multi(3f32, 5f32, 1f32);
///     assert_eq!(f, 15.4f32);
///     assert_eq!(der_x, 8f32);
///     assert_eq!(der_y, -0.08f32);
/// }
/// ```
///
/// Much like a derive macro, this is appended to your code, the original `function_name` function remains unedited.
#[proc_macro_attribute]
pub fn reverse_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let start_item = item.clone();
    let ast = syn::parse_macro_input!(item as syn::Item);
    // Checks item is function.
    let mut function = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };
    // Unwraps nested binary expressions
    // ---------------------------------------------------------------------------
    let statements = function
        .block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    function.block.stmts = statements;

    // eprintln!("function.block.stmts:");
    // for stmt in function.block.stmts.iter() {
    //     eprintln!("\t{}",stmt.to_token_stream().to_string());
    // }

    // Updates function signature
    // ---------------------------------------------------------------------------
    let (function_input_identifiers, number_of_return_elements) = {
        // Updates identifier.
        function.sig.ident = syn::Ident::new(
            &format!(
                "{}{}",
                rust_ad_consts::REVERSE_PREFIX,
                function.sig.ident.to_string()
            ),
            function.sig.ident.span(),
        );
        // Gets function inputs in usable form `[(ident,type)]`.
        let function_inputs = function
            .sig
            .inputs
            .iter()
            .map(|fn_arg| {
                let typed = fn_arg.typed().expect("forward: signature input not typed");
                let mut arg_type = typed.ty.to_token_stream().to_string();
                arg_type.remove_matches(" "); // Remove space separators in type

                let arg_ident = typed.pat.to_token_stream().to_string();
                (arg_ident, arg_type)
            })
            .collect::<Vec<_>>();
        let (input_idents, input_types) =
            function_inputs.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
        // Put existing inputs into tuple.
        // -----------------------------------------------
        let inputs_tuple_str = format!(
            "({}):({})",
            input_idents
                .iter()
                .cloned()
                .intersperse(String::from(","))
                .collect::<String>(),
            input_types
                .iter()
                .cloned()
                .intersperse(String::from(","))
                .collect::<String>()
        );
        let inputs_tuple =
            syn::parse_str(&inputs_tuple_str).expect("reverse: inputs tuple parse fail");
        // Gets tuple of derivatives of inputs.
        // -----------------------------------------------
        let (function_output, number_of_return_elements) = match &function.sig.output {
            syn::ReturnType::Type(_, return_type) => (
                return_type.to_token_stream().to_string(),
                match &**return_type {
                    syn::Type::Tuple(type_tuple) => type_tuple.elems.len(),
                    syn::Type::Path(_) => 1,
                    _ => {
                        let err = "Unsupported return type";
                        Diagnostic::spanned(
                            return_type.span().unwrap(),
                            proc_macro::Level::Error,
                            err,
                        )
                        .emit();
                        panic!("{}", err);
                    }
                },
            ),
            syn::ReturnType::Default => {
                let err = "Unsupported return form";
                Diagnostic::spanned(
                    function.sig.output.span().unwrap(),
                    proc_macro::Level::Error,
                    err,
                )
                .emit();
                panic!("{}", err);
            }
        };
        let derivative_input_tuple_str = format!(
            "({}):{}",
            (0..number_of_return_elements)
                .map(|i| rtn!(i))
                .intersperse(String::from(","))
                .collect::<String>(),
            function_output
        );

        let derivative_input_tuple =
            syn::parse_str(&derivative_input_tuple_str).expect("reverse: output tuple parse fail");
        // Sets new function inputs
        // -----------------------------------------------
        let mut new_fn_inputs = syn::punctuated::Punctuated::new();
        new_fn_inputs.push(inputs_tuple);
        new_fn_inputs.push(derivative_input_tuple);
        function.sig.inputs = new_fn_inputs;
        // Sets new function outputs
        update_function_outputs(&mut function.sig, input_types).expect("reverse_autodiff 0");
        (input_idents, number_of_return_elements)
    };

    // Propagates types through function
    // ---------------------------------------------------------------------------
    let type_map = propagate_types(&function).expect("propagate_types: ");
    // eprintln!("type_map: {:?}",type_map);

    // Generates reverse mode code
    // ---------------------------------------------------------------------------
    let mut component_map = vec![HashMap::new(); number_of_return_elements];

    let mut rev_iter = function.block.stmts.iter().rev().peekable();
    let mut reverse_derivative_stmts = Vec::new();

    // In release we apply optimizations which shrink the produced code (eliminating unnecessary code)
    // These are not applied in debug mode so one might use debug to give a clearer view of the fundamental process.
    #[cfg(not(debug_assertions))]
    let mut non_zero_derivatives = HashSet::<String>::new();

    // let new_return = rev_iter.peek()

    // TODO TODO LOOK HERE HERE HERE HERE  New version
    while let Some(next) = rev_iter.next() {
        #[cfg(not(debug_assertions))]
        reverse_derivative_stmts.push(
            reverse_derivative(
                next,
                &type_map,
                &mut component_map,
                &function_input_identifiers,
                &mut non_zero_derivatives,
            )
            .expect("der temp"),
        );
        #[cfg(debug_assertions)]
        reverse_derivative_stmts.push(
            reverse_derivative(
                next,
                &type_map,
                &mut component_map,
                &function_input_identifiers,
            )
            .expect("der temp"),
        );

        // If there is a statement before this one, accumulate derivative for this statement
        reverse_derivative_stmts.push(if let Some(before) = rev_iter.peek() {
            #[cfg(debug_assertions)]
            {
                reverse_accumulate_derivative(before, &component_map, &type_map).expect("acc temp")
            }
            #[cfg(not(debug_assertions))]
            {
                reverse_accumulate_derivative(before, &component_map, &mut non_zero_derivatives)
                    .expect("acc temp")
            }
        }
        // Else if no statement before then accumulate for function inputs
        else {
            #[cfg(debug_assertions)]
            {
                Some(reverse_accumulate_inputs(
                    &function_input_identifiers,
                    &component_map,
                    &type_map,
                ))
            }

            #[cfg(not(debug_assertions))]
            {
                Some(reverse_accumulate_inputs(
                    &function_input_identifiers,
                    &component_map,
                    &type_map,
                    &mut non_zero_derivatives,
                ))
            }
        })
    }
    let new_return = reverse_append_derivatives(
        function.block.stmts.pop().unwrap(),
        &function_input_identifiers,
    )
    .expect("rtn temp");

    let mut reverse_derivative_stmts = reverse_derivative_stmts
        .into_iter()
        .filter_map(|d| d)
        .collect::<Vec<_>>();

    function.block.stmts.append(&mut reverse_derivative_stmts);
    function.block.stmts.push(new_return);

    let new = quote::quote! { #function };
    let new_stream = TokenStream::from(new);
    join_streams(start_item, new_stream)
}

/// Unwraps nested expressions into separate variable assignments.
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
                // If left side is not

                // If left is not a literal or path
                if !(bin_expr.left.is_lit() || bin_expr.left.is_path()) {
                    // Creates new left statement.
                    let left_ident = format!("{}_", local_ident);
                    let new_stmt_str =
                        format!("let {} = {};", left_ident, bin_expr.left.to_token_stream());
                    let new_stmt: syn::Stmt =
                        syn::parse_str(&new_stmt_str).expect("unwrap: left bad parse");
                    // Recurse
                    statements.append(&mut unwrap_statement(&new_stmt));

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
                // If right is not a literal or path
                if !(bin_expr.right.is_lit() || bin_expr.right.is_path()) {
                    // eprintln!("this should trigger: {}",right_bin_expr.to_token_stream());
                    // Creates new right statement.
                    let right_ident = format!("{}_", local_ident);
                    let new_stmt_str = format!(
                        "let {} = {};",
                        right_ident,
                        bin_expr.right.to_token_stream()
                    );
                    let new_stmt: syn::Stmt =
                        syn::parse_str(&new_stmt_str).expect("unwrap: right bad parse");
                    // Recurse
                    statements.append(&mut unwrap_statement(&new_stmt));

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
                            format!("{}_{}", FUNCTION_PREFIX.repeat(i + 1), local_ident);
                        func_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: function not ident")
                            .ident =
                            syn::parse_str(&func_ident).expect("unwrap: function ident parse fail");
                        *func_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(arg_bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&func_stmt));

                        // Updates statement to contain reference to new variables
                        let arg_expr: syn::Expr =
                            syn::parse_str(&func_ident).expect("unwrap: function parse fail");
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
                        let mut receiver_stmt = stmt.clone();
                        let receiver_local = receiver_stmt
                            .local_mut()
                            .expect("unwrap: receiver statement not local");
                        let receiver_ident =
                            format!("{}_{}", RECEIVER_PREFIX, local_ident.to_string());
                        receiver_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: receiver not ident")
                            .ident = syn::parse_str(&receiver_ident)
                            .expect("unwrap: receiver ident parse fail");
                        *receiver_local.init.as_mut().unwrap().1 =
                            syn::Expr::Binary(bin_expr.clone());
                        // Recurse
                        statements.append(&mut unwrap_statement(&receiver_stmt));

                        // Updates statement to contain variable referencing new statement.
                        let receiver_expr: syn::Expr =
                            syn::parse_str(&receiver_ident).expect("unwrap: receiver parse fail");
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
                            format!("{}_{}", FUNCTION_PREFIX.repeat(i + 1), local_ident);
                        func_local
                            .pat
                            .ident_mut()
                            .expect("unwrap: method not ident")
                            .ident =
                            syn::parse_str(&func_ident).expect("unwrap: method ident parse fail");
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
            } else if let syn::Expr::Paren(paren_expr) = init.1.as_ref() {
                base_statement
                    .local_mut()
                    .expect("unwrap: 3a")
                    .init
                    .as_mut()
                    .unwrap()
                    .1 = paren_expr.expr.clone();
                statements.append(&mut unwrap_statement(&base_statement));
                // Skips adding base statement we already added.
                return statements;
            }
        }
    } else if let syn::Stmt::Semi(semi_expr, _) = stmt {
        if let syn::Expr::Return(rtn_expr) = semi_expr {
            if let Some(rtn) = &rtn_expr.expr {
                if let syn::Expr::Binary(_bin_expr) = &**rtn {
                    let new_ident = format!("_{}", RETURN_SUFFIX);
                    let new_stmt_str = format!("let {};", new_ident);
                    let mut new_stmt: syn::Stmt =
                        syn::parse_str(&new_stmt_str).expect("unwrap: return stmt parse fail");
                    let new_local = new_stmt
                        .local_mut()
                        .expect("unwrap: return statement not local");
                    new_local
                        .pat
                        .ident_mut()
                        .expect("unwrap: return not ident")
                        .ident =
                        syn::parse_str(&new_ident).expect("unwrap: return ident parse fail");

                    // TODO Create `eq_token` some better way.
                    let eq_token = syn::parse_str("=").expect("unwrap: fml this is dumb");

                    new_local.init = Some((eq_token, rtn.clone()));
                    // Recurse
                    statements.append(&mut unwrap_statement(&new_stmt));

                    // Updates statement to contain variable referencing new statement.
                    let new_rtn_str = format!("return {};", new_ident);
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
fn propagate_types(func: &syn::ItemFn) -> Result<HashMap<String, String>, PassError> {
    // Collects input tuples into initial `type_map`.
    let input_types = func.sig.inputs
        .iter()
        .map(|input| match input {
            syn::FnArg::Typed(pat_type) => match (&*pat_type.pat,&*pat_type.ty) {
                (syn::Pat::Path(path_ident),syn::Type::Path(path_type)) => {
                    let ident = path_ident.to_token_stream().to_string();
                    let mut type_str = path_type.to_token_stream().to_string();
                    type_str.remove_matches(" "); // Remove space separators in type
                    Ok(vec![(ident,type_str)])
                },
                (syn::Pat::Tuple(tuple_ident),syn::Type::Tuple(tuple_type)) => {
                    // eprintln!("tuple_ident: {}",tuple_ident.to_token_stream());
                    let input_types_vec = tuple_ident.elems.iter().zip(tuple_type.elems.iter()).map(|(i,t)| match i {
                        syn::Pat::Ident(ident) => {
                            let ident_str = ident.to_token_stream().to_string();
                            if ident_str == "_" {
                                Ok(None)
                            }
                            else {
                                let mut type_str = t.to_token_stream().to_string();
                                type_str.remove_matches(" "); // Remove space separators in type
                                Ok(Some((ident_str,type_str)))
                            }
                        }
                        _ => {
                            // eprintln!("ident i: {:#?}",i);
                            let err = "Non-ident tuple type. `return (a,b,)` is supported. `return (a,(b,c))` is not supported.";
                            Diagnostic::spanned(
                                input.span().unwrap(),
                                proc_macro::Level::Error,
                                err,
                            )
                            .emit();
                            Err(err)
                        }
                    }).collect::<Result<Vec<_>,_>>().expect("propagate_types: tuple input error");
                    let input_types_vec = input_types_vec.into_iter().filter_map(|e|e).collect::<Vec<_>>();
                    Ok(input_types_vec)
                }
                _ => {
                    let err = "Unsupported input type combination";
                    Diagnostic::spanned(
                        input.span().unwrap(),
                        proc_macro::Level::Error,
                        err,
                    )
                    .emit();
                    Err(err)
                }
            },
            syn::FnArg::Receiver(_) => {
                let err = "Unsupported input type";
                Diagnostic::spanned(
                    input.span().unwrap(),
                    proc_macro::Level::Error,
                    err,
                )
                .emit();
                Err(err)
            }
        })
        .collect::<Result<Vec<_>,_>>().expect("propagate_types: input types error");
    let mut type_map = input_types
        .into_iter()
        .flatten()
        .collect::<HashMap<String, String>>();
    eprintln!("type_map: {:?}", type_map);

    // Propagates types through statements
    for stmt in func.block.stmts.iter() {
        // eprintln!("type_map: {:?}", type_map);
        // eprintln!("stmt:\n{:#?}\n", stmt);
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
                let output_type = match expr_type(&*init.1, &type_map) {
                    Ok(res) => res,
                    Err(e) => return Err(e),
                };
                for var_ident in var_idents.into_iter() {
                    type_map.insert(var_ident, output_type.clone());
                }
            }
        }
    }
    // eprintln!("final map: {:?}", map);
    Ok(type_map)
}
