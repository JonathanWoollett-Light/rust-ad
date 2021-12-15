//! A super restrictive rough WIP beginnings of a library attempting to implement auto-differentiation in Rust.
//! ## Status
//! This library is super WIP and thus extremely rough, temperamental and inconsistent.
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

/// Flattens nested binary expressions into separate variable assignments.
/// A basic example:
/// ```
/// #[rad::unweave]
/// fn forward((x, y): (f32, f32)) -> f32 {
///     let v = 2. * x + y / 3.;
///     return v;
/// }
/// ```
/// Produces:
/// ```
/// fn forward((x, y): (f32, f32)) -> f32 {
///     let _v = 2. * x;
///     let v_ = y / 3.;
///     let v = _v + v_
///     return v;
/// }
/// ```
/// A more complex example:
/// ```
/// #[rad::unweave]
/// fn forward((x, y): (f32, f32)) -> f32 {
///     let p = 7. * x;
///     let r = 10. - y;
///     let q = p * x * 5.;
///     let v = 2. * p * q + 3. * r;
///     return v;
/// }
/// ```
/// Produces:
/// ```
/// fn forward((x, y): (f32, f32)) -> f32 {
///     let p = 7. * x;
///     let r = 10. - y;
///     let _q = p * x;
///     let q = _q * 5.;
///     let __v = 2. * p;
///     let _v = __v * q;
///     let v_ = 3. * r;
///     let v = _v + v_;
///     return v;
/// }
/// ```
/// It may be worth adding `#[allow(non_snake_case)]` as many of the intermediate variables the macro sets will cause this warning.
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

/// Transforms a given function into a form for forward auto-differentiation.
///
/// At the moment this is super restrictive:
/// - The function must take a tuple of `f32`s as input and output a `f32` like `fn fn_name((x,y,):(f32,f32,)) -> f32`.
/// - It only works with the primitive operations `-`, `+`, `*`, and `/`.
/// ```
/// #[rad::forward_autodiff]
/// fn forward((x, y): (f32, f32)) -> f32 {
///     let p = 7. * x;
///     let r = 10. - y;
///     let q = p * x * 5.;
///     let v = 2. * p * q + 3. * r;
///     return v;
/// }
/// ```
/// Produces:
/// ```
/// fn forward((x, y): (f32, f32), (der_x, der_y): (f32, f32)) -> (f32, f32) {
///     let p = 7. * x;
///     let der_p = x * 0f32 + 7. * der_x;
///     let r = 10. - y;
///     let der_r = 0f32 - der_y;
///     let _q = p * x;
///     let der__q = x * der_p + p * der_x;
///     let q = _q * 5.;
///     let der_q = 5. * der__q + _q * 0f32;
///     let __v = 2. * p;
///     let der___v = p * 0f32 + 2. * der_p;
///     let _v = __v * q;
///     let der__v = q * der___v + __v * der_q;
///     let v_ = 3. * r;
///     let der_v_ = r * 0f32 + 3. * der_r;
///     let v = _v + v_;
///     let der_v = der__v + der_v_;
///     return (v, der_v);
/// }
/// ```
/// It may be worth adding `#[allow(non_snake_case)]` as many of the intermediate variables the macro sets will cause this warning.
#[proc_macro_attribute]
pub fn forward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);

    // eprintln!("{:#?}",ast);

    // Checks item is function.
    let mut ast = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };

    // eprintln!("sig: {:#?}",ast);
    // Appends derivative inputs to function signature, `f((x,y))` -> `f((x,y),(dx,dy))`
    let first_input_span = ast.sig.inputs[0].typed().pat.tuple().elems[0]
        .ident()
        .ident
        .span();
    ast.sig.inputs.push(syn::FnArg::Typed(syn::PatType {
        attrs: Vec::new(),
        pat: Box::new({
            let mut pat = (*ast.sig.inputs[0].typed().pat).clone();
            for (input, deriv) in ast.sig.inputs[0]
                .typed()
                .pat
                .tuple()
                .elems
                .iter()
                .zip(pat.tuple_mut().elems.iter_mut())
            {
                deriv.ident_mut().ident = syn::Ident::new(
                    &format!("{}{}", DERIVATIVE_PREFIX, input.ident().ident),
                    input.ident().ident.span(),
                );
            }
            pat
        }),
        colon_token: syn::token::Colon {
            spans: [first_input_span],
        },
        ty: ast.sig.inputs[0].typed().ty.clone(),
    }));
    ast.sig.output = syn::parse_str(&"->(f32,f32)").unwrap();

    let block = &mut ast.block;
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

    let new = quote::quote! { # };
    TokenStream::from(new)
}

/// Given a variable and a number of times to repeat returns a tuple of clones of this variable. `dup!(x,3)` -> `(x.clone(),x.clone(),x.clone())`.
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

#[proc_macro_attribute]
pub fn backward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);
    // Checks item is function.
    let mut function = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Only `fn` items are supported."),
    };

    // Add input derivatives to output signature.
    // ---------------------------------------------------------------------------
    match &mut function.sig.output {
        syn::ReturnType::Type(_, ref mut return_type_type) => {
            // eprintln!("return_type_type: {:#?}", return_type_type);
            let inputs = function.sig.inputs.len();
            let output = format!("(f32,({}))", "f32,".repeat(inputs));
            let new_rtn: syn::Type = syn::parse_str(&output).unwrap();
            *return_type_type = Box::new(new_rtn);
        }
        syn::ReturnType::Default => {
            let a = quote::quote_spanned! {
                function.sig.span() => compile_error!("Expected return type `f32`");
            };
            return TokenStream::from(a);
        }
    }

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
    let mut num_used_inputs = 0;
    // `.rev()` is unnecessary here, but it declare dupes of inputs in order that inputs are declared, so it's nice.
    for input in function.sig.inputs.iter().rev() {
        match input {
            syn::FnArg::Typed(t) => {
                match &*t.pat {
                    syn::Pat::Ident(i) => {
                        let ident_str = format!("{}", i.ident);
                        if let Some(count) = counts.remove(&ident_str) {
                            num_used_inputs += 1;
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
                            function.block.stmts.insert(0, new_stmt);
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
    for stmt in function.block.stmts.iter().skip(num_used_inputs) {
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
    function.block.stmts = dup_stmts;

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

    let new = quote::quote! { #function };
    let rtn = TokenStream::from(new);
    eprintln!("rtn:\n{}", rtn);
    rtn
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
        if let Some(init) = &local.init {
            let init_expr = &*init.1;
            if let syn::Expr::Binary(bin_expr) = init_expr {
            } else if let syn::Expr::Macro(macro_expr) = init_expr {
                eprintln!("else: {:#?}", macro_expr);
                let macro_token_stream =
                    proc_macro::TokenStream::from(macro_expr.mac.tokens.clone())
                        .into_iter()
                        .collect::<Vec<_>>();
                let ident = macro_token_stream[0].ident().to_string();
                let num: usize = macro_token_stream[2].literal().to_string().parse().unwrap();
                let token_str = format!(
                    "let der_{} = {};",
                    ident,
                    (0..num)
                        .map(|c| format!("der_{}{}", ident, c))
                        .intersperse(String::from("+"))
                        .collect::<String>()
                );
                let der_dup_stmt: syn::Stmt = syn::parse_str(&token_str).unwrap();
                eprintln!("der_dup_stmt: {:?}", der_dup_stmt);
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
                            &format!("{}{}", DERIVATIVE_PREFIX, local.pat.ident().ident),
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
                                                        &format!(
                                                            "{}{}",
                                                            DERIVATIVE_PREFIX,
                                                            expr_path.path.segments[0].ident
                                                        ),
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
                    // expr_path.path.segments.push(syn::PathSegment {
                    //     ident: syn::Ident::new(
                    //         &format!("{}{}",DERIVATIVE_PREFIX,expr_path.path.segments[0].ident),
                    //         expr_path.path.segments[0].ident.span()
                    //     ),
                    //     arguments: syn::PathArguments::None
                    // });
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
                            &format!("{}{}", DERIVATIVE_PREFIX, expr_path.path.segments[0].ident),
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
