mod utils;
use utils::*;

extern crate proc_macro;
use proc_macro::TokenStream;

const DERIVATIVE_PREFIX: &'static str = "der_";

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

#[proc_macro_attribute]
pub fn forward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);

    // eprintln!("{:#?}",ast);

    // Checks item is impl.
    let mut ast = match ast {
        syn::Item::Fn(func) => func,
        _ => panic!("Macro must be applied to a `fn`"),
    };

    // eprintln!("sig: {:#?}",ast);
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
    let output: TokenStream = "->(f32,f32)".parse().unwrap();
    ast.sig.output = syn::parse_macro_input!(output as syn::ReturnType);

    let block = &mut ast.block;
    // eprintln!("\n\nblock:\n{:?}\n\n", block);

    let statements = block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    // block.stmts = statements;

    // for s in statements.iter() {
    //     append_derivative(s);
    // }
    let statements = statements
        .into_iter()
        .flat_map(|statement| append_derivative(statement))
        .collect::<Vec<_>>();

    block.stmts = statements;

    let new = quote::quote! { #ast };
    TokenStream::from(new)
}

// http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/
fn append_derivative(stmt: syn::Stmt) -> Vec<syn::Stmt> {
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
