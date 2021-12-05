mod utils;
use utils::*;

extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn forward_autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);

    // Checks item is impl.
    let mut ast = match ast {
        syn::Item::Impl(implementation) => implementation,
        _ => panic!("Macro must be applied to `impl`."),
    };
    // Asserts this macro is applied to impl of `Forward` trait.
    assert_eq!(
        ast.trait_.as_ref().unwrap().1.segments[0].ident.to_string(),
        "Forward"
    );

    let items = &mut ast.items;
    let block = match &mut items[2] {
        syn::ImplItem::Method(m) => &mut m.block,
        _ => unreachable!(),
    };
    // eprintln!("\n\nblock:\n{:?}\n\n", block);

    let statements = block
        .stmts
        .iter()
        .flat_map(|statement| unwrap_statement(statement))
        .collect::<Vec<_>>();
    // block.stmts = statements;

    // let statements = statements
    //     .into_iter()
    //     .flat_map(|statement| append_derivative(statement))
    //     .collect::<Vec<_>>();

    block.stmts = statements;

    let new = quote::quote! { #ast };
    TokenStream::from(new)
}

// http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/
fn append_derivative(stmt: syn::Stmt) /* /-> [syn::Stmt;2] */ {
    if let syn::Stmt::Local(local) = stmt {
        if let Some(init) = local.init {
            if let syn::Expr::Binary(bin_expr) = *init.1 {
                // TODO Do the rest of the operations.
                let new_bin_expr = match bin_expr.op {
                    syn::BinOp::Add(_) => {
                        // x+7 -> d_x+0
                        // x+y -> d_x+d_y
                    },
                    syn::BinOp::Sub(_) => {
                        // x-7 -> d_x-0
                        // x-y -> d_x-d_y
                    },
                    syn::BinOp::Mul(_) => {
                    },
                    syn::BinOp::Div(_) => {

                    },
                    _ => panic!("Uncovered operation")
                };
            }
        }
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
