// #![feature(log_syntax)]

mod utils;
use utils::*;

extern crate proc_macro;
use proc_macro::TokenStream;
// use syn::{parse_macro_input, DeriveInput};
use quote::quote;

#[proc_macro]
pub fn make_answer(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}

#[proc_macro_attribute]
pub fn return_as_is(_attr: TokenStream, item: TokenStream) -> TokenStream {
    eprintln!("\n\nitem: {:#?}\n\n", item);
    item
}

#[proc_macro_attribute]
pub fn return_as_is_syn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item as syn::Item);
    eprintln!("\n\nast: {:#?}\n\n", ast);

    let new = quote! { #ast };
    TokenStream::from(new)
}

#[proc_macro_attribute]
pub fn autodiff(_attr: TokenStream, item: TokenStream) -> TokenStream {
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
    eprintln!("\n\nblock:\n{:?}\n\n", block);

    let new_block_statements = block
        .stmts
        .iter()
        .flat_map(|statement| {
            let mut statements = vec![statement.clone()];
            // If the statement is local variable declaration (e.g. `let ...`).
            if let syn::Stmt::Local(local) = statement {
                eprintln!("\n.\n.\n.\n{:#?}", local);
                let local_ident = &local.pat.ident().ident;

                // If our statement has some initialization (e.g. `let a = 3;`).
                if let Some(init) = &local.init {
                    // If initialization is a binary expression (e.g. `let a = b + c;`).
                    if init.1.is_binary() {
                        // eprintln!("\n.\n.\n.\n{:#?}",bin_expr);
                        let mut i = 1;
                        while statements[0]
                            .local()
                            .init
                            .as_ref()
                            .unwrap()
                            .1
                            .binary()
                            .left
                            .is_binary()
                        {
                            let mut l_statement = statements[0].clone();
                            let lls = l_statement.local_mut();
                            // Sets identifier.
                            let ident = format!("{}{}", "_".repeat(i), local_ident.to_string());
                            lls.pat.ident_mut().ident = syn::Ident::new(&ident, local_ident.span());
                            // Sets initialization.
                            *lls.init.as_mut().unwrap().1 =
                                *(statements[0].local().init.as_ref().unwrap().1.binary().left)
                                    .clone();

                            let mut p = syn::punctuated::Punctuated::new();
                            p.push(syn::PathSegment {
                                ident: syn::Ident::new(&ident, local_ident.span()),
                                arguments: syn::PathArguments::None,
                            });
                            *statements[0]
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

                            statements.insert(0, l_statement);
                            i += 1;
                        }
                        // if let syn::Expr::Binary(bin_expr_right) = &*bin_expr.right {}
                    }
                }
            }
            statements
        })
        .collect::<Vec<_>>();

    block.stmts = new_block_statements;

    let new = quote! { #ast };
    TokenStream::from(new)
}
