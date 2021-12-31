//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use proc_macro::{TokenStream, TokenTree};
use rust_ad_consts::{INTERNAL_FORWARD_PREFIX, INTERNAL_REVERSE_PREFIX};

/// Effectively performs `forward_derivative_macro` and `reverse_derivative_macro` consecutively.
#[proc_macro]
pub fn combined_derivative_macro(item: TokenStream) -> TokenStream {
    // eprintln!("\nitem:\n{:?}\n",item);
    let mut iter = item.into_iter();
    let name = match iter.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("No function ident"),
    };
    let vec = iter.collect::<Vec<_>>();
    assert_eq!(vec.len() % 2, 0, "Bad punctuation");
    let num = (vec.len() - 1) / 2;
    let mut iter = vec.chunks_exact(2);

    let default = match iter.next() {
        Some(item) => {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(default)) => default,
                _ => panic!("Bad default value"),
            }
        }
        _ => panic!("No default value"),
    };

    let iter = iter.enumerate();
    let arg_fmt_str = (0..num)
        .map(|i| format!("args[{}],", i))
        .collect::<String>();

    let der_functions = iter
        .map(|(index, item)| {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(format_str)) => format!(
                    "\tconst f{}: DFn = |args: &[Arg]| -> String {{ compose!({},{}) }};\n",
                    index, format_str, arg_fmt_str
                ),
                _ => panic!("Bad format strings"),
            }
        })
        .collect::<String>();
    let fn_fmt_str = (0..num).map(|i| format!("f{},", i)).collect::<String>();

    let for_out_str = format!(
        "pub static {}{}: FgdType = {{\n{}\n\tfgd::<{{ {} }},{{ &[{}] }}>\n}};",
        INTERNAL_FORWARD_PREFIX, name, der_functions, default, fn_fmt_str
    );
    let rev_out_str = format!(
        "pub static {}{}: RgdType = {{\n{}\n\trgd::<{{ {} }},{{ &[{}] }}>\n}};",
        INTERNAL_REVERSE_PREFIX, name, der_functions, default, fn_fmt_str
    );
    let out_str = format!("{}\n{}", for_out_str, rev_out_str);
    // eprintln!("out_str: \n{}\n",out_str);
    out_str.parse().unwrap()
}

/// ```ignore
/// static outer_test: FgdType = {
///     const base_fn: DFn = |args:&[String]| -> String { format!("{0}-{1}",args[0],args[1]) };
///     const exponent_fn: DFn = |args:&[String]| -> String { format!("{0}*{1}+{0}",args[0],args[1]) };
///     fgd::<"0f32",{&[base_fn, exponent_fn]}>
/// };
/// ```
/// Is equivalent to
/// ```ignore
/// forward_derivative_macro!(outer_test,"0f32","{0}-{1}","{0}*{1}+{0}");
/// ```
#[proc_macro]
pub fn forward_derivative_macro(item: TokenStream) -> TokenStream {
    // eprintln!("\nitem:\n{:?}\n",item);
    let mut iter = item.into_iter();
    let name = match iter.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("No function ident"),
    };
    let vec = iter.collect::<Vec<_>>();
    assert_eq!(vec.len() % 2, 0, "Bad punctuation");
    let num = (vec.len() - 1) / 2;
    let mut iter = vec.chunks_exact(2);

    let default = match iter.next() {
        Some(item) => {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(default)) => default,
                _ => panic!("Bad default value"),
            }
        }
        _ => panic!("No default value"),
    };

    let iter = iter.enumerate();
    let arg_fmt_str = (0..num)
        .map(|i| format!("args[{}],", i))
        .collect::<String>();

    let der_functions = iter
        .map(|(index, item)| {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(format_str)) => format!(
                    "\tconst f{}: DFn = |args: &[Arg]| -> String {{ compose!({},{}) }};\n",
                    index, format_str, arg_fmt_str
                ),
                _ => panic!("Bad format strings"),
            }
        })
        .collect::<String>();
    let fn_fmt_str = (0..num).map(|i| format!("f{},", i)).collect::<String>();
    let out_str = format!(
        "pub static {}{}: FgdType = {{\n{}\n\tfgd::<{{ {} }},{{ &[{}] }}>\n}};",
        INTERNAL_FORWARD_PREFIX, name, der_functions, default, fn_fmt_str
    );
    // eprintln!("out_str: \n{}\n",out_str);
    out_str.parse().unwrap()
}

#[proc_macro]
pub fn reverse_derivative_macro(item: TokenStream) -> TokenStream {
    // eprintln!("\nitem:\n{:?}\n",item);
    let mut iter = item.into_iter();
    let name = match iter.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => panic!("No function ident"),
    };
    let vec = iter.collect::<Vec<_>>();
    assert_eq!(vec.len() % 2, 0, "Bad punctuation");
    let num = (vec.len() - 1) / 2;
    let mut iter = vec.chunks_exact(2);

    let default = match iter.next() {
        Some(item) => {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(default)) => default,
                _ => panic!("Bad default value"),
            }
        }
        _ => panic!("No default value"),
    };

    let iter = iter.enumerate();
    let arg_fmt_str = (0..num)
        .map(|i| format!("args[{}],", i))
        .collect::<String>();

    let der_functions = iter
        .map(|(index, item)| {
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(format_str)) => format!(
                    "\tconst f{}: DFn = |args: &[Arg]| -> String {{ compose!({},{}) }};\n",
                    index, format_str, arg_fmt_str
                ),
                _ => panic!("Bad format strings"),
            }
        })
        .collect::<String>();
    let fn_fmt_str = (0..num).map(|i| format!("f{},", i)).collect::<String>();
    let out_str = format!(
        "pub static {}{}: RgdType = {{\n{}\n\trgd::<{{ {} }},{{ &[{}] }}>\n}};",
        INTERNAL_REVERSE_PREFIX, name, der_functions, default, fn_fmt_str
    );
    // eprintln!("out_str: \n{}\n",out_str);
    out_str.parse().unwrap()
}

/// `format!()` but:
/// 1. only allows positional arguments e.g. `{0}`, `{1}`, etc.
/// 2. allows unused arguments.
#[proc_macro]
pub fn compose(item: TokenStream) -> TokenStream {
    // eprintln!("item: {}",item);
    let mut iter = item.into_iter();
    let fmt_str = match iter.next() {
        Some(TokenTree::Literal(l)) => l.to_string(),
        _ => panic!("No fmt str"),
    };
    let vec = iter.skip(1).collect::<Vec<_>>();
    let component_iter = vec.split(|t| match t {
        TokenTree::Punct(p) => p.as_char() == ',',
        _ => false,
    });
    let components = component_iter
        .map(|component_slice| {
            component_slice
                .iter()
                .map(|c| c.to_string())
                .collect::<String>()
        })
        .collect::<Vec<_>>();

    let mut bytes_string = Vec::from(&fmt_str.as_bytes()[1..fmt_str.len() - 1]);
    let mut i = 0;
    let mut out_str = String::from("let mut temp = String::new();");
    while i < bytes_string.len() {
        if bytes_string[i] == b'}' {
            // Removes opening '}'
            let index_str = String::from_utf8(bytes_string.drain(0..i).collect::<Vec<_>>())
                .expect("compose: utf8");
            let index: usize = index_str.parse().expect("compose: parse");
            out_str.push_str(&format!(
                "\n\ttemp.push_str(&{}.to_string());",
                components[index]
            ));
            // Removes'}'
            bytes_string.remove(0);
            i = 0;
        } else if bytes_string[i] == b'{' {
            let segment = String::from_utf8(bytes_string.drain(0..i).collect::<Vec<_>>())
                .expect("compose: utf8");
            out_str.push_str(&format!("\n\ttemp.push_str(\"{}\");", segment));
            // Removes '{'
            bytes_string.remove(0);
            i = 0;
        } else {
            i += 1;
        }
    }
    let segment = String::from_utf8(bytes_string).expect("compose: utf8");
    out_str.push_str(&format!("\n\ttemp.push_str(\"{}\");", segment));

    let out_str = format!("{{\n\t{}\n\ttemp\n}}", out_str);
    // eprintln!("out_str: {}",out_str);
    out_str.parse().unwrap()
}

/// Gets internal forward derivative function identifier
#[proc_macro]
pub fn f(item: TokenStream) -> TokenStream {
    let mut items = item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let call_str = format!("{}{}", INTERNAL_FORWARD_PREFIX, function_ident);
    call_str.parse().unwrap()
}
/// Gets internal reverse derivative function identifier
#[proc_macro]
pub fn r(item: TokenStream) -> TokenStream {
    let mut items = item.into_iter();
    let function_ident = match items.next() {
        Some(proc_macro::TokenTree::Ident(ident)) => ident,
        _ => panic!("Requires function identifier"),
    };
    let call_str = format!("{}{}", INTERNAL_REVERSE_PREFIX, function_ident);
    call_str.parse().unwrap()
}
