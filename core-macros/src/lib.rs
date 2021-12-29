//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use proc_macro::{TokenStream, TokenTree};

/// ```ignore
/// static outer_test: FgdType = {
///     const base_fn: DFn = |args:&[String]| -> String { format!("{0}-{1}",args[0],args[1]) };
///     const exponent_fn: DFn = |args:&[String]| -> String { format!("{0}*{1}+{0}",args[0],args[1]) };
///     fgd::<"0f32",{&[base_fn, exponent_fn]}>
/// };
/// ```
/// Is equivilant to
/// ```ignore
/// forward_derivative_macro!(outer_test,"{0}-{1}","{0}*{1}+{0}");
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
        "pub static {}: FgdType = {{\n{}\n\tfgd::<{{ {} }},{{ &[{}] }}>\n}};",
        name, der_functions, default, fn_fmt_str
    );
    // eprintln!("out_str: \n{}\n",out_str);
    out_str.parse().unwrap()
}

#[proc_macro]
pub fn compose(item: TokenStream) -> TokenStream {
    // eprintln!("\nitem:\n{:?}\n",item);
    let mut iter = item.into_iter();
    let fmt_str = match iter.next() {
        Some(TokenTree::Literal(l)) => l.to_string(),
        _ => panic!("No fmt str"),
    };
    let vec = iter.collect::<Vec<_>>();

    let components = vec
        .chunks_exact(2)
        .map(|item| {
            eprintln!("item: {:?}",item);
            let (punc, lit) = (&item[0], &item[1]);
            match (punc, lit) {
                (TokenTree::Punct(_), TokenTree::Literal(l)) => l.to_string(),
                (TokenTree::Punct(_), TokenTree::Ident(i)) => i.to_string(),
                _ => panic!("Bad component"),
            }
        })
        .collect::<Vec<_>>();

    let bstr = &fmt_str.as_bytes()[1..fmt_str.len() - 1];
    let mut inside = false;
    let mut i = 0;
    let mut j = 0;
    let mut out_str = String::from("let mut temp = String::new();");
    while i + 1 < bstr.len() {
        // eprintln!("i: {}",i);
        if inside {
            if bstr[i] == b'}' && bstr[i + 1] != b'}' {
                let index_str = String::from_utf8(bstr[j..i].to_vec()).expect("compose: utf8");
                // eprintln!("index_str: {}",index_str);
                let index: usize = index_str.parse().expect("compose: parse");

                if i != j {
                    out_str.push_str(&format!("\n\ttemp.push_str({});", components[index]));
                }

                j = i + 1;
                inside = false
            }
        } else {
            if bstr[i] == b'{' && bstr[i + 1] != b'{' {
                let segment = String::from_utf8(bstr[j..i].to_vec()).expect("compose: utf8");
                // eprintln!("segment: {}",segment);
                if i != j {
                    out_str.push_str(&format!("\n\ttemp.push_str(\"{}\");", segment));
                }

                j = i + 1;
                inside = true
            }
        }
        i += 1;
    }
    if inside {
        let index_str = String::from_utf8(bstr[j..bstr.len() - 1].to_vec()).expect("compose: utf8");
        // eprintln!("index_str: {}",index_str);
        let index: usize = index_str.parse().expect("compose: parse");
        if i != j {
            out_str.push_str(&format!("\n\ttemp.push_str({});", components[index]));
        }
    } else {
        let segment = String::from_utf8(bstr[j..].to_vec()).expect("compose: utf8");
        // eprintln!("segment: {}",segment);
        if i != j {
            out_str.push_str(&format!("\n\ttemp.push_str(\"{}\");", segment));
        }
    }
    // eprintln!("end: i: {}, j: {}",i,j);
    let out_str = format!("{{\n\t{}\n\ttemp\n}}", out_str);
    // eprintln!("out_str: \n{}\n",out_str);
    out_str.parse().unwrap()
}
