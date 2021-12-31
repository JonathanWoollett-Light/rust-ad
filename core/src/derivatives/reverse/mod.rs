mod f32;
pub use self::f32::*;
mod f64;
pub use self::f64::*;

mod i8;
pub use self::i8::*;
mod i16;
pub use self::i16::*;
mod i32;
pub use self::i32::*;
mod i64;
pub use self::i64::*;
mod i128;
pub use self::i128::*;

mod u8;
pub use self::u8::*;
mod u16;
pub use self::u16::*;
mod u32;
pub use self::u32::*;
mod u64;
pub use self::u64::*;
mod u128;
pub use self::u128::*;

use crate::derivatives::{Arg, DFn};
use crate::{append_insert, der, wrt};
use std::collections::HashMap;

/// Reverse General Derivative type
pub type RgdType = fn(String, &[Arg], &mut HashMap<String, Vec<String>>) -> syn::Stmt;

/// Reverse General Derivative
pub fn rgd<const DEFAULT: &'static str, const TRANSLATION_FUNCTIONS: &'static [DFn]>(
    local_ident: String,
    args: &[Arg],
    component_map: &mut HashMap<String, Vec<String>>,
) -> syn::Stmt {
    assert_eq!(args.len(), TRANSLATION_FUNCTIONS.len());

    let (idents, deriatives) = args
        .iter()
        .zip(TRANSLATION_FUNCTIONS.iter())
        .filter_map(|(arg, t)| match arg {
            Arg::Variable(v) => Some((v, t)),
            Arg::Literal(_) => None,
        })
        .map(|(arg, t)| {
            let der_ident = wrt!(arg, local_ident);
            append_insert(arg, local_ident.clone(), component_map);

            let (derivative, accumulator) = (t(args), der!(local_ident));
            let full_der = format!("({})*{}", derivative, accumulator);
            (der_ident, full_der)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();

    let (idents, deriatives) = (
        idents
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>(),
        deriatives
            .into_iter()
            .intersperse(String::from(","))
            .collect::<String>(),
    );

    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    // eprintln!("stmt_str: {}", stmt_str);
    syn::parse_str(&stmt_str).expect("fgd: parse fail")
}
