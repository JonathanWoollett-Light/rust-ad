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

use crate::{der, wrt};

// Utils
// -------------------------------------------------------------------

/// Gets cumulative derivative for given expression for a given input variable (only supports literals and paths).
///
/// This is difficult to explain here.
///
/// In practical application we unwrap all statements such that `let e=2.*b+d;` becomes 2
///  statements `let _e=2.*b;` and `let e=_e+d;`, when we do this can optimize this
///  function, instead of needing to do `d/db(2.*b+d)` and `d/dd(2.*b+d)` we can
///  simply know in an addition if the component is a variable the deriative is `1.`
///  since `d/d_e(_e+d)` and `d/d_d(_e+d)` are both 1, thus we know the result
///  for an input `x` would be `1.*_e_x + 1.*d_x` simply `_e_x + d_x`.
///
/// In this optimization we apply this function t0o each component (e.g. `_e`, `d` etc.) seperately
///  with 4 possible results for each:
/// 1. Where the component is a literal (not a variable) it is simply `0.`,
/// 2. Where the component is not a function input, we get the cumulative deriative for this
///    variable with respect to our function input (e.g. `_e_x`).
/// 3. Where the component is an input and we looking at the cumulative derivative for this input it
///     is our seed input cumulative derivative e.g. `_x` since `1. * _x`.
/// 4. Where the component is an input, but we are not looking at the cumulative derivative for this
///     input, it is `0.` since we don't have cumulative deriatives for inputs with respect to each
///     other with `1. * x_wrt_y`, `x_wrt_y` doens't exist and we presume inputs independant.

pub enum Arg {
    /// e.g. `a`
    Variable(String),
    /// e.g. `7.3f32`
    Literal(String),
}
impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(s) => write!(f, "{}", s),
            Self::Literal(s) => write!(f, "{}", s),
        }
    }
}
impl TryFrom<&syn::Expr> for Arg {
    type Error = &'static str;
    fn try_from(expr: &syn::Expr) -> Result<Self, Self::Error> {
        match expr {
            syn::Expr::Lit(l) => match &l.lit {
                syn::Lit::Int(int) => Ok(Self::Literal(int.to_string())),
                syn::Lit::Float(float) => Ok(Self::Literal(float.to_string())),
                _ => Err("Unsupported literal type argument"),
            },
            syn::Expr::Path(p) => Ok(Self::Variable(p.path.segments[0].ident.to_string())),
            _ => Err("Non literal or path argument"),
        }
    }
}
/// Forward General Derivative type
pub type FgdType = fn(&[String], String, &[Arg]) -> syn::Stmt;
/// Derivative function type
pub type DFn = fn(&[Arg]) -> String;
/// Forward general deriviative
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
pub fn fgd<const DEFAULT: &'static str, const TRANSLATION_FUNCTIONS: &'static [DFn]>(
    outer_fn_args: &[String],
    local_ident: String,
    args: &[Arg],
) -> syn::Stmt {
    assert_eq!(args.len(), TRANSLATION_FUNCTIONS.len());

    // Gets vec of deriative idents and derivative functions
    let (idents, deriatives) = outer_fn_args
        .iter()
        .map(|outer_fn_input| {
            let acc = args
                .iter()
                .zip(TRANSLATION_FUNCTIONS.iter())
                .map(|(arg,t)|
                // See the docs for cumulative (these if's accomplish the same-ish thing)
                // TODO Improve docs here directly
                match arg {
                    Arg::Literal(_) => DEFAULT.to_string(),
                    Arg::Variable(v) => {
                        let (a,b) = (
                            t(args),
                        if v == outer_fn_input {
                            der!(outer_fn_input)
                        } else if outer_fn_args.contains(v) {
                            DEFAULT.to_string()
                        } else {
                            wrt!(arg,outer_fn_input)
                        });
                        // eprintln!("a: {}, b: {}",a,b);
                        format!("({})*{}",a,b)
                    }
                })
                .intersperse(String::from("+"))
                .collect::<String>();

            (wrt!(local_ident, outer_fn_input), acc)
        })
        .unzip::<_, _, Vec<_>, Vec<_>>();
    // eprintln!("idents: {:?}",idents);
    // eprintln!("deriatives: {:?}",deriatives);

    // Converts vec's to strings
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
    // eprintln!("idents: {}",idents);
    // eprintln!("deriatives: {}",deriatives);

    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    // eprintln!("stmt_str: {}",stmt_str);
    syn::parse_str(&stmt_str).expect("fgd: parse fail")
}
