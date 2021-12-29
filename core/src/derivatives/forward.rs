use super::lm_identifiers;
use crate::*;
use crate::{traits::*, utils::*};
use rust_ad_core_macros::{forward_derivative_macro,compose};

extern crate proc_macro;

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
fn cumulative_derivative_wrt<const OUT_TYPE: Type>(
    expr: &syn::Expr,
    input_var: &str,
    function_inputs: &[String],
) -> String {
    match expr {
        // Result 1
        syn::Expr::Lit(_) => OUT_TYPE.zero(),
        syn::Expr::Path(path_expr) => {
            // x typically is the left or right of binary expression, regardless we are doing d/dx(expr) so at this we got
            let x = path_expr.path.segments[0].ident.to_string();

            // Result 3
            if x == input_var {
                der!(input_var)
            }
            // Result 4
            else if function_inputs.contains(&x) {
                OUT_TYPE.zero()
            }
            // Result 2
            else {
                wrt!(x, input_var)
            }
        }
        _ => panic!("cumulative_derivative_wrt: unsupported expr"),
    }
}

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

    let (idents, deriatives) = outer_fn_args
        .iter()
        .map(|outer_fn_input| {
            let acc = args
                .iter()
                .zip(TRANSLATION_FUNCTIONS.iter())
                .filter_map(|(arg,t)|
                // See the docs for cumulative (these if's accomplish the same-ish thing)
                // TODO Improve docs here directly
                match arg {
                    Arg::Literal(_) => None,
                    Arg::Variable(v) => Some(format!("({})*{}",
                        t(args),
                        if v == outer_fn_input {
                            der!(outer_fn_input)
                        } else if outer_fn_args.contains(v) {
                            DEFAULT.to_string()
                        } else {
                            wrt!(arg,outer_fn_input)
                        }
                    ))
                })
                .intersperse(String::from("+"))
                .collect::<String>();

            (wrt!(local_ident, outer_fn_input), acc)
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
    syn::parse_str(&stmt_str).expect("fgd: parse fail")
}

// Primitive procedures
// -------------------------------------------------------------------

// forward_derivative_macro!(add_f32,"0f32","1f32","1f32");

// Forward deriative of [std::ops::Add].
pub fn forward_add<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let local = stmt.local().expect("forward_add: not local");
    let bin_expr = &local
        .init
        .as_ref()
        .expect("forward_add: no init")
        .1
        .binary()
        .expect("forward_add: not binary");
    let local_ident = local
        .pat
        .ident()
        .expect("forward_add: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            format!(
                "{dx1}+{dx2}",
                dx1 = cumulative_derivative_wrt::<OUT>(&*bin_expr.left, input, function_inputs),
                dx2 = cumulative_derivative_wrt::<OUT>(&*bin_expr.right, input, function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_add: parse fail")
}

/// Forward deriative of [std::ops::Sub].
pub fn forward_sub<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let local = stmt.local().expect("forward_sub: not local");
    let bin_expr = &local
        .init
        .as_ref()
        .expect("forward_sub: no init")
        .1
        .binary()
        .expect("forward_sub: not binary");
    let local_ident = local
        .pat
        .ident()
        .expect("forward_sub: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            format!(
                "{dx1}-{dx2}",
                dx1 = cumulative_derivative_wrt::<OUT>(&*bin_expr.left, input, function_inputs),
                dx2 = cumulative_derivative_wrt::<OUT>(&*bin_expr.right, input, function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_sub: parse fail")
}
/// Forward deriative of [std::ops::Mul].
pub fn forward_mul<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let local = stmt.local().expect("forward_mul: not local");
    let bin_expr = &local
        .init
        .as_ref()
        .expect("forward_mul: no init")
        .1
        .binary()
        .expect("forward_mul: not binary");
    let local_ident = local
        .pat
        .ident()
        .expect("forward_mul: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            format!(
                "{x2}*{dx1}+{x1}*{dx2}",
                x1 = expr_str(l),
                x2 = expr_str(r),
                dx1 = cumulative_derivative_wrt::<OUT>(l, input, function_inputs),
                dx2 = cumulative_derivative_wrt::<OUT>(r, input, function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_add: parse fail")
}
/// Forward deriative of [std::ops::Div].
pub fn forward_div<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let local = stmt.local().expect("forward_div: not local");
    let bin_expr = &local
        .init
        .as_ref()
        .expect("forward_div: no init")
        .1
        .binary()
        .expect("forward_div: not binary");
    let local_ident = local
        .pat
        .ident()
        .expect("forward_div: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    // d/d(numerator) = 1/denominator, d/d(denominator) = -numerator/(denominator*denominator)
    // 1/denominator -numerator/(denominator*denominator) simplifies to
    let (numerator, denominator) = (&*bin_expr.left, &*bin_expr.right);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            format!(
                "{dx1} * (1{val_type} / {denominator}) + {dx2}* (-{numerator}/({denominator}*{denominator}))",
                val_type = OUT.to_string(),
                numerator = expr_str(numerator),
                denominator = expr_str(denominator),
                dx1 = cumulative_derivative_wrt::<OUT>(numerator, input, function_inputs),
                dx2 = cumulative_derivative_wrt::<OUT>(denominator, input, function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_div: parse fail")
}

// Exponent procedures
// -------------------------------------------------------------------

// Forward deriative of [`powi`](https://doc.rust-lang.org/std/primitive.f32.html#method.powi).
// forward_derivative_macro!(
//     powi_f32,
//     "0f32",
//     "{1} * {0}.powi({1} - 1i32)",
//     "{0}.powi({1}) * {0}.ln()"
// );
// // Forward deriative of [`powf`](https://doc.rust-lang.org/std/primitive.f32.html#method.powf)
// forward_derivative_macro!(
//     powf_f32,
//     "0f32",
//     "{1} * {0}.powf({1} - 1f32)",
//     "{0}.powf({1}) * {0}.ln()"
// );
// // Forward deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.sqrt).
// forward_derivative_macro!(sqrt_f32, "0f32", "1f32 / (2f32 * {0}.sqrt())");
// // Forward deriative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.cbrt).
// forward_derivative_macro!(cbrt_f32, "0f32", "1f32 / (3f32*{0}.powf(2f32/3f32))");
// // Forward deriative of [`exp`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp).
// forward_derivative_macro!(exp_f32, "0f32", "{0}.exp()");
// // Forward deriative of [`exp2`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp2).
// forward_derivative_macro!(exp2_f32, "0f32", "{0}.exp2() * (2f32).ln()");
// // Forward deriative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp_m1).
// forward_derivative_macro!(exp_m1_f32, "0f32", "{0}.exp()");

// // Log procedures
// // -------------------------------------------------------------------

// // Forward deriative of [`ln`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln).
// forward_derivative_macro!(ln_f32, "0f32", "1f32 / {0}");
// // Forward deriative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln_1p).
// forward_derivative_macro!(ln_1p_f32, "0f32", "1f32 / (1f32+{0})");
// // Forward deriative of [`log`](https://doc.rust-lang.org/std/primitive.f32.html#method.log).
// forward_derivative_macro!(
//     log_f32,
//     "0f32",
//     "1f32 / ({0}*{1}.ln())",
//     "-{0}.ln() / ({1} *{1}.ln()*{1}.ln())"
// );
// // Forward deriative of [`log10`](https://doc.rust-lang.org/std/primitive.f32.html#method.log10).
// forward_derivative_macro!(log10_f32, "0f32", "1f32 / ({0}*(10f32).ln())");
// // Forward deriative of [`log2`](https://doc.rust-lang.org/std/primitive.f32.html#method.log2).
// forward_derivative_macro!(log2_f32, "0f32", "1f32 / ({0}*(2f32).ln())");

// // Trig procedures
// // -------------------------------------------------------------------

// // Forward deriative of [`acos`](https://doc.rust-lang.org/std/primitive.f32.html#method.acos).
// forward_derivative_macro!(acos_f32, "0f32", "-1f32 / (1f32-{0}*{0}).sqrt())");
// // Forward deriative of [`acosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.acosh).
// forward_derivative_macro!(
//     acosh_f32,
//     "0f32",
//     "1f32 / ( ({0}-1f32).sqrt() * ({0}+1f32).sqrt() )"
// );
/// Forward deriative of [`asin`](https://doc.rust-lang.org/std/primitive.f32.html#method.asin).
pub fn forward_asin<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / (1{val_type}-{base}*{base}).sqrt()",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_asin: parse fail")
}
/// Forward deriative of [`asinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.asinh).
pub fn forward_asinh<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / ({base}*{base}+1{val_type}).sqrt()",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_asinh: parse fail")
}
/// Forward deriative of [`atan`](https://doc.rust-lang.org/std/primitive.f32.html#method.atan).
pub fn forward_atan<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / ({base}*{base}+1{val_type})",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_atan: parse fail")
}
/// Forward deriative of [`sin`](https://doc.rust-lang.org/std/primitive.f32.html#method.sin).
pub fn forward_sin<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("{base}.cos()", base = base,)
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_sin: parse fail")
}
/// Forward deriative of [`atanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.atanh).
pub fn forward_atanh<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / (1{val_type}-{base}*{base})",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_atanh: parse fail")
}
/// Forward deriative of [`cos`](https://doc.rust-lang.org/std/primitive.f32.html#method.cos).
pub fn forward_cos<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("-{base}.sin()", base = base,)
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_cos: parse fail")
}
/// Forward deriative of [`cosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.cosh).
pub fn forward_cosh<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("{base}.sinh()", base = base,)
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_cosh: parse fail")
}
/// Forward deriative of [`sinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.sinh).
pub fn forward_sinh<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("{base}.cosh()", base = base,)
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_sinh: parse fail")
}
/// Forward deriative of [`tan`](https://doc.rust-lang.org/std/primitive.f32.html#method.tan).
pub fn forward_tan<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / ({base}.cos() * {base}.cos())",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_tan: parse fail")
}
/// Forward deriative of [`tanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.tanh).
pub fn forward_tanh<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{val_type} / ({base}.cosh()*{base}.cosh())",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_tanh: parse fail")
}

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f32.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f32.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

/// Forward deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.sqrt).
pub fn forward_abs<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            // If base>0 then as it rises, so does local_ident, thus 1 derivative, inversly, if base<0, then -1 deriative
            // x/x.abs() == if x >= 0 { 1 } else { -1 }
            if *input == base {
                format!("{base} / {base}.abs()", base = base,)
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_abs: parse fail")
}

// TODO Is this derivative for `ceil` right?
/// Forward deriative of [`ceil`](https://doc.rust-lang.org/std/primitive.f32.html#method.ceil).
pub fn forward_ceil<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("1{val_type}", val_type = OUT.to_string())
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_ceil: parse fail")
}

// TODO Is this derivative for `floor` right?
/// Forward deriative of [`floor`](https://doc.rust-lang.org/std/primitive.f32.html#method.floor).
pub fn forward_floor<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("1{val_type}", val_type = OUT.to_string())
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_floor: parse fail")
}

// TODO Is this derivative for `fract` right?
/// Forward deriative of [`fract`](https://doc.rust-lang.org/std/primitive.f32.html#method.fract).
pub fn forward_fract<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("1{val_type}", val_type = OUT.to_string())
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_fract: parse fail")
}

// TODO Is this derivative for `recip` right?
/// Forward deriative of [`recip`](https://doc.rust-lang.org/std/primitive.f32.html#method.recip).
pub fn forward_recip<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "-1{val_type} / ({base}*{base})",
                    base = base,
                    val_type = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_recip: parse fail")
}
// TODO Is this derivative for `round` right?
/// Forward deriative of [`round`](https://doc.rust-lang.org/std/primitive.f32.html#method.round).
pub fn forward_round<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let (local_ident, method_expr) = lm_identifiers(&stmt);

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(local_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!("1{val_type}", val_type = OUT.to_string())
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_round: parse fail")
}

// TODO Add some of these procedures here:
// - clamp https://doc.rust-lang.org/std/primitive.f32.html#method.clamp
// - div_eculid https://doc.rust-lang.org/std/primitive.f32.html#method.div_euclid
// - hypot https://doc.rust-lang.org/std/primitive.f32.html#method.hypot
// - mul_add https://doc.rust-lang.org/std/primitive.f32.html#method.mul_add
// - signum https://doc.rust-lang.org/std/primitive.f32.html#method.signum
// - rem_euclid https://doc.rust-lang.org/std/primitive.f32.html#method.rem_euclid
// - to_degrees https://doc.rust-lang.org/std/primitive.f32.html#method.to_degrees
// - to_radians https://doc.rust-lang.org/std/primitive.f32.html#method.to_radians
// - trunc https://doc.rust-lang.org/std/primitive.f32.html#method.trunc
