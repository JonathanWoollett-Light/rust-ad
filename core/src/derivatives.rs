use crate::{traits::*, utils::*};

use crate::append_insert;
use crate::*;

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
/// Gets cumulative derivative for given expression for a given input variable (only supports literals and paths).
pub fn cumulative_derivative_wrt_rt(
    expr: &syn::Expr,
    input_var: &str,
    function_inputs: &[String],
    out_type: &Type,
) -> String {
    match expr {
        // Result 1
        syn::Expr::Lit(_) => out_type.zero(),
        syn::Expr::Path(path_expr) => {
            // x typically is the left or right of binary expression, regardless we are doing d/dx(expr) so at this we got
            let x = path_expr.path.segments[0].ident.to_string();

            // Result 3
            if x == input_var {
                der!(input_var)
            }
            // Result 4
            else if function_inputs.contains(&x) {
                out_type.zero()
            }
            // Result 2
            else {
                wrt!(x, input_var)
            }
        }
        _ => panic!("cumulative_derivative_wrt: unsupported expr"),
    }
}

#[derive(PartialEq, Eq)]
pub enum Type {
    F32,
    F64,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
}
impl Type {
    pub fn zero(&self) -> String {
        format!("0{}", self.to_string())
    }
}
impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::U128 => "u128",
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::I128 => "i128",
        }
        .into()
    }
}
impl TryFrom<&str> for Type {
    type Error = &'static str;
    fn try_from(string: &str) -> Result<Self, Self::Error> {
        match string {
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            "u128" => Ok(Self::U128),
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "i128" => Ok(Self::I128),
            _ => Err("Type::try_from unsupported type"),
        }
    }
}

/// Forward deriative of [std::ops::Add].
pub fn forward_add<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    let local = stmt.local().expect("forward_add: not local");
    let bin_expr = &local
        .init
        .as_ref()
        .expect("forward_add: no init")
        .1
        .binary()
        .expect("forward_add: not binary");
    let val_ident = local
        .pat
        .ident()
        .expect("forward_add: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
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
    let val_ident = local
        .pat
        .ident()
        .expect("forward_sub: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
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
    let val_ident = local
        .pat
        .ident()
        .expect("forward_mul: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
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
    let val_ident = local
        .pat
        .ident()
        .expect("forward_div: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
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
/// Forward deriative of [`powi`](https://doc.rust-lang.org/std/primitive.f32.html#method.powi).
pub fn forward_powi<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("forward_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_powi: not method");

    let val_ident = local
        .pat
        .ident()
        .expect("forward_powi: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let (base, exponent) = (
        expr_str(&*method_expr.receiver),
        expr_str(&method_expr.args[0]),
    );
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "{exponent} as {type_str} * {base}.powi({exponent} - 1i32)",
                    exponent = exponent,
                    base = base,
                    type_str = OUT.to_string()
                )
            } else if *input == exponent {
                format!(
                    "{base}.powi({exponent}) * {base}.ln()",
                    exponent = exponent,
                    base = base
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_powi: parse fail")
}
/// Forward deriative of [`powf`](https://doc.rust-lang.org/std/primitive.f32.html#method.powf).
pub fn forward_powf<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("forward_powf: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_powf: not method");

    let val_ident = local
        .pat
        .ident()
        .expect("forward_powf: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let (base, exponent) = (
        expr_str(&*method_expr.receiver),
        expr_str(&method_expr.args[0]),
    );
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "{exponent} * {base}.powf({exponent} - 1{type_str})",
                    exponent = exponent,
                    base = base,
                    type_str = OUT.to_string()
                )
            } else if *input == exponent {
                format!(
                    "{base}.powf({exponent}) * {base}.ln()",
                    exponent = exponent,
                    base = base
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_powf: parse fail")
}
/// Forward deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.sqrt).
pub fn forward_sqrt<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("forward_sqrt: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_sqrt: not method");

    let val_ident = local
        .pat
        .ident()
        .expect("forward_sqrt: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{type_str} / (2{type_str} * {base}.sqrt())",
                    base = base,
                    type_str = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_powf: parse fail")
}
/// Forward deriative of [`ln`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln).
pub fn forward_ln<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("forward_ln: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_ln: not method");

    let val_ident = local
        .pat
        .ident()
        .expect("forward_ln: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let base = expr_str(&*method_expr.receiver);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == base {
                format!(
                    "1{type_str} / {base}",
                    base = base,
                    type_str = OUT.to_string()
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_ln: parse fail")
}
/// Forward deriative of [`log`](https://doc.rust-lang.org/std/primitive.f32.html#method.log).
pub fn forward_log<const OUT: Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("forward_log: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_log: not method");

    let val_ident = local
        .pat
        .ident()
        .expect("forward_log: not ident")
        .ident
        .to_string();

    let idents = function_inputs
        .iter()
        .map(|input| wrt!(val_ident, input))
        .intersperse(String::from(","))
        .collect::<String>();

    let (log_input, base) = (
        expr_str(&*method_expr.receiver),
        expr_str(&method_expr.args[0]),
    );
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            if *input == log_input {
                format!(
                    "1{val_type} / ( {log_input} * {base}.ln() )",
                    log_input = log_input,
                    base = base,
                    val_type = OUT.to_string()
                )
            } else if *input == base {
                format!(
                    "-{log_input}.ln() / ( {base} * {base}.ln() * {base}.ln() )",
                    log_input = log_input,
                    base = base,
                )
            } else {
                OUT.zero()
            }
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_log: parse fail")
}

/// Reverse deriative of [std::ops::Add].
pub fn reverse_add<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_add: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_add: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_add: not ident")
        .ident
        .to_string();

    let (a, b): (String, String) = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&l, lis.clone(), component_map);
            append_insert(&r, lis.clone(), component_map);
            (wrt!(l, lis), wrt!(r, lis))
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => {
            let l = expr_path_l.path.segments[0].ident.to_string();
            append_insert(&l, lis.clone(), component_map);
            (wrt!(l, lis), String::from("_"))
        }
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => {
            let r = expr_path_r.path.segments[0].ident.to_string();
            append_insert(&r, lis.clone(), component_map);
            (String::from("_"), wrt!(r, lis))
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_add: Unsupported bin expr"),
    };
    let stmt_str = format!("let ({},{}) = ({},{});", a, b, der!(lis), der!(lis));
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_add: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [std::ops::Sub].
pub fn reverse_sub<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_sub: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_sub: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_sub: not ident")
        .ident
        .to_string();

    let (a, b): (String, String) = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&l, lis.clone(), component_map);
            append_insert(&r, lis.clone(), component_map);
            (wrt!(l, lis), wrt!(r, lis))
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => {
            let l = expr_path_l.path.segments[0].ident.to_string();
            append_insert(&l, lis.clone(), component_map);
            (wrt!(l, lis), String::from("_"))
        }
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => {
            let r = expr_path_r.path.segments[0].ident.to_string();
            append_insert(&r, lis.clone(), component_map);
            (String::from("_"), wrt!(r, lis))
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_sub: Unsupported bin expr"),
    };
    let stmt_str = format!("let ({},{}) = ({},-{});", a, b, der!(lis), der!(lis));
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sub: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [std::ops::Mul].
pub fn reverse_mul<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_mul: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_mul: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_mul: not ident")
        .ident
        .to_string();

    let stmt_str = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&l, lis.clone(), component_map);
            append_insert(&r, lis.clone(), component_map);
            format!(
                "let ({},{}) = ({}*{},{}*{});",
                wrt!(l, lis),
                wrt!(r, lis),
                r,
                der!(lis),
                l,
                der!(lis)
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            append_insert(&l, lis.clone(), component_map);
            format!("let {} = {}*{};", wrt!(l, lis), r, der!(lis))
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&r, lis.clone(), component_map);
            format!("let {} = {}*{};", wrt!(r, lis), l, der!(lis))
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_mul: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_mul: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [std::ops::Div].
pub fn reverse_div<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_div: not local");
    let init = local.init.as_ref().unwrap();
    let init_expr = &*init.1;
    let bin_expr = init_expr.binary().expect("reverse_div: not binary");
    let lis = local
        .pat
        .ident()
        .expect("reverse_div: not ident")
        .ident
        .to_string();

    let stmt_str = match (&*bin_expr.left, &*bin_expr.right) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (numerator, denominator) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&numerator, lis.clone(), component_map);
            append_insert(&denominator, lis.clone(), component_map);
            format!(
                "let ({},{}) = ({dx} * (1{}/{denominator}), {dx} * (-{numerator} / ({denominator}*{denominator})));",
                wrt!(numerator,lis),
                wrt!(denominator,lis),
                OUT.to_string(),
                numerator=numerator,
                denominator=denominator,
                dx = der!(lis),
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (numerator, denominator) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            append_insert(&numerator, lis.clone(), component_map);
            format!(
                "let {} = {} * (1{}/{});",
                wrt!(numerator, lis),
                der!(lis),
                OUT.to_string(),
                denominator
            )
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (numerator, denominator) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&denominator, lis.clone(), component_map);
            format!(
                "let {} = {} * (-{}/({}*{}));",
                wrt!(denominator, lis),
                der!(lis),
                numerator,
                denominator,
                denominator
            )
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_div: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_div: parse fail");
    Some(new_stmt)
}

/// Reverse deriative of [`powi`](https://doc.rust-lang.org/std/primitive.f32.html#method.powi).
pub fn reverse_powi<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_powi: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_powi: not ident")
        .ident
        .to_string();

    let (base, exponent) = (&*method_expr.receiver, &method_expr.args[0]);

    let stmt_str = match (base, exponent) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (base, exponent) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&base, lis.clone(), component_map);
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let ({},{}) = ({dx} * ({exponent} as {val_type} * {base}.powi({exponent}-1i32)), {dx} * ({base}.powi({exponent}) * {base}.ln() ) );",
                wrt!(base,lis),
                wrt!(exponent,lis),
                base = base,
                exponent = exponent,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (base, exponent) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {} * ({exponent} as {val_type} * {base}.powi({exponent}-1i32));",
                wrt!(base, lis),
                der!(lis),
                base = base,
                exponent = exponent,
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (base, exponent) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let {} = {} * ({base}.powi({exponent}) * {base}.ln() );",
                wrt!(exponent, lis),
                der!(lis),
                base = base,
                exponent = exponent,
            )
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_powi: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_powi: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`powf`](https://doc.rust-lang.org/std/primitive.f32.html#method.powf).
pub fn reverse_powf<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_powf: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_powf: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_powf: not ident")
        .ident
        .to_string();

    let (base, exponent) = (&*method_expr.receiver, &method_expr.args[0]);

    let stmt_str = match (base, exponent) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (base, exponent) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&base, lis.clone(), component_map);
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let ({},{}) = ({dx} * ({exponent} * {base}.powi({exponent}-1{val_type})), {dx} * ({base}.powi({exponent}) * {base}.ln() ) );",
                wrt!(base,lis),
                wrt!(exponent,lis),
                base = base,
                exponent = exponent,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (base, exponent) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {} * ({exponent} * {base}.powi({exponent}-1{val_type}));",
                wrt!(base, lis),
                der!(lis),
                base = base,
                exponent = exponent,
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (base, exponent) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let {} = {} * ({base}.powi({exponent}) * {base}.ln() );",
                wrt!(exponent, lis),
                der!(lis),
                base = base,
                exponent = exponent,
            )
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_powf: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_powf: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.sqrt).
pub fn reverse_sqrt<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_sqrt: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_sqrt: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_sqrt: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ( 2{val_type} * {base}.sqrt() ) );",
                wrt!(base,lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_sqrt: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sqrt: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`ln`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln).
pub fn reverse_ln<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_ln: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_ln: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_ln: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / {base} );",
                wrt!(base,lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_ln: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_ln: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`log`](https://doc.rust-lang.org/std/primitive.f32.html#method.log).
pub fn reverse_log<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_log: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_log: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_log: not ident")
        .ident
        .to_string();

    let (input, base) = (&*method_expr.receiver, &method_expr.args[0]);

    let stmt_str = match (input, base) {
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => {
            let (input, base) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&input, lis.clone(), component_map);
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let ({},{}) = {dx} * ( 1{val_type} / ( {input} * {base}.ln() )), {dx} * (-{input}.ln() / ( {base} * {base}.ln() * {base}.ln() ));",
                wrt!(input,lis),
                wrt!(base,lis),
                input = input,
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (input, base) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            append_insert(&input, lis.clone(), component_map);
            format!(
                "let {} = {} * ( 1{val_type} / ( {input} * {base}.ln() ));",
                wrt!(input, lis),
                der!(lis),
                base = base,
                input = input,
                val_type = OUT.to_string()
            )
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (input, base) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {} * (-{input}.ln() / ( {base} * {base}.ln() * {base}.ln() );",
                wrt!(base, lis),
                der!(lis),
                input = input,
                base = base,
            )
        },
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_log: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_log: parse fail");
    Some(new_stmt)
}