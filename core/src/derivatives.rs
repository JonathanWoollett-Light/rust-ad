use crate::{traits::*, utils::*};

use crate::*;

/// Gets cumulative derivative for given expression for a given input variable (only supports literals and paths).
///
/// This is difficult to explain here.
/// TODO A good explanation.
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
fn cumulative_derivative_wrt<const OUT_TYPE:Type>(
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
    out_type: &Type
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
    F32,F64,U8,U16,U32,U64,U128,I8,I16,I32,I64,I128
}
impl Type {
    pub fn zero(&self) -> String {
        format!("0{}",self.to_string())
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
            Self::I128 => "i128"
        }.into()
    }
}
impl TryFrom<&str> for Type {
    type Error = &'static str;
    fn try_from(string:&str) -> Result<Self,Self::Error> {
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
            _ => Err("Type::try_from unsupported type")
        }
    }
}
/// ```ignore
/// y = add (x1, x2)
/// dy = add (dx1, dx2)
/// ```
pub fn forward_add<const OUT:Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
/// ```ignore
/// y = sub (x1, x2)
/// dy = sub (dx1, dx2)
/// ```
pub fn forward_sub<const OUT:Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
/// ```ignore
/// y = mul (x1, x2)
/// dy = add (mul (x2, dx1), mul (x1, dx2))
/// ```
pub fn forward_mul<const OUT:Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
/// ```ignore
/// y = div (x1, x2)
/// dy = add (div (dx1, x2), negate (mul (div (x1, mul (x2, x2)), dx2)))
/// ```
/// Simpler:
/// ```ignore
/// dy = dx1*x2 - dx2*x2*x2/x1
/// ```
pub fn forward_div<const OUT:Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let deriatives = function_inputs
        .iter()
        .map(|input| {
            format!(
                "{dx1}*{x2} - {dx2}*{x2}*{x2}/{x1}",
                x1 = expr_str(l),
                x2 = expr_str(r),
                dx1 = cumulative_derivative_wrt::<OUT>(l, input, function_inputs),
                dx2 = cumulative_derivative_wrt::<OUT>(r, input, function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_div: parse fail")
}
// TODO Can we reduce/Remove code duplication between `forward_powi_f64` and `forward_powi_f32`?
/// Deriative of f32::powi(i32) operation.
pub fn forward_powi<const OUT:Type>(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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

pub fn reverse_powi_f32(stmt: &syn::Stmt) -> syn::Stmt {
    reverse_powi(stmt, "f32")
}
pub fn reverse_powi_f64(stmt: &syn::Stmt) -> syn::Stmt {
    reverse_powi(stmt, "f64")
}
// TODO Is this wrong? (I feel like it is)
/// ```ignore
/// a = x^y
/// dx, dy = y*x^(y-1), x^y*ln(x)
/// ```
fn reverse_powi(stmt: &syn::Stmt, float: &'static str) -> syn::Stmt {
    assert!(float == "f32" || float == "f64");
    let local = stmt.local().expect("reverse_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_powi: not method");

    let receiver_ident = method_expr
        .receiver
        .path()
        .expect("reverse_powi: not path")
        .path
        .segments[0]
        .ident
        .to_string();
    let exponent = expr_str(&method_expr.args[0]);

    let new_str = format!(
        "let ({},{}) = ({exponent} as {float}*{base}.powi({exponent}-1i32),{base}.powi({exponent})*{base}.ln());",
        der!(receiver_ident),
        der!(exponent),
        float = float,
        base=receiver_ident,
        exponent=exponent,
    );
    let new_stmt = syn::parse_str(&new_str).expect("reverse_powi: parse fail");
    new_stmt
}

pub fn reverse_add(stmt: &syn::Stmt) -> syn::Stmt {
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
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            der!(expr_path_r.path.segments[0].ident.to_string()),
        ),
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            String::from("_"),
        ),
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => (
            String::from("_"),
            der!(expr_path_r.path.segments[0].ident.to_string()),
        ),
        _ => panic!("reverse_add: Uncovered `syn::BinOp::Add(_)` binary expression combination"),
    };
    let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, der!(lis));
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_add: parse fail");
    new_stmt
}
pub fn reverse_sub(stmt: &syn::Stmt) -> syn::Stmt {
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
        (syn::Expr::Path(expr_path_l), syn::Expr::Path(expr_path_r)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            format!("-{}", der!(expr_path_r.path.segments[0].ident.to_string())),
        ),
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(_)) => (
            der!(expr_path_l.path.segments[0].ident.to_string()),
            String::from("_"),
        ),
        (syn::Expr::Lit(_), syn::Expr::Path(expr_path_r)) => (
            String::from("_"),
            format!("-{}", der!(expr_path_r.path.segments[0].ident.to_string())),
        ),
        _ => panic!("reverse_sub: Uncovered `syn::BinOp::Sub(_)` binary expression combination"),
    };
    let stmt_str = format!("let ({},{}) = rust_ad::dup!({},2);", a, b, lis);
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sub: parse fail");
    new_stmt
}
pub fn reverse_mul(stmt: &syn::Stmt) -> syn::Stmt {
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
            format!(
                "let ({},{}) = ({}*{},{}*{});",
                der!(l),
                der!(r),
                r,
                lis,
                l,
                lis
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                lit_str(expr_lit_r),
            );
            format!("let {} = {}*{};", der!(l), r, lis)
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                lit_str(expr_lit_l),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!("let {} = {}*{};", der!(r), l, lis)
        }
        _ => panic!("reverse_mul: Uncovered `syn::BinOp::Mul(_)` binary expression combination"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_mul: parse fail");
    new_stmt
}
pub fn reverse_div(stmt: &syn::Stmt) -> syn::Stmt {
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
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!(
                "let ({},{}) = ({}/{},{}*{});",
                der!(l),
                der!(r),
                lis,
                r,
                l,
                lis
            )
        }
        (syn::Expr::Path(expr_path_l), syn::Expr::Lit(expr_lit_r)) => {
            let (l, r) = (
                expr_path_l.path.segments[0].ident.to_string(),
                expr_lit_r
                    .lit
                    .float()
                    .expect("reverse_div: right not literal")
                    .to_string(),
            );
            format!("let {} = {}/{};", der!(l), lis, r)
        }
        (syn::Expr::Lit(expr_lit_l), syn::Expr::Path(expr_path_r)) => {
            let (l, r) = (
                expr_lit_l
                    .lit
                    .float()
                    .expect("reverse_div: left not literal")
                    .to_string(),
                expr_path_r.path.segments[0].ident.to_string(),
            );
            format!("let {} = {}*{};", der!(r), l, lis)
        }
        _ => panic!("Uncovered `syn::BinOp::Mul(_)` binary expression combination"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_div: parse fail");
    new_stmt
}
