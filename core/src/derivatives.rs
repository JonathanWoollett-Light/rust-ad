use crate::{traits::*, utils::*};

use crate::*;

/// Derivative of express for given expression with respect to itself, for a given input variable (only supports literals and paths).
fn der_wrt(expr: &syn::Expr, input_var: &str, function_inputs: &[String]) -> String {
    match expr {
        // d/dx(7) is always 0 irrespective of `x`
        syn::Expr::Lit(_) => String::from("Zero::zero()"),
        syn::Expr::Path(path_expr) => {
            // x is the left or right of binary expression typically, its the component, so if its ident
            let x = path_expr.path.segments[0].ident.to_string();

            // δa/δa = a_ (input value)
            if x == input_var {
                // d/dx(x) * a_ (where `a_` is our cumulative derivative)
                der!(input_var)
            }
            // If `x` is an input, then it with respect to another input is `Zero::zero()` since the inputs don't are presumed to be independant.
            else if function_inputs.contains(&x) {
                String::from("Zero::zero()")
            }
            // Nnot input, so needs to be relative to some function.
            else {
                // d/dx(x) * b_wrt_a (where `b_wrt_a` is our cumulative derivative)
                wrt!(x, input_var)
            }
        }
        _ => panic!("der_wrt: unsupported expr"),
    }
}
/// ```ignore
/// y = add (x1, x2)
/// dy = add (dx1, dx2)
/// ```
pub fn forward_add(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
                dx1 = der_wrt(&*bin_expr.left, input,function_inputs),
                dx2 = der_wrt(&*bin_expr.right, input,function_inputs)
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
pub fn forward_sub(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
                dx1 = der_wrt(&*bin_expr.left, input,function_inputs),
                dx2 = der_wrt(&*bin_expr.right, input,function_inputs)
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
pub fn forward_mul(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
                dx1 = der_wrt(l, input,function_inputs),
                dx2 = der_wrt(r, input,function_inputs)
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
pub fn forward_div(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
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
                dx1 = der_wrt(l, input,function_inputs),
                dx2 = der_wrt(r, input,function_inputs)
            )
        })
        .intersperse(String::from(","))
        .collect::<String>();
    let stmt_str = format!("let ({}) = ({});", idents, deriatives);
    syn::parse_str(&stmt_str).expect("forward_div: parse fail")
}

pub fn forward_powi_f32(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    forward_powi(stmt, function_inputs, "f32")
}
pub fn forward_powi_f64(stmt: &syn::Stmt, function_inputs: &[String]) -> syn::Stmt {
    forward_powi(stmt, function_inputs, "f64")
}
// TODO Can we reduce/Remove code duplication between `forward_powi_f64` and `forward_powi_f32`?
/// Deriative of f32::powi(i32) operation.
fn forward_powi(stmt: &syn::Stmt, function_inputs: &[String], float: &'static str) -> syn::Stmt {
    assert!(float == "f32" || float == "f64");
    let local = stmt.local().expect("forward_powi: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("forward_powi: not method");

    let val_ident = der!(local
        .pat
        .ident()
        .expect("forward_powi: not ident")
        .ident
        .to_string());

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
                    type_str = float
                )
            } else if *input == exponent {
                format!(
                    "{base}.powi({exponent}) * {base}.ln()",
                    exponent = exponent,
                    base = base
                )
            } else {
                String::from("Zero::zero()")
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
