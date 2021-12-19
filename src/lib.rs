use rust_ad_core::der;
use rust_ad_core::utils::*;
pub use rust_ad_macros::*;

use num::Zero;

/// Forward derivative operation for [std::ops::Add].
pub trait ForwardAdd {
    fn __for_add(stmt: syn::Stmt) -> syn::Stmt;
}
/// Forward derivative operation for [std::ops::Sub].
pub trait ForwardSub {
    fn __for_sub(stmt: syn::Stmt) -> syn::Stmt;
}
/// Forward derivative operation for [std::ops::Div].
pub trait ForwardDiv {
    fn __for_div(stmt: syn::Stmt) -> syn::Stmt;
}
/// Forward derivative operation for [std::ops::Mul].
pub trait ForwardMul {
    fn __for_mul(stmt: syn::Stmt) -> syn::Stmt;
}

impl ForwardAdd for f32 {
    fn __for_add(stmt: syn::Stmt) -> syn::Stmt {
        let local = stmt.local();
        let init = &local.init;
        let bin_expr = init.as_ref().unwrap().1.binary();

        let str = format!(
            "let {} = {} + {};",
            local.pat.ident().ident,
            derivative_expr(&bin_expr.left),
            derivative_expr(&bin_expr.right)
        );
        syn::parse_str(&str).unwrap()
    }
}
fn derivative_expr(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(expr_lit) => String::from("0f32"),
        syn::Expr::Path(expr_path) => der!(expr_path.path.segments[0].ident.to_string()),
        _ => panic!("Uncoverd expr for `derivative_expr`"),
    }
}
