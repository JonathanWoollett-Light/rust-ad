use crate::der;

/// Gets given literal expression as string (only supports float and integers)
pub fn lit_str(lit: &syn::ExprLit) -> String {
    match &lit.lit {
        syn::Lit::Int(int_lit) => int_lit.to_string(),
        syn::Lit::Float(float_lit) => float_lit.to_string(),
        _ => panic!("lit_str: unsupported lit"),
    }
}
/// Gets given expression as string (only supports literals and paths)
pub fn expr_str(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(lit_expr) => lit_str(lit_expr),
        syn::Expr::Path(path_expr) => path_expr.path.segments[0].ident.to_string(),
        _ => panic!("expr_str: unsupported expr"),
    }
}
/// Gets derivative string of given expr (only supports literals and paths)
pub fn expr_der_str(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(_) => String::from("Zero::zero()"),
        syn::Expr::Path(path_expr) => der!(path_expr.path.segments[0].ident.to_string()),
        _ => panic!("expr_der_str: unsupported expr"),
    }
}
