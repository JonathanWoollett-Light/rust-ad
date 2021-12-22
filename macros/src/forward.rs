use rust_ad_core::utils::*;
use rust_ad_core::*;

pub fn update_forward_return(s: Option<&mut syn::Stmt>) {
    *s.unwrap() = match s {
        Some(syn::Stmt::Semi(syn::Expr::Return(expr_return), _)) => {
            if let Some(b) = expr_return.expr.as_ref() {
                if let syn::Expr::Path(expr_path) = &**b {
                    let ident = &expr_path.path.segments[0].ident;
                    let return_str = format!("return ({},{});", ident, der!(ident.to_string()));
                    syn::parse_str(&return_str).expect("update_forward_return malformed statement")
                } else {
                    panic!("No return path:\n{:#?}", b)
                }
            } else {
                panic!("No return expression:\n{:#?}", expr_return)
            }
        }
        _ => panic!("No retun statement:\n{:#?}", s),
    }
}

/// Insperses values with respect to the preceding values.
pub fn interspese_succedding<T>(x: Vec<T>, f: fn(&T) -> Option<T>) -> Vec<T> {
    let len = x.len();
    let new_len = len * 2 - 1;
    let mut y = Vec::with_capacity(new_len);
    let mut x_iter = x.into_iter().rev();
    if let Some(last) = x_iter.next() {
        y.push(last);
    }
    for a in x_iter {
        if let Some(b) = f(&a) {
            y.push(b);
        }
        y.push(a);
    }
    y.into_iter().rev().collect()
}

// http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/
pub fn forward_derivative(stmt: &syn::Stmt) -> Option<syn::Stmt> {
    if let syn::Stmt::Local(local) = stmt {
        if let Some(init) = &local.init {
            if let syn::Expr::Binary(bin_expr) = &*init.1 {
                // eprintln!("bin_expr: {:#?}\n.\n", bin_expr);
                // panic!("stopping here");

                // TODO Do the rest of the operations.
                let new_stmt = match bin_expr.op {
                    // y = x1+x2 => dy = dx1 + dx2
                    syn::BinOp::Add(_) => forward_add(&stmt),
                    // y = x1-x2 => dy = dx1 - dx2
                    syn::BinOp::Sub(_) => forward_sub(&stmt),
                    // y = x1*x2 => dy = (x2*dx1)+(x1*dx2)
                    syn::BinOp::Mul(_) => forward_mul(&stmt),
                    // y = x1 / x2 => dy = dx1/x2 - (x1/(x2*x2))*dx2
                    syn::BinOp::Div(_) => forward_div(&stmt),
                    _ => panic!("Uncovered operation"),
                };
                return Some(new_stmt);
            } else if let syn::Expr::Call(_call_expr) = &*init.1 {
                let local = stmt.local().expect("forward_derivative: not local");
                let d = der!(local
                    .pat
                    .ident()
                    .expect("forward_derivative: not ident")
                    .ident
                    .to_string());
                let str = format!("let {} = placeholder();", d,);
                let expr = syn::parse_str(&str).expect("forward_add: parse fail");
                return Some(expr);
            }
        }
    }
    None
}

fn forward_add(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_add: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_add: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_add: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {} + {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).expect("forward_add: parse fail")
}
fn forward_sub(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_sub: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_sub: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_sub: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {} - {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).expect("forward_sub: parse fail")
}
fn forward_mul(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_mul: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_mul: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_mul: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {r}*{dl} + {l}*{dr};",
        d,
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).expect("forward_mul: parse fail")
}
fn forward_div(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local().expect("forward_div: not local");
    let init = &local.init;
    let bin_expr = init
        .as_ref()
        .unwrap()
        .1
        .binary()
        .expect("forward_div: not binary");

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local
        .pat
        .ident()
        .expect("forward_div: not ident")
        .ident
        .to_string());

    let str = format!(
        "let {} = {dl}/{r} - {dr}*{r}*{r}/{l};",
        d,
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).expect("forward_div: parse fail")
}
fn expr_string(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(expr_lit) => match &expr_lit.lit {
            syn::Lit::Float(lit_float) => lit_float.to_string(),
            _ => panic!("Uncovere literaly in `expr_string`"),
        },
        syn::Expr::Path(expr_path) => expr_path.path.segments[0].ident.to_string(),
        _ => panic!("Uncoverd expr for `derivative_expr`"),
    }
}
/// Derivative expression string
fn derivative_expr_string(expr: &syn::Expr) -> String {
    match expr {
        syn::Expr::Lit(_) => String::from("0."),
        syn::Expr::Path(expr_path) => der!(expr_path.path.segments[0].ident.to_string()),
        _ => panic!("Uncoverd expr for `derivative_expr`"),
    }
}
