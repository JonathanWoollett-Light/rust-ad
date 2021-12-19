use rust_ad_core::utils::*;
use rust_ad_core::*;

// http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/
pub fn forward_derivative(stmt: syn::Stmt) -> Vec<syn::Stmt> {
    if let syn::Stmt::Local(ref local) = stmt {
        if let Some(ref init) = local.init {
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
                return vec![stmt, new_stmt];
            }
        }
    } else if let syn::Stmt::Semi(semi, _) = &stmt {
        if let syn::Expr::Return(rtn) = semi {
            if let Some(ref rtn_expr) = rtn.expr {
                if let syn::Expr::Path(expr_path) = &**rtn_expr {
                    let rtn = syn::Stmt::Semi(
                        syn::Expr::Return(syn::ExprReturn {
                            attrs: Vec::new(),
                            return_token: syn::token::Return {
                                span: expr_path.path.segments[0].ident.span(),
                            },
                            expr: Some(Box::new(syn::Expr::Tuple(syn::ExprTuple {
                                attrs: Vec::new(),
                                paren_token: syn::token::Paren {
                                    span: expr_path.path.segments[0].ident.span(),
                                },
                                elems: {
                                    let mut outer_p = syn::punctuated::Punctuated::new();
                                    outer_p.push(syn::Expr::Path(expr_path.clone()));
                                    outer_p.push(syn::Expr::Path(syn::ExprPath {
                                        attrs: Vec::new(),
                                        qself: None,
                                        path: syn::Path {
                                            leading_colon: None,
                                            segments: {
                                                let mut p = syn::punctuated::Punctuated::new();
                                                p.push(syn::PathSegment {
                                                    ident: syn::Ident::new(
                                                        &der!(expr_path.path.segments[0]
                                                            .ident
                                                            .to_string()),
                                                        expr_path.path.segments[0].ident.span(),
                                                    ),
                                                    arguments: syn::PathArguments::None,
                                                });
                                                p
                                            },
                                        },
                                    }));
                                    outer_p
                                },
                            }))),
                        }),
                        syn::token::Semi {
                            spans: [expr_path.path.segments[0].ident.span()],
                        },
                    );
                    return vec![rtn];
                }
            }
        }
    }
    // eprintln!("stmt: {:#?}",stmt);
    vec![stmt]
}

fn forward_add(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local();
    let init = &local.init;
    let bin_expr = init.as_ref().unwrap().1.binary();

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local.pat.ident().ident.to_string());

    let str = format!(
        "let {} = {} + {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).unwrap()
}
fn forward_sub(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local();
    let init = &local.init;
    let bin_expr = init.as_ref().unwrap().1.binary();

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);
    let d = der!(local.pat.ident().ident.to_string());

    let str = format!(
        "let {} = {} - {};",
        d,
        derivative_expr_string(l),
        derivative_expr_string(r)
    );
    syn::parse_str(&str).unwrap()
}
fn forward_mul(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local();
    let init = &local.init;
    let bin_expr = init.as_ref().unwrap().1.binary();

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);

    let str = format!(
        "let {} = ({r}*{dl}) + ({l}*{dr});",
        der!(local.pat.ident().ident.to_string()),
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).unwrap()
}
fn forward_div(stmt: &syn::Stmt) -> syn::Stmt {
    let local = stmt.local();
    let init = &local.init;
    let bin_expr = init.as_ref().unwrap().1.binary();

    let (l, r) = (&*bin_expr.left, &*bin_expr.right);

    let str = format!(
        "let {} = ({dl}/{r}) - ({dr}*{r}*{r}/{l});",
        der!(local.pat.ident().ident.to_string()),
        dl = derivative_expr_string(l),
        dr = derivative_expr_string(r),
        l = expr_string(l),
        r = expr_string(r),
    );
    syn::parse_str(&str).unwrap()
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
        syn::Expr::Lit(expr_lit) => String::from("0f32"),
        syn::Expr::Path(expr_path) => der!(expr_path.path.segments[0].ident.to_string()),
        _ => panic!("Uncoverd expr for `derivative_expr`"),
    }
}
