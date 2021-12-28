use crate::append_insert;
use crate::*;
use crate::{traits::*, utils::*};

// Primitive procedures
// -------------------------------------------------------------------

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
        }
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
        }
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
        }
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
        }
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_div: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_div: parse fail");
    Some(new_stmt)
}

// Exponent procedures
// -------------------------------------------------------------------

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
        }
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
                "let ({},{}) = ({dx} * ({exponent} * {base}.powf({exponent}-1{val_type})), {dx} * ({base}.powf({exponent}) * {base}.ln() ) );",
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
                "let {} = {} * ({exponent} * {base}.powf({exponent}-1{val_type}));",
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
                "let {} = {} * ({base}.powf({exponent}) * {base}.ln() );",
                wrt!(exponent, lis),
                der!(lis),
                base = base,
                exponent = exponent,
            )
        }
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_powf: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_powf: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.sqrt).
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
                wrt!(base, lis),
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
/// Reverse deriative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.cbrt).
pub fn reverse_cbrt<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_cbrt: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_cbrt: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_cbrt: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / (3{val_type}*{base}.powf(2f32/3f32)) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_cbrt: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_cbrt: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`exp`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp).
pub fn reverse_exp<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_exp: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_exp: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_exp: not ident")
        .ident
        .to_string();

    let exponent = &*method_expr.receiver;

    let stmt_str = match exponent {
        syn::Expr::Path(expr_path) => {
            let exponent = expr_path.path.segments[0].ident.to_string();
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {exponent}.exp() );",
                wrt!(exponent, lis),
                exponent = exponent,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_exp: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_exp: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`exp2`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp2).
pub fn reverse_exp2<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_exp2: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_exp2: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_exp2: not ident")
        .ident
        .to_string();

    let exponent = &*method_expr.receiver;

    let stmt_str = match exponent {
        syn::Expr::Path(expr_path) => {
            let exponent = expr_path.path.segments[0].ident.to_string();
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {exponent}.exp2() * (2f32).ln() );",
                wrt!(exponent, lis),
                exponent = exponent,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_exp2: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_exp2: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp_m1).
pub fn reverse_exp_m1<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_exp_m1: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_exp_m1: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_exp_m1: not ident")
        .ident
        .to_string();

    let exponent = &*method_expr.receiver;

    let stmt_str = match exponent {
        syn::Expr::Path(expr_path) => {
            let exponent = expr_path.path.segments[0].ident.to_string();
            append_insert(&exponent, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {exponent}.exp() );",
                wrt!(exponent, lis),
                exponent = exponent,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_exp_m1: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_exp_m1: parse fail");
    Some(new_stmt)
}

// Log procedures
// -------------------------------------------------------------------

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
                wrt!(base, lis),
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
/// Reverse deriative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln_1p).
pub fn reverse_ln_1p<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_ln_1p: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_ln_1p: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_ln_1p: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / (1{val_type}+{base}) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_ln_1p: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_ln_1p: parse fail");
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
        }
        (syn::Expr::Lit(_), syn::Expr::Lit(_)) => return None,
        _ => panic!("reverse_log: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_log: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`log10`](https://doc.rust-lang.org/std/primitive.f32.html#method.log10).
pub fn reverse_log10<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_log10: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_log10: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_log10: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}*(10{val_type}).ln()) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_log10: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_log10: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`log2`](https://doc.rust-lang.org/std/primitive.f32.html#method.log2).
pub fn reverse_log2<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    assert!(OUT == Type::F32 || OUT == Type::F64);
    let local = stmt.local().expect("reverse_log2: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_log2: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_log2: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}*(2{val_type}).ln()) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_log2: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_log2: parse fail");
    Some(new_stmt)
}

// Trig procedures
// -------------------------------------------------------------------

/// Reverse deriative of [`acos`](https://doc.rust-lang.org/std/primitive.f32.html#method.acos).
pub fn reverse_acos<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_acos: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_acos: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_acos: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( -1{val_type} / (1{val_type}-{base}*{base}).sqrt() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_acos: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_acos: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`acosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.acosh).
pub fn reverse_acosh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_acosh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_acosh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_acosh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ( ({base}-1{val_type}).sqrt() * ({base}+1{val_type}).sqrt() ) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_acosh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_acosh: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`asin`](https://doc.rust-lang.org/std/primitive.f32.html#method.asin).
pub fn reverse_asin<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_asin: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_asin: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_asin: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / (1{val_type}-{base}*{base}).sqrt() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_asin: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_asin: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`asinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.asinh).
pub fn reverse_asinh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_asinh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_asinh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_asinh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}*{base}+1{val_type}).sqrt() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_asinh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_asinh: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`atan`](https://doc.rust-lang.org/std/primitive.f32.html#method.atan).
pub fn reverse_atan<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_atan: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_atan: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_atan: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}*{base}+1{val_type}) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_atan: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_atan: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`sin`](https://doc.rust-lang.org/std/primitive.f32.html#method.sin).
pub fn reverse_sin<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_sin: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_sin: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_sin: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {base}.cos() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_sin: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sin: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`atanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.atanh).
pub fn reverse_atanh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_atanh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_atanh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_atanh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / (1{val_type}-{base}*{base}) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_atanh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_atanh: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`cos`](https://doc.rust-lang.org/std/primitive.f32.html#method.cos).
pub fn reverse_cos<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_cos: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_cos: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_cos: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( -{base}.sin() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_cos: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_cos: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`cosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.cosh).
pub fn reverse_cosh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_cosh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_cosh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_cosh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {base}.sinh() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_cosh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_cosh: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`sinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.sinh).
pub fn reverse_sinh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_sinh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_sinh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_sinh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( {base}.cosh() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_sinh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_sinh: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`tan`](https://doc.rust-lang.org/std/primitive.f32.html#method.tan).
pub fn reverse_tan<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_tan: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_tan: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_tan: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}.cos() * {base}.cos()) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_tan: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_tan: parse fail");
    Some(new_stmt)
}
/// Reverse deriative of [`tanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.tanh).
pub fn reverse_tanh<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_tanh: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_tanh: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_tanh: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} / ({base}.cosh()*{base}.cosh()) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_tanh: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_tanh: parse fail");
    Some(new_stmt)
}

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f32.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f32.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

/// Reverse deriative of [`abs`](https://doc.rust-lang.org/std/primitive.f32.html#method.abs).
pub fn reverse_abs<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_abs: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_abs: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_abs: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            // If base>0 then as it rises, so does lis, thus 1 derivative, inversly, if base<0, then -1 deriative
            // x/x.abs() == if x >= 0 { 1 } else { -1 } == x.signum()
            format!(
                "let {} = {dx} * ( {base}.signum() );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_abs: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_abs: parse fail");
    Some(new_stmt)
}

// TODO Is this derivative for `ceil` right?
/// Reverse deriative of [`ceil`](https://doc.rust-lang.org/std/primitive.f32.html#method.ceil).
pub fn reverse_ceil<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_ceil: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_ceil: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_ceil: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} );",
                wrt!(base, lis),
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_ceil: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_ceil: parse fail");
    Some(new_stmt)
}

// TODO Is this derivative for `floor` right?
/// Reverse deriative of [`floor`](https://doc.rust-lang.org/std/primitive.f32.html#method.floor).
pub fn reverse_floor<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_floor: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_floor: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_floor: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} );",
                wrt!(base, lis),
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_floor: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_floor: parse fail");
    Some(new_stmt)
}

// TODO Is this derivative for `fract` right?
/// Reverse deriative of [`fract`](https://doc.rust-lang.org/std/primitive.f32.html#method.fract).
pub fn reverse_fract<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_fract: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_fract: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_fract: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} );",
                wrt!(base, lis),
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_fract: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_fract: parse fail");
    Some(new_stmt)
}

/// Reverse deriative of [`recip`](https://doc.rust-lang.org/std/primitive.f32.html#method.recip).
pub fn reverse_recip<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_recip: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_recip: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_recip: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( -1{val_type} / ({base}*{base}) );",
                wrt!(base, lis),
                base = base,
                dx = der!(lis),
                val_type = OUT.to_string(),
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_recip: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_recip: parse fail");
    Some(new_stmt)
}

// TODO Is this derivative for `round` right?
/// Reverse deriative of [`round`](https://doc.rust-lang.org/std/primitive.f32.html#method.round).
pub fn reverse_round<const OUT: Type>(
    stmt: &syn::Stmt,
    component_map: &mut HashMap<String, Vec<String>>,
) -> Option<syn::Stmt> {
    let local = stmt.local().expect("reverse_round: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("reverse_round: not method");

    let lis = local
        .pat
        .ident()
        .expect("reverse_round: not ident")
        .ident
        .to_string();

    let base = &*method_expr.receiver;

    let stmt_str = match base {
        syn::Expr::Path(expr_path) => {
            let base = expr_path.path.segments[0].ident.to_string();
            append_insert(&base, lis.clone(), component_map);
            format!(
                "let {} = {dx} * ( 1{val_type} );",
                wrt!(base, lis),
                dx = der!(lis),
                val_type = OUT.to_string()
            )
        }
        syn::Expr::Lit(_) => return None,
        _ => panic!("reverse_round: Unsupported bin expr"),
    };
    let new_stmt: syn::Stmt = syn::parse_str(&stmt_str).expect("reverse_round: parse fail");
    Some(new_stmt)
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
