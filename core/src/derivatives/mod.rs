use crate::*;

pub mod f32;
pub use self::f32::*;
pub mod f64;
pub use self::f64::*;
pub mod i8;
pub use self::i8::*;
pub mod i16;
pub use self::i16::*;
pub mod i32;
pub use self::i32::*;
pub mod i64;
pub use self::i64::*;
pub mod i128;
pub use self::i128::*;
pub mod u8;
pub use self::u8::*;
pub mod u16;
pub use self::u16::*;
pub mod u32;
pub use self::u32::*;
pub mod u64;
pub use self::u64::*;
pub mod u128;
pub use self::u128::*;

/// Forward General Derivative type
pub type FgdType = fn(&[String], String, &[Arg]) -> syn::Stmt;
/// Reverse General Derivative type
pub type RgdType = fn(String, &[Arg], &mut HashMap<String, Vec<String>>) -> syn::Stmt;

/// Function argument type
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

/// Derivative function type
pub type DFn = fn(&[Arg]) -> String;

/// Local identifier and method identifier
pub fn lm_identifiers(stmt: &syn::Stmt) -> (String, &syn::ExprMethodCall) {
    let local = stmt.local().expect("lm_identifiers: not local");
    let init = &local.init;
    let method_expr = init
        .as_ref()
        .unwrap()
        .1
        .method_call()
        .expect("lm_identifiers: not method");

    let local_ident = local
        .pat
        .ident()
        .expect("lm_identifiers: not ident")
        .ident
        .to_string();
    (local_ident, method_expr)
}

/// Gets cumulative derivative for given expression for a given input variable (only supports literals and paths).
///
/// See `cumulative_derivative_wrt` for more documentation
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

/// Forward general derivative
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

/// Reverse General Derivative
pub fn rgd<const DEFAULT: &'static str, const TRANSLATION_FUNCTIONS: &'static [DFn]>(
    local_ident: String,
    args: &[Arg],
    component_map: &mut HashMap<String, Vec<String>>,
) -> syn::Stmt {
    assert_eq!(args.len(), TRANSLATION_FUNCTIONS.len());

    let (idents, deriatives) = args
        .iter()
        .zip(TRANSLATION_FUNCTIONS.iter())
        .filter_map(|(arg, t)| match arg {
            Arg::Variable(v) => Some((v, t)),
            Arg::Literal(_) => None,
        })
        .map(|(arg, t)| {
            let der_ident = wrt!(arg, local_ident);
            append_insert(arg, local_ident.clone(), component_map);

            let (derivative, accumulator) = (t(args), der!(local_ident));
            let full_der = format!("({})*{}", derivative, accumulator);
            (der_ident, full_der)
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
    // eprintln!("stmt_str: {}", stmt_str);
    syn::parse_str(&stmt_str).expect("fgd: parse fail")
}
