use crate::*;

mod forward;
pub use forward::*;

mod reverse;
pub use reverse::*;

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
