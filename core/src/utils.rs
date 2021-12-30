/// Gets given literal expression as string (only supports float and integers)
pub fn lit_str(lit: &syn::ExprLit) -> String {
    match &lit.lit {
        syn::Lit::Int(int_lit) => int_lit.to_string(),
        syn::Lit::Float(float_lit) => float_lit.to_string(),
        _ => panic!("lit_str: unsupported lit"),
    }
}
