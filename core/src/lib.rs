//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

use std::collections::HashMap;

/// Some utility functions used for [syn].
pub mod utils;

/// The prefix used for the derivatives of a variable (e.g. The derivative of `x` would be `der_x`).
pub const DERIVATIVE_PREFIX: &'static str = "__der_";
/// Prefix used to for the forward differentiation function.
pub const FORWARD_MODE_PREFIX: &'static str = "__for_";
/// Prefix used to for the reverse differentiation function.
pub const REVERSE_MODE_PREFIX: &'static str = "__rev_";
pub const FUNCTION_PREFFIX: &'static str = "f";
pub const RECEIVER_PREFIX: &'static str = "r";

/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
#[macro_export]
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", rust_ad_core::DERIVATIVE_PREFIX, $a)
    }};
}
pub struct FunctonSignature {
    /// Input types
    inputs: Vec<String>,
    /// Output type
    output: String,
}
impl FunctonSignature {
    fn new(inputs: &[&'static str], output: &'static str) -> Self {
        Self {
            inputs: inputs.iter().map(|input| String::from(*input)).collect(),
            output: String::from(output),
        }
    }
}
pub struct FunctionMap(HashMap<String, Vec<FunctonSignature>>);
impl FunctionMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn get(&self, name: &str, input_types: &[String]) -> Option<String> {
        match self.0.get(name) {
            Some(signatures) => signatures
                .iter()
                .find(|sig| sig.inputs == input_types)
                .map(|s| s.output.clone()),
            None => None,
        }
    }
    pub fn insert(&mut self, name: &str, signatures: Vec<FunctonSignature>) {
        self.0.insert(String::from(name), signatures);
    }
}

pub struct MethodSignature {
    /// Type of `self`
    on: String,
    /// Input types
    inputs: Vec<String>,
    /// Output type
    output: String,
}
impl MethodSignature {
    fn new(on: &'static str, inputs: &[&'static str], output: &'static str) -> Self {
        Self {
            on: String::from(on),
            inputs: inputs.iter().map(|input| String::from(*input)).collect(),
            output: String::from(output),
        }
    }
}
pub struct MethodMap(HashMap<String, Vec<MethodSignature>>);
impl MethodMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn get(&self, name: &str, on: &str, input_types: &[String]) -> Option<String> {
        eprintln!("get: {}, {}, {:?}",name,on,input_types);
        match self.0.get(name) {
            Some(signatures) => signatures
                .iter()
                .find(|sig| sig.inputs == input_types && sig.on == on)
                .map(|s| s.output.clone()),
            None => None,
        }
    }
    pub fn insert(&mut self, name: &str, signatures: Vec<MethodSignature>) {
        self.0.insert(String::from(name), signatures);
    }
}

lazy_static::lazy_static! {
    /// Internal map of currently supported functions.
    pub static ref SUPPORTED_FUNCTIONS: FunctionMap = {
        let mut map = FunctionMap::new();
        map.insert("some_function",vec![FunctonSignature::new(&["f64","f64"],"f32")]);
        map
    };
    /// Internal map of currently supported methods.
    pub static ref SUPPORTED_METHODS: MethodMap = {
        let mut map = MethodMap::new();
        map.insert("powi",vec![
            MethodSignature::new("f32",&["i32"],"f32"),
            MethodSignature::new("f64",&["i32"],"f64")
        ]);
        map
    };
}
