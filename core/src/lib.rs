//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**

/// Some utility functions used for [syn].
pub mod utils;

/// The prefix used to attached to derivatives of a variable (e.g. The derivative of `x` would be `der_x`).
pub const DERIVATIVE_PREFIX: &'static str = "__der_";
pub const FORWARD_MODE_PREFIX: &'static str = "__for_";
pub const REVERSE_MODE_PREFIX: &'static str = "__rev_";

/// Given identifier string (e.g. `x`) appends `DERIVATIVE_PREFIX` (e.g. `der_a`).
#[macro_export]
macro_rules! der {
    ($a:expr) => {{
        format!("{}{}", rust_ad_core::DERIVATIVE_PREFIX, $a)
    }};
}
