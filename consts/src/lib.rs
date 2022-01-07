//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**
//!
//! Internal constants.
//!
//! Lowest level of dependency.

use const_format::concatcp;

/// Prefix used for the derivatives of a variable (e.g. The derivative of `x` would be `der_x`).
pub const DERIVATIVE_PREFIX: &str = "__der_";
/// Prefix for external forward auto-diff functions.
pub const FORWARD_PREFIX: &str = "__f_";
/// Prefix for external reverse auto-diff functions.
pub const REVERSE_PREFIX: &str = "__r_";
/// Suffix for internal functions.
const INTERNAL_SUFFIX: &str = "internal_";
/// Prefix for internal forward auto-diff functions (e.g. `__f_a_users_function` vs `__f_internal_powi_f32`).
pub const INTERNAL_FORWARD_PREFIX: &str = concatcp!(FORWARD_PREFIX, INTERNAL_SUFFIX);
/// Prefix for internal reverse auto-diff functions (e.g. `__r_a_users_function` vs `__r_internal_powi_f32`).
pub const INTERNAL_REVERSE_PREFIX: &str = concatcp!(REVERSE_PREFIX, INTERNAL_SUFFIX);

// const RETURN_SUFFIX: &str = "__rtn";
// pub const REVERSE_RETURN_DERIVATIVE: &str = concatcp!(DERIVATIVE_PREFIX,RETURN_SUFFIX);

pub const REVERSE_RETURN_DERIVATIVE: &str = "r";
