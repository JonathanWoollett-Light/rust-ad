//! **I do not recommend using this directly, please sea [rust-ad](https://crates.io/crates/rust-ad).**
//!
//! Contains constants used internally.

use const_format::concatcp;

pub const FORWARD_PREFIX: &'static str = "__f_";
pub const REVERSE_PREFIX: &'static str = "__r_";
const INTERNAL_SUFFIX: &'static str = "internal_";
pub const INTERNAL_FORWARD_PREFIX: &'static str = concatcp!(FORWARD_PREFIX, INTERNAL_SUFFIX);
pub const INTERNAL_REVERSE_PREFIX: &'static str = concatcp!(REVERSE_PREFIX, INTERNAL_SUFFIX);
