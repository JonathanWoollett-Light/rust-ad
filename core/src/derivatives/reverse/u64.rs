use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_u64, "0u64", "1u64", "1u64");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_u64, "0u64", "1u64", "-1u64");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_u64, "0u64", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_u64, "0u64", "1u64/{1}", "-{0}/({1}*{1})");
