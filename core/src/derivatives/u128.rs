use super::*;
use rust_ad_core_macros::{combined_derivative_macro, compose};

// Primitive procedures
// -------------------------------------------------------------------

// Derivative of [std::ops::Add].
combined_derivative_macro!(add_u128, "0u128", "1u128", "1u128");
// Derivative of [std::ops::Sub].
combined_derivative_macro!(sub_u128, "0u128", "1u128", "-1u128");
// Derivative of [std::ops::Mul].
combined_derivative_macro!(mul_u128, "0u128", "{1}", "{0}");
// Derivative of [std::ops::Div].
combined_derivative_macro!(div_u128, "0u128", "1u128/{1}", "-{0}/({1}*{1})");
