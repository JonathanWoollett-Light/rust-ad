use super::*;
use rust_ad_core_macros::{combined_derivative_macro, compose};

// Primitive procedures
// -------------------------------------------------------------------

// Derivative of [std::ops::Add].
combined_derivative_macro!(add_u8, "0u8", "1u8", "1u8");
// Derivative of [std::ops::Sub].
combined_derivative_macro!(sub_u8, "0u8", "1u8", "-1u8");
// Derivative of [std::ops::Mul].
combined_derivative_macro!(mul_u8, "0u8", "{1}", "{0}");
// Derivative of [std::ops::Div].
combined_derivative_macro!(div_u8, "0u8", "1u8/{1}", "-{0}/({1}*{1})");
