use super::*;
use rust_ad_core_macros::{combined_derivative_macro, compose};

// Primitive procedures
// -------------------------------------------------------------------

// Derivative of [std::ops::Add].
combined_derivative_macro!(add_i128, "0i128", "1i128", "1i128");
// Derivative of [std::ops::Sub].
combined_derivative_macro!(sub_i128, "0i128", "1i128", "-1i128");
// Derivative of [std::ops::Mul].
combined_derivative_macro!(mul_i128, "0i128", "{1}", "{0}");
// Derivative of [std::ops::Div].
combined_derivative_macro!(div_i128, "0i128", "1i128/{1}", "-{0}/({1}*{1})");
