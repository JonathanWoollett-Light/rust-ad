use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_i8, "0i8", "1i8", "1i8");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_i8, "0i8", "1i8", "-1i8");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_i8, "0i8", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_i8, "0i8", "1i8/{1}", "-{0}/({1}*{1})");
