use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_i32, "0i32", "1i32", "1i32");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_i32, "0i32", "1i32", "-1i32");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_i32, "0i32", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_i32, "0i32", "1i32/{1}", "-{0}/({1}*{1})");
