use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_i16, "0i16", "1i16", "1i16");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_i16, "0i16", "1i16", "-1i16");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_i16, "0i16", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_i16, "0i16", "1i16/{1}", "-{0}/({1}*{1})");
