use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_u8, "0u8", "1u8", "1u8");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_u8, "0u8", "1u8", "-1u8");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_u8, "0u8", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_u8, "0u8", "1u8/{1}", "-{0}/({1}*{1})");
