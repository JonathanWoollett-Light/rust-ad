use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_u8, "0u8", "1u8", "1u8");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_u8, "0u8", "1u8", "-1u8");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_u8, "0u8", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_u8, "0u8", "1u8/{1}", "-{0}/({1}*{1})");
