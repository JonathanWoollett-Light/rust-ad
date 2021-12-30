use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_u32, "0u32", "1u32", "1u32");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_u32, "0u32", "1u32", "-1u32");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_u32, "0u32", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_u32, "0u32", "1u32/{1}", "-{0}/({1}*{1})");
