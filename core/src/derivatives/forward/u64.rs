use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_u64, "0u64", "1u64", "1u64");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_u64, "0u64", "1u64", "-1u64");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_u64, "0u64", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_u64, "0u64", "1u64/{1}", "-{0}/({1}*{1})");
