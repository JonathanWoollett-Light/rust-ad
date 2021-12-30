use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_i8, "0i8", "1i8", "1i8");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_i8, "0i8", "1i8", "-1i8");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_i8, "0i8", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_i8, "0i8", "1i8/{1}", "-{0}/({1}*{1})");
