use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_i16, "0i16", "1i16", "1i16");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_i16, "0i16", "1i16", "-1i16");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_i16, "0i16", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_i16, "0i16", "1i16/{1}", "-{0}/({1}*{1})");
