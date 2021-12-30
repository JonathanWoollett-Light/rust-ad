use super::*;
use rust_ad_core_macros::{compose, forward_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Forward deriative of [std::ops::Add].
forward_derivative_macro!(add_f64, "0f64", "1f64", "1f64");
// Forward deriative of [std::ops::Sub].
forward_derivative_macro!(sub_f64, "0f64", "1f64", "-1f64");
// Forward deriative of [std::ops::Mul].
forward_derivative_macro!(mul_f64, "0f64", "{1}", "{0}");
// Forward deriative of [std::ops::Div].
forward_derivative_macro!(div_f64, "0f64", "1f64/{1}", "-{0}/({1}*{1})");

// Exponent procedures
// -------------------------------------------------------------------

// Forward deriative of [`powi`](https://doc.rust-lang.org/std/primitive.f64.html#method.powi).
forward_derivative_macro!(
    powi_f64,
    "0f64",
    "{1} * {0}.powi({1} - 1i32)",
    "{0}.powi({1}) * {0}.ln()"
);
// Forward deriative of [`powf`](https://doc.rust-lang.org/std/primitive.f64.html#method.powf)
forward_derivative_macro!(
    powf_f64,
    "0f64",
    "{1} * {0}.powf({1} - 1f64)",
    "{0}.powf({1}) * {0}.ln()"
);
// Forward deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.sqrt).
forward_derivative_macro!(sqrt_f64, "0f64", "1f64 / (2f64 * {0}.sqrt())");
// Forward deriative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.cbrt).
forward_derivative_macro!(cbrt_f64, "0f64", "1f64 / (3f64*{0}.powf(2f64/3f64))");
// Forward deriative of [`exp`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp).
forward_derivative_macro!(exp_f64, "0f64", "{0}.exp()");
// Forward deriative of [`exp2`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp2).
forward_derivative_macro!(exp2_f64, "0f64", "{0}.exp2() * (2f64).ln()");
// Forward deriative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp_m1).
forward_derivative_macro!(exp_m1_f64, "0f64", "{0}.exp()");

// Log procedures
// -------------------------------------------------------------------

// Forward deriative of [`ln`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln).
forward_derivative_macro!(ln_f64, "0f64", "1f64 / {0}");
// Forward deriative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln_1p).
forward_derivative_macro!(ln_1p_f64, "0f64", "1f64 / (1f64+{0})");
// Forward deriative of [`log`](https://doc.rust-lang.org/std/primitive.f64.html#method.log).
forward_derivative_macro!(
    log_f64,
    "0f64",
    "1f64 / ({0}*{1}.ln())",
    "-{0}.ln() / ({1} *{1}.ln()*{1}.ln())"
);
// Forward deriative of [`log10`](https://doc.rust-lang.org/std/primitive.f64.html#method.log10).
forward_derivative_macro!(log10_f64, "0f64", "1f64 / ({0}*(10f64).ln())");
// Forward deriative of [`log2`](https://doc.rust-lang.org/std/primitive.f64.html#method.log2).
forward_derivative_macro!(log2_f64, "0f64", "1f64 / ({0}*(2f64).ln())");

// Trig procedures
// -------------------------------------------------------------------

// Forward deriative of [`acos`](https://doc.rust-lang.org/std/primitive.f64.html#method.acos).
forward_derivative_macro!(acos_f64, "0f64", "-1f64 / (1f64-{0}*{0}).sqrt())");
// Forward deriative of [`acosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.acosh).
forward_derivative_macro!(
    acosh_f64,
    "0f64",
    "1f64 / ( ({0}-1f64).sqrt() * ({0}+1f64).sqrt() )"
);
// Forward deriative of [`asin`](https://doc.rust-lang.org/std/primitive.f64.html#method.asin).
forward_derivative_macro!(asin_f64, "0f64", "1f64 / (1f64-{0}*{0}).sqrt()");
// Forward deriative of [`asinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.asinh).
forward_derivative_macro!(asinh_f64, "0f32","1f64 / ({0}*{0}+1f64).sqrt()");
// Forward deriative of [`atan`](https://doc.rust-lang.org/std/primitive.f64.html#method.atan).
forward_derivative_macro!(atan_f64, "0f32","1f64 / ({0}*{0}+1f64)");
// Forward deriative of [`sin`](https://doc.rust-lang.org/std/primitive.f64.html#method.sin).
forward_derivative_macro!(sin_f64, "0f32","{0}.cos()");
// Forward deriative of [`atanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.atanh).
forward_derivative_macro!(atanh_f64, "0f32","1f64 / (1f64-{0}*{0})");
// Forward deriative of [`cos`](https://doc.rust-lang.org/std/primitive.f64.html#method.cos).
forward_derivative_macro!(cos_f64, "0f32","-({0}).sin()");
// Forward deriative of [`cosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.cosh).
forward_derivative_macro!(cosh_f64, "0f32","{0}.sinh()");
// Forward deriative of [`sinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.sinh).
forward_derivative_macro!(sinh_f64, "0f32","{0}.cosh()");
// Forward deriative of [`tan`](https://doc.rust-lang.org/std/primitive.f64.html#method.tan).
forward_derivative_macro!(tan_f64, "0f32","1f64 / ({0}.cos() * {0}.cos())");
// Forward deriative of [`tanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.tanh).
// forward_derivative_macro!(tanh_f64, "0f32", "1f64 / ({base}.cosh()*{base}.cosh())");

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f64.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f64.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

// Forward deriative of [`abs`](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).
forward_derivative_macro!(abs_f64, "0f32","{0}.signum()");

// TODO Is this derivative for `ceil` right?
// Forward deriative of [`ceil`](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).
forward_derivative_macro!(ceil_f64, "0f32","1f64");

// TODO Is this derivative for `floor` right?
// Forward deriative of [`floor`](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).
forward_derivative_macro!(floor_f64, "0f32","1f64");

// TODO Is this derivative for `fract` right?
// Forward deriative of [`fract`](https://doc.rust-lang.org/std/primitive.f64.html#method.fract).
forward_derivative_macro!(fract_f64, "0f32","1f64");

// TODO Is this derivative for `recip` right?
// Forward deriative of [`recip`](https://doc.rust-lang.org/std/primitive.f64.html#method.recip).
forward_derivative_macro!(recip_f64, "0f32","-1f64 / ({0}{0})");

// TODO Is this derivative for `round` right?
// Forward deriative of [`round`](https://doc.rust-lang.org/std/primitive.f64.html#method.round).
forward_derivative_macro!(round_f64, "0f32","1f64");

// TODO Add some of these procedures here:
// - clamp https://doc.rust-lang.org/std/primitive.f64.html#method.clamp
// - div_eculid https://doc.rust-lang.org/std/primitive.f64.html#method.div_euclid
// - hypot https://doc.rust-lang.org/std/primitive.f64.html#method.hypot
// - mul_add https://doc.rust-lang.org/std/primitive.f64.html#method.mul_add
// - signum https://doc.rust-lang.org/std/primitive.f64.html#method.signum
// - rem_euclid https://doc.rust-lang.org/std/primitive.f64.html#method.rem_euclid
// - to_degrees https://doc.rust-lang.org/std/primitive.f64.html#method.to_degrees
// - to_radians https://doc.rust-lang.org/std/primitive.f64.html#method.to_radians
// - trunc https://doc.rust-lang.org/std/primitive.f64.html#method.trunc
