use super::*;
use rust_ad_core_macros::{combined_derivative_macro, compose};

// Primitive procedures
// -------------------------------------------------------------------

// Derivative of [std::ops::Add].
combined_derivative_macro!(add_f64, "0f64", "1f64", "1f64");
// Derivative of [std::ops::Sub].
combined_derivative_macro!(sub_f64, "0f64", "1f64", "-1f64");
// Derivative of [std::ops::Mul].
combined_derivative_macro!(mul_f64, "0f64", "{1}", "{0}");
// Derivative of [std::ops::Div].
combined_derivative_macro!(div_f64, "0f64", "1f64/{1}", "-{0}/({1}*{1})");

// Exponent procedures
// -------------------------------------------------------------------

// Derivative of [`powi`](https://doc.rust-lang.org/std/primitive.f64.html#method.powi).
combined_derivative_macro!(
    powi_f64,
    "0f64",
    "{1} as f64 * {0}.powi({1} - 1i32)",
    "{0}.powi({1}) * {0}.ln()"
);
// Derivative of [`powf`](https://doc.rust-lang.org/std/primitive.f64.html#method.powf)
combined_derivative_macro!(
    powf_f64,
    "0f64",
    "{1} as f64 * {0}.powf({1} - 1f64)",
    "{0}.powf({1}) * {0}.ln()"
);
// Derivative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.sqrt).
combined_derivative_macro!(sqrt_f64, "0f64", "1f64 / (2f64 * {0}.sqrt())");
// Derivative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.cbrt).
combined_derivative_macro!(cbrt_f64, "0f64", "1f64 / (3f64*{0}.powf(2f64/3f64))");
// Derivative of [`exp`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp).
combined_derivative_macro!(exp_f64, "0f64", "{0}.exp()");
// Derivative of [`exp2`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp2).
combined_derivative_macro!(exp2_f64, "0f64", "{0}.exp2() * (2f64).ln()");
// Derivative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp_m1).
combined_derivative_macro!(exp_m1_f64, "0f64", "{0}.exp()");

// Log procedures
// -------------------------------------------------------------------

// Derivative of [`ln`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln).
combined_derivative_macro!(ln_f64, "0f64", "1f64 / {0}");
// Derivative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln_1p).
combined_derivative_macro!(ln_1p_f64, "0f64", "1f64 / (1f64+{0})");
// Derivative of [`log`](https://doc.rust-lang.org/std/primitive.f64.html#method.log).
combined_derivative_macro!(
    log_f64,
    "0f64",
    "1f64 / ({0}*{1}.ln())",
    "-{0}.ln() / ({1} *{1}.ln()*{1}.ln())"
);
// Derivative of [`log10`](https://doc.rust-lang.org/std/primitive.f64.html#method.log10).
combined_derivative_macro!(log10_f64, "0f64", "1f64 / ({0}*(10f64).ln())");
// Derivative of [`log2`](https://doc.rust-lang.org/std/primitive.f64.html#method.log2).
combined_derivative_macro!(log2_f64, "0f64", "1f64 / ({0}*(2f64).ln())");

// Trig procedures
// -------------------------------------------------------------------

// Derivative of [`acos`](https://doc.rust-lang.org/std/primitive.f64.html#method.acos).
combined_derivative_macro!(acos_f64, "0f64", "-1f64 / (1f64-{0}*{0}).sqrt())");
// Derivative of [`acosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.acosh).
combined_derivative_macro!(
    acosh_f64,
    "0f64",
    "1f64 / ( ({0}-1f64).sqrt() * ({0}+1f64).sqrt() )"
);
// Derivative of [`asin`](https://doc.rust-lang.org/std/primitive.f64.html#method.asin).
combined_derivative_macro!(asin_f64, "0f64", "1f64 / (1f64-{0}*{0}).sqrt()");
// Derivative of [`asinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.asinh).
combined_derivative_macro!(asinh_f64, "0f32", "1f64 / ({0}*{0}+1f64).sqrt()");
// Derivative of [`atan`](https://doc.rust-lang.org/std/primitive.f64.html#method.atan).
combined_derivative_macro!(atan_f64, "0f32", "1f64 / ({0}*{0}+1f64)");
// Derivative of [`sin`](https://doc.rust-lang.org/std/primitive.f64.html#method.sin).
combined_derivative_macro!(sin_f64, "0f32", "{0}.cos()");
// Derivative of [`atanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.atanh).
combined_derivative_macro!(atanh_f64, "0f32", "1f64 / (1f64-{0}*{0})");
// Derivative of [`cos`](https://doc.rust-lang.org/std/primitive.f64.html#method.cos).
combined_derivative_macro!(cos_f64, "0f32", "-({0}).sin()");
// Derivative of [`cosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.cosh).
combined_derivative_macro!(cosh_f64, "0f32", "{0}.sinh()");
// Derivative of [`sinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.sinh).
combined_derivative_macro!(sinh_f64, "0f32", "{0}.cosh()");
// Derivative of [`tan`](https://doc.rust-lang.org/std/primitive.f64.html#method.tan).
combined_derivative_macro!(tan_f64, "0f32", "1f64 / ({0}.cos() * {0}.cos())");
// Derivative of [`tanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.tanh).
// combined_derivative_macro!(tanh_f64, "0f32", "1f64 / ({base}.cosh()*{base}.cosh())");

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f64.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f64.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

// Derivative of [`abs`](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).
combined_derivative_macro!(abs_f64, "0f32", "{0}.signum()");
// Derivative of [`recip`](https://doc.rust-lang.org/std/primitive.f64.html#method.recip).
combined_derivative_macro!(recip_f64, "0f32", "-1f64 / ({0}{0})");

// TODO For the below functions, I do not think the given derivatives are entirely accurate.

// Derivative of [`ceil`](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).
combined_derivative_macro!(ceil_f64, "0f32", "1f64");
// Derivative of [`floor`](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).
combined_derivative_macro!(floor_f64, "0f32", "1f64");
// Derivative of [`fract`](https://doc.rust-lang.org/std/primitive.f64.html#method.fract).
combined_derivative_macro!(fract_f64, "0f32", "1f64");
// Derivative of [`round`](https://doc.rust-lang.org/std/primitive.f64.html#method.round).
combined_derivative_macro!(round_f64, "0f32", "1f64");

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
