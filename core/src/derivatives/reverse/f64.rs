use super::*;
use rust_ad_core_macros::{compose, reverse_derivative_macro};

// Primitive procedures
// -------------------------------------------------------------------

// Reverse deriative of [std::ops::Add].
reverse_derivative_macro!(add_f64, "0f64", "1f64", "1f64");
// Reverse deriative of [std::ops::Sub].
reverse_derivative_macro!(sub_f64, "0f64", "1f64", "-1f64");
// Reverse deriative of [std::ops::Mul].
reverse_derivative_macro!(mul_f64, "0f64", "{1}", "{0}");
// Reverse deriative of [std::ops::Div].
reverse_derivative_macro!(div_f64, "0f64", "1f64/{1}", "-{0}/({1}*{1})");

// Exponent procedures
// -------------------------------------------------------------------

// Reverse deriative of [`powi`](https://doc.rust-lang.org/std/primitive.f64.html#method.powi).
reverse_derivative_macro!(
    powi_f64,
    "0f64",
    "{1} as f64 * {0}.powi({1} - 1i32)",
    "{0}.powi({1}) * {0}.ln()"
);
// Reverse deriative of [`powf`](https://doc.rust-lang.org/std/primitive.f64.html#method.powf)
reverse_derivative_macro!(
    powf_f64,
    "0f64",
    "{1} as f64 * {0}.powf({1} - 1f64)",
    "{0}.powf({1}) * {0}.ln()"
);
// Reverse deriative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.sqrt).
reverse_derivative_macro!(sqrt_f64, "0f64", "1f64 / (2f64 * {0}.sqrt())");
// Reverse deriative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f64.html#method.cbrt).
reverse_derivative_macro!(cbrt_f64, "0f64", "1f64 / (3f64*{0}.powf(2f64/3f64))");
// Reverse deriative of [`exp`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp).
reverse_derivative_macro!(exp_f64, "0f64", "{0}.exp()");
// Reverse deriative of [`exp2`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp2).
reverse_derivative_macro!(exp2_f64, "0f64", "{0}.exp2() * (2f64).ln()");
// Reverse deriative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f64.html#method.exp_m1).
reverse_derivative_macro!(exp_m1_f64, "0f64", "{0}.exp()");

// Log procedures
// -------------------------------------------------------------------

// Reverse deriative of [`ln`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln).
reverse_derivative_macro!(ln_f64, "0f64", "1f64 / {0}");
// Reverse deriative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f64.html#method.ln_1p).
reverse_derivative_macro!(ln_1p_f64, "0f64", "1f64 / (1f64+{0})");
// Reverse deriative of [`log`](https://doc.rust-lang.org/std/primitive.f64.html#method.log).
reverse_derivative_macro!(
    log_f64,
    "0f64",
    "1f64 / ({0}*{1}.ln())",
    "-{0}.ln() / ({1} *{1}.ln()*{1}.ln())"
);
// Reverse deriative of [`log10`](https://doc.rust-lang.org/std/primitive.f64.html#method.log10).
reverse_derivative_macro!(log10_f64, "0f64", "1f64 / ({0}*(10f64).ln())");
// Reverse deriative of [`log2`](https://doc.rust-lang.org/std/primitive.f64.html#method.log2).
reverse_derivative_macro!(log2_f64, "0f64", "1f64 / ({0}*(2f64).ln())");

// Trig procedures
// -------------------------------------------------------------------

// Reverse deriative of [`acos`](https://doc.rust-lang.org/std/primitive.f64.html#method.acos).
reverse_derivative_macro!(acos_f64, "0f64", "-1f64 / (1f64-{0}*{0}).sqrt())");
// Reverse deriative of [`acosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.acosh).
reverse_derivative_macro!(
    acosh_f64,
    "0f64",
    "1f64 / ( ({0}-1f64).sqrt() * ({0}+1f64).sqrt() )"
);
// Reverse deriative of [`asin`](https://doc.rust-lang.org/std/primitive.f64.html#method.asin).
reverse_derivative_macro!(asin_f64, "0f64", "1f64 / (1f64-{0}*{0}).sqrt()");
// Reverse deriative of [`asinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.asinh).
reverse_derivative_macro!(asinh_f64, "0f32", "1f64 / ({0}*{0}+1f64).sqrt()");
// Reverse deriative of [`atan`](https://doc.rust-lang.org/std/primitive.f64.html#method.atan).
reverse_derivative_macro!(atan_f64, "0f32", "1f64 / ({0}*{0}+1f64)");
// Reverse deriative of [`sin`](https://doc.rust-lang.org/std/primitive.f64.html#method.sin).
reverse_derivative_macro!(sin_f64, "0f32", "{0}.cos()");
// Reverse deriative of [`atanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.atanh).
reverse_derivative_macro!(atanh_f64, "0f32", "1f64 / (1f64-{0}*{0})");
// Reverse deriative of [`cos`](https://doc.rust-lang.org/std/primitive.f64.html#method.cos).
reverse_derivative_macro!(cos_f64, "0f32", "-({0}).sin()");
// Reverse deriative of [`cosh`](https://doc.rust-lang.org/std/primitive.f64.html#method.cosh).
reverse_derivative_macro!(cosh_f64, "0f32", "{0}.sinh()");
// Reverse deriative of [`sinh`](https://doc.rust-lang.org/std/primitive.f64.html#method.sinh).
reverse_derivative_macro!(sinh_f64, "0f32", "{0}.cosh()");
// Reverse deriative of [`tan`](https://doc.rust-lang.org/std/primitive.f64.html#method.tan).
reverse_derivative_macro!(tan_f64, "0f32", "1f64 / ({0}.cos() * {0}.cos())");
// Reverse deriative of [`tanh`](https://doc.rust-lang.org/std/primitive.f64.html#method.tanh).
// reverse_derivative_macro!(tanh_f64, "0f32", "1f64 / ({base}.cosh()*{base}.cosh())");

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f64.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f64.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

// Reverse deriative of [`abs`](https://doc.rust-lang.org/std/primitive.f64.html#method.abs).
reverse_derivative_macro!(abs_f64, "0f32", "{0}.signum()");
// Reverse deriative of [`recip`](https://doc.rust-lang.org/std/primitive.f64.html#method.recip).
reverse_derivative_macro!(recip_f64, "0f32", "-1f64 / ({0}{0})");

// TODO For the below functions, I do not think the given derivatives are entirely accurate.

// Reverse deriative of [`ceil`](https://doc.rust-lang.org/std/primitive.f64.html#method.ceil).
reverse_derivative_macro!(ceil_f64, "0f32", "1f64");
// Reverse deriative of [`floor`](https://doc.rust-lang.org/std/primitive.f64.html#method.floor).
reverse_derivative_macro!(floor_f64, "0f32", "1f64");
// Reverse deriative of [`fract`](https://doc.rust-lang.org/std/primitive.f64.html#method.fract).
reverse_derivative_macro!(fract_f64, "0f32", "1f64");
// Reverse deriative of [`round`](https://doc.rust-lang.org/std/primitive.f64.html#method.round).
reverse_derivative_macro!(round_f64, "0f32", "1f64");

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
