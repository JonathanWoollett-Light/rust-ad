use super::*;
use rust_ad_core_macros::{combined_derivative_macro, compose};

// Primitive procedures
// -------------------------------------------------------------------

// Forward derivative of [std::ops::Add].
combined_derivative_macro!(add_f32, "0f32", "1f32", "1f32");
// Forward derivative of [std::ops::Sub].
combined_derivative_macro!(sub_f32, "0f32", "1f32", "-1f32");
// Forward derivative of [std::ops::Mul].
combined_derivative_macro!(mul_f32, "0f32", "{1}", "{0}");
// Forward derivative of [std::ops::Div].
combined_derivative_macro!(div_f32, "0f32", "1f32/{1}", "-{0}/({1}*{1})");

// Exponent procedures
// -------------------------------------------------------------------

// Forward derivative of [`powi`](https://doc.rust-lang.org/std/primitive.f32.html#method.powi).
combined_derivative_macro!(
    powi_f32,
    "0f32",
    "{1} as f32 * {0}.powi({1} - 1i32)",
    "{0}.powi({1}) * {0}.ln()"
);
// Forward derivative of [`powf`](https://doc.rust-lang.org/std/primitive.f32.html#method.powf)
combined_derivative_macro!(
    powf_f32,
    "0f32",
    "{1} as f32 * {0}.powf({1} - 1f32)",
    "{0}.powf({1}) * {0}.ln()"
);
// Forward derivative of [`sqrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.sqrt).
combined_derivative_macro!(sqrt_f32, "0f32", "1f32 / (2f32 * {0}.sqrt())");
// Forward derivative of [`cbrt`](https://doc.rust-lang.org/std/primitive.f32.html#method.cbrt).
combined_derivative_macro!(cbrt_f32, "0f32", "1f32 / (3f32*{0}.powf(2f32/3f32))");
// Forward derivative of [`exp`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp).
combined_derivative_macro!(exp_f32, "0f32", "{0}.exp()");
// Forward derivative of [`exp2`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp2).
combined_derivative_macro!(exp2_f32, "0f32", "{0}.exp2() * 2f32.ln()");
// Forward derivative of [`exp_m1`](https://doc.rust-lang.org/std/primitive.f32.html#method.exp_m1).
combined_derivative_macro!(exp_m1_f32, "0f32", "{0}.exp()");

// Log procedures
// -------------------------------------------------------------------

// Forward derivative of [`ln`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln).
combined_derivative_macro!(ln_f32, "0f32", "1f32 / {0}");
// Forward derivative of [`ln_1p`](https://doc.rust-lang.org/std/primitive.f32.html#method.ln_1p).
combined_derivative_macro!(ln_1p_f32, "0f32", "1f32 / (1f32+{0})");
// Forward derivative of [`log`](https://doc.rust-lang.org/std/primitive.f32.html#method.log).
combined_derivative_macro!(
    log_f32,
    "0f32",
    "1f32 / ({0}*{1}.ln())",
    "-{0}.ln() / ({1} *{1}.ln()*{1}.ln())"
);
// Forward derivative of [`log10`](https://doc.rust-lang.org/std/primitive.f32.html#method.log10).
combined_derivative_macro!(log10_f32, "0f32", "1f32 / ({0}*(10f32).ln())");
// Forward derivative of [`log2`](https://doc.rust-lang.org/std/primitive.f32.html#method.log2).
combined_derivative_macro!(log2_f32, "0f32", "1f32 / ({0}*(2f32).ln())");

// Trig procedures
// -------------------------------------------------------------------

// Forward derivative of [`acos`](https://doc.rust-lang.org/std/primitive.f32.html#method.acos).
combined_derivative_macro!(acos_f32, "0f32", "-1f32 / (1f32-{0}*{0}).sqrt())");
// Forward derivative of [`acosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.acosh).
combined_derivative_macro!(
    acosh_f32,
    "0f32",
    "1f32 / ( ({0}-1f32).sqrt() * ({0}+1f32).sqrt() )"
);
// Forward derivative of [`asin`](https://doc.rust-lang.org/std/primitive.f32.html#method.asin).
combined_derivative_macro!(asin_f32, "0f32", "1f32 / (1f32-{0}*{0}).sqrt()");
// Forward derivative of [`asinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.asinh).
combined_derivative_macro!(asinh_f32, "0f32", "1f32 / ({0}*{0}+1f32).sqrt()");
// Forward derivative of [`atan`](https://doc.rust-lang.org/std/primitive.f32.html#method.atan).
combined_derivative_macro!(atan_f32, "0f32", "1f32 / ({0}*{0}+1f32)");
// Forward derivative of [`sin`](https://doc.rust-lang.org/std/primitive.f32.html#method.sin).
combined_derivative_macro!(sin_f32, "0f32", "{0}.cos()");
// Forward derivative of [`atanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.atanh).
combined_derivative_macro!(atanh_f32, "0f32", "1f32 / (1f32-{0}*{0})");
// Forward derivative of [`cos`](https://doc.rust-lang.org/std/primitive.f32.html#method.cos).
combined_derivative_macro!(cos_f32, "0f32", "-({0}).sin()");
// Forward derivative of [`cosh`](https://doc.rust-lang.org/std/primitive.f32.html#method.cosh).
combined_derivative_macro!(cosh_f32, "0f32", "{0}.sinh()");
// Forward derivative of [`sinh`](https://doc.rust-lang.org/std/primitive.f32.html#method.sinh).
combined_derivative_macro!(sinh_f32, "0f32", "{0}.cosh()");
// Forward derivative of [`tan`](https://doc.rust-lang.org/std/primitive.f32.html#method.tan).
combined_derivative_macro!(tan_f32, "0f32", "1f32 / ({0}.cos() * {0}.cos())");
// Forward derivative of [`tanh`](https://doc.rust-lang.org/std/primitive.f32.html#method.tanh).
// combined_derivative_macro!(tanh_f32, "0f32","1f32 / ({base}.cosh()*{base}.cosh())");

// TODO Add atan2 (https://doc.rust-lang.org/std/primitive.f32.html#method.atan2)
// TODO Add sin_cos (https://doc.rust-lang.org/std/primitive.f32.html#method.sin_cos)

// Misc procedures
// -------------------------------------------------------------------

// Forward derivative of [`abs`](https://doc.rust-lang.org/std/primitive.f32.html#method.abs).
combined_derivative_macro!(abs_f32, "0f32", "{0}.signum()");
// Forward derivative of [`recip`](https://doc.rust-lang.org/std/primitive.f32.html#method.recip).
combined_derivative_macro!(recip_f32, "0f32", "-1f32 / ({0}{0})");

// TODO For the below functions, I do not think the given derivatives are entirely accurate.

// Forward derivative of [`ceil`](https://doc.rust-lang.org/std/primitive.f32.html#method.ceil).
combined_derivative_macro!(ceil_f32, "0f32", "1f32");
// Forward derivative of [`floor`](https://doc.rust-lang.org/std/primitive.f32.html#method.floor).
combined_derivative_macro!(floor_f32, "0f32", "1f32");
// Forward derivative of [`fract`](https://doc.rust-lang.org/std/primitive.f32.html#method.fract).
combined_derivative_macro!(fract_f32, "0f32", "1f32");
// Forward derivative of [`round`](https://doc.rust-lang.org/std/primitive.f32.html#method.round).
combined_derivative_macro!(round_f32, "0f32", "1f32");

// TODO Add some of these procedures here:
// - clamp https://doc.rust-lang.org/std/primitive.f32.html#method.clamp
// - div_eculid https://doc.rust-lang.org/std/primitive.f32.html#method.div_euclid
// - hypot https://doc.rust-lang.org/std/primitive.f32.html#method.hypot
// - mul_add https://doc.rust-lang.org/std/primitive.f32.html#method.mul_add
// - signum https://doc.rust-lang.org/std/primitive.f32.html#method.signum
// - rem_euclid https://doc.rust-lang.org/std/primitive.f32.html#method.rem_euclid
// - to_degrees https://doc.rust-lang.org/std/primitive.f32.html#method.to_degrees
// - to_radians https://doc.rust-lang.org/std/primitive.f32.html#method.to_radians
// - trunc https://doc.rust-lang.org/std/primitive.f32.html#method.trunc
