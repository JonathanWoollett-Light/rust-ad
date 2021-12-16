# RustAD - Rust Auto-Differentiation

[![Crates.io](https://img.shields.io/crates/v/rust-ad)](https://crates.io/crates/rust-ad)
[![lib.rs.io](https://img.shields.io/crates/v/rust-ad?color=blue&label=lib.rs)](https://lib.rs/crates/rust-ad)
[![docs](https://img.shields.io/crates/v/rust-ad?color=yellow&label=docs)](https://docs.rs/rust-ad)

A super restrictive rough WIP beginnings of a library attempting to implement auto-differentiation in Rust.
  
Auto-differentiation is implemented via a 2 procedural attribute macros:

### `forward_autodiff`

```rust
#[rust_ad::forward_autodiff]
fn function_name(x:f32, y:f32) -> f32 {
    let p = 7. * x;
    let r = 10. - y;
    let q = p * x * 5.;
    let v = 2. * p * q + 3. * r;
    return v;
}
```
Expands to:
```rust
fn __for_function_name(x: f32, y: f32, der_x: f32, der_y: f32) -> (f32, f32) {
    let a = 7. * x;
    let der_a = x * 0f32 + 7. * der_x;
    let b = 3. * x;
    let der_b = x * 0f32 + 3. * der_x;
    let c = x + b;
    let der_c = der_x + der_b;
    let _d = y + b;
    let der__d = der_y + der_b;
    let d = _d + c;
    let der_d = der__d + der_c;
    return (d, der_d);
}
```

### `reverse_autodiff`

```rust
#[rust_ad::reverse_autodiff]
fn function_name(x: f32, y: f32) -> f32 {
    let a = 7. * x;
    let b = 3. * x;
    let c = x + b;
    let d = y + b + c;
    return d;
}
```
Expands to:
```rust
fn __rev_function_name(x: f32, y: f32, der_d: f32) -> (f32, f32, f32) {
    let (x0, x1, x2) = (x.clone(), x.clone(), x.clone());
    let (y0,) = (y.clone(),);
    let a = 7. * x0;
    let b = 3. * x1;
    let (b0, b1) = (b.clone(), b.clone());
    let c = x2 + b0;
    let (c0,) = (c.clone(),);
    let _d = y0 + b1;
    let (_d0,) = (_d.clone(),);
    let d = _d0 + c0;
    let (der__d0, der_c0) = (d.clone(), d.clone());
    let der__d = der__d0;
    let (der_y0, der_b1) = (_d.clone(), _d.clone());
    let der_c = der_c0;
    let (der_x2, der_b0) = (c.clone(), c.clone());
    let der_b = der_b0 + der_b1;
    let der_x1 = 3. * b;
    let der_x0 = 7. * a;
    let der_y = der_y0;
    let der_x = der_x0 + der_x1 + der_x2;
    return (d, der_x, der_y);
}
```