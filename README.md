# RustAD - Rust Auto-Differentiation

[![Crates.io](https://img.shields.io/crates/v/rust-ad)](https://crates.io/crates/rust-ad)
[![lib.rs.io](https://img.shields.io/crates/v/rust-ad?color=blue&label=lib.rs)](https://lib.rs/crates/rust-ad)
[![docs](https://img.shields.io/crates/v/rust-ad?color=yellow&label=docs)](https://docs.rs/rust-ad)

A restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.

**Why would I use this over \<insert library\>?** You wouldn't, not yet anyway. I'd say wait until support for ndarray is more comprehensive, then this becomes probably the most convenient Rust AutoDiff library.

## Status

- [x] Forward Auto-differentiation
- [x] Reverse Auto-differentiation
- [x] Numerical primitives (e.g. `f32`, `u32` etc.) support*
- [ ] limited° [ndarray](https://github.com/rust-ndarray/ndarray) support*
- [ ] limited° [nalgebra](https://docs.rs/nalgebra/latest/nalgebra/) support*
- [ ] `if`, `if else` and `else` support
- [ ] `for`, `while` and `loop` support

*`typeof` (e.g. [`decltype`](https://en.cppreference.com/w/cpp/language/decltype)) not being currently implemented in Rust makes support more difficult.

°Support limited to the basic blas-like operations.

## Application

Auto-differentiation is implemented via 2 attribute procedural macros, e.g.

```rust
#[rust_ad::forward_autodiff]
fn function_name(x: f32, y: f32) -> f32 {
    let p = 7.0f32 * x;
    let r = 10f32 - y;
    let q = p * x * 5.f32;
    let v = 2f32 * p * q + 3f32 * r;
    return v;
}
```
```rust
#[rust_ad::reverse_autodiff]
fn function_name(x: f32, y: f32) -> f32 {
    let a = 7f32 * x;
    let b = 3.0f32 * x;
    let c = x + b;
    let d = y + b + c;
    return d;
}
```
