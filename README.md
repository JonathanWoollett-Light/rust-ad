# RustAD - Rust Auto-Differentiation

[![Crates.io](https://img.shields.io/crates/v/rust-ad)](https://crates.io/crates/rust-ad)
[![lib.rs.io](https://img.shields.io/crates/v/rust-ad?color=blue&label=lib.rs)](https://lib.rs/crates/rust-ad)
[![docs](https://img.shields.io/crates/v/rust-ad?color=yellow&label=docs)](https://docs.rs/rust-ad)

A restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.

**Why would I use this over \<insert library\>?** You wouldn't, not yet anyway. I'd say wait until support for ndarray is more comprehensive, then this becomes probably the most convenient Rust AutoDiff library.

**It's all messy be warned.**

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
fn multi_test() {
    let (f, (der_x, der_y)) = forward!(multi, 3f32, 5f32);
    assert_eq!(f, 15.4f32);
    assert_eq!(der_x, 8f32);
    assert_eq!(der_y, -0.08f32);

    /// f = x^2 + 2x + 2/y
    /// δx|y=5 = 2x + 2
    /// δy|x=3 = -2/y^2
    #[forward_autodiff]
    fn multi(x: f32, y: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
```
```rust
fn multi_test() {
    let (f, (der_x, der_y)) = reverse!(multi, (3f32, 5f32), (1f32));
    assert_eq!(f, 15.4f32);
    assert_eq!(der_x, 8f32);
    assert_eq!(der_y, -0.08f32);

    /// f = x^2 + 2x + 2/y
    /// δx|y=5 = 2x + 2
    /// δy|x=3 = -2/y^2
    #[reverse_autodiff]
    fn multi(x: f32, y: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
```
