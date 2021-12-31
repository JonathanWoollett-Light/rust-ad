//! A restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.
//!
//! ### Status
//!
//! - [x] Forward Auto-differentiation
//! - [x] Reverse Auto-differentiation
//! - [x] `f32` & `f64` support*
//! - [ ] limited° [ndarray](https://github.com/rust-ndarray/ndarray) support*
//! - [ ] limited° [nalgebra](https://docs.rs/nalgebra/latest/nalgebra/) support*
//! - [ ] `if`, `if else` and `else` support
//! - [ ] `for`, `while` and `loop` support
//!
//! *`typeof` (e.g. [`decltype`](https://en.cppreference.com/w/cpp/language/decltype)) not being currently implemented in Rust makes support more difficult.
//!
//! °Support limited to the basic blas-like operations.
//!
//! Type/s | Support
//! --- | ---
//! Floats: `f32` & `f64` | `+`, `-`, `*`, `/` and most methods (e.g. `powf`).
//! Intgers: `u16`, `i16` etc. | `+`, `-`, `*` and `/`
//!
//! For the specifics of operation support see the [rust-ad-core docs](https://docs.rs/rust-ad-core/).
//!
//! ### Resources
//! - [Automatic Differentation in Rust](https://github.com/JonathanWoollett-Light/autodiff-book)
//! - [automatic-differentiation-worked-examples](http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/)
pub use rust_ad_macros::*;
