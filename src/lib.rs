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
//! Integers: `u16`, `i16` etc. | `+`, `-`, `*` and `/`
//!
//! For the specifics of operation support see the [rust-ad-core docs](https://docs.rs/rust-ad-core/).
//!
//!
//! ### Multi-variate output format
//!
//! For some code
//! ```
//! assert_eq!(
//!     ((8f32, 16f32), ((1f32, 1f32), (2f32, 2f32))),
//!     rust_ad::forward!(tuple_function, 3f32, 5f32)
//! );
//! #[rust_ad::forward_autodiff]
//! fn tuple_function(x: f32, y: f32) -> (f32, f32) {
//!     let a1 = x + y;
//!     let b = 2f32 * a1;
//!     return (a1, b);
//! }
//! ```
//! - `(8f32, 16f32)` is the output value (`(a1, b)`).
//! - `(1f32, 1f32)` represents the affects/derivatives of `x` on the return elements (`a1`, `b`). The first element representing `x` affect on the first return element (`a1`) and the second representing `x` affect on second return element (`b`).
//! - `(2f32, 2f32)` represents the affects/derivatives of `y`.
//!
//!
//! ### Resources
//! - [Automatic Differentiation in Rust](https://github.com/JonathanWoollett-Light/autodiff-book)
//! - [automatic-differentiation-worked-examples](http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/)
pub use rust_ad_macros::*;
