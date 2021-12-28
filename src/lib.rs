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
//! Floats: `f32` & `f64` | ✓
//! Intgers: `u16`, `i16` etc. | 
//! 
//! All mentioned types have support for basic `+`, `-`, `*` and `/` operations if they are implemented.
//! 
//! Support means almost* all the native operations (operations implemented by the library which defines the type) are supported.
//! 
//! *The 'almost' exception is here since some operations may simply not be worth supporting given extremely limited usage, possible deprecation, etc.
//! 
//! For the specifics of operation support see the [rust-ad-core docs](https://docs.rs/rust-ad-core/).
//! 
//! ### Notes on internals
//! With `cargo expand` when an `.unwrap()` panics it doesn't give a line reference. Using unique panic messages makes locating errors easier. These should help debuging quite specifically (ctrl+f).
//!
//! ### Resources
//! - [Automatic Differentation in Rust](https://github.com/JonathanWoollett-Light/autodiff-book)
//! - [automatic-differentiation-worked-examples](http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/)
pub use rust_ad_macros::*;
