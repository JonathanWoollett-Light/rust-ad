//! A restrictive WIP beginnings of a library attempting to implement auto-differentiation in Rust.
//! ## Status
//! This library is very much a WIP and thus extremely rough, temperamental and inconsistent.
//!
//! I would not recommend you use it at the moment, it is only public to allow the possibility of collaborative work on it.
//!
//! - [x] Forward Auto-differentiation
//! - [x] Reverse Auto-differentiation
//! - [x] `f32` & `f64` support*
//! - [ ] [ndarray](https://github.com/rust-ndarray/ndarray) support*
//! - [ ] Non-primitive operations (specifically [`ndarray::Array2::dot()`](https://docs.rs/ndarray/latest/ndarray/struct.ArrayBase.html#method.dot-1))
//! - [ ] [nalgebra](https://docs.rs/nalgebra/latest/nalgebra/) support*
//! - [ ] `if`, `if else` and `else` support
//! - [ ] `for`, `while` and `loop` support
//!
//! *Becuase `typeof` (e.g. [`decltype`](https://en.cppreference.com/w/cpp/language/decltype)) is currently not implemented in Rust this makes supporting different types and more complex operations a massive pain. E.g. to figure out what operation `a + b` is actually doing I need to figure out the types `a` and `b`. Currently I'm considering requiring the manual annotatation of all types, `let a: f32 = b + c;` instead of `let a = b + c;` etc.
//!
//! ## Notes on internals
//! `.expect()` is used extensively in internals, this is since when using `cargo expand` when an `.unwrap()` would panic it gives no line reference thus it can be difficult to locate errors. Thus by using unique `.expect()` messages this makes locating errors easier.

pub use rust_ad_macros::*;
