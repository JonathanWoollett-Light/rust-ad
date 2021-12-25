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
//! ### Type support
//! - **Primitive support:** `+`, `-`, `*` and `/` operations are supported.
//! - **Extended support:** All operations from [std::ops] are supproted.
//! - **Near-full support:** Almost* all native operations (operations implemented by the library which defines the type) are supported.
//!
//! Please note these just specify a minimum set of operations, full details of other specific supported operations are given below.
//!
//! *The 'almost' exception is here since some operations may simply not be worth supporting given extremely limited usage, possible deprecation, etc.
//!
//! <table>
//!   <tr>
//!   <td style="border-left-style: none;">Primitive support</td>
//!   <td style="border-right-style: none;"><code>f32</code>,<code>f64</code>,<code>i8</code>,<code>i16</code>,<code>i32</code>,<code>i64</code>,<code>i128</code>,<code>u8</code>,<code>u16</code>,<code>u32</code>,<code>u64</code>,<code>u128</code></td>
//! </tr>
//! <tr>
//!   <td style="border-left-style: none;">Extended support</td>
//!   <td style="border-right-style: none;"></td>
//! </tr>
//! <tr>
//!   <td style="border-left-style: none;">Near-full support</td>
//!   <td style="border-right-style: none;"></td>
//! </tr>
//! </table>
//!
//! ### Specific operation support
//! - `f32`: [`powi`](https://doc.rust-lang.org/std/primitive.f32.html#method.powi)
//! - `f64`: [`powi`](https://doc.rust-lang.org/std/primitive.f64.html#method.powi)
//!
//! ### Notes on internals
//! With `cargo expand` when an `.unwrap()` panics it doesn't give a line reference. Using unique panic messages makes locating errors easier. These should help debuging quite specifically (ctrl+f).
//!
//! ### Resources
//! - [automatic-differentiation-worked-examples](http://h2.jaguarpaw.co.uk/posts/automatic-differentiation-worked-examples/)
pub use rust_ad_macros::*;
