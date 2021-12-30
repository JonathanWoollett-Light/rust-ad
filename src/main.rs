#![allow(dead_code)]

extern crate rust_ad;

use rust_ad_core::*;
use rust_ad_core_macros::*;

// File for testing.

// #[rust_ad::reverse_autodiff]
// fn empty(x: f32) -> f32 {
//     return x;
// }

// #[rust_ad::forward_autodiff]
// fn multi(x: f32, y: f32) -> f32 {
//     let a = x.blah(2i32);
//     let b = x * 2f32;
//     let c = 2f32 / y;
//     let f = a + b + c;
//     return f;
// }

// forward_derivative_macro!(add_f32, "0f32", "1f32", "1f32");

forward_derivative_macro!(
    powi_f32,
    "0f32",
    "{1} * {0}.powi({1} - 1i32)",
    "{0}.powi({1}) * {0}.ln()"
);

fn main() {
    // let args = ["whatever", "yu boi"];
    // compose!(
    //     "hello {0} goodbye {1} someboi {2}",
    //     args[0],
    //     "testing this",
    //     args[1]
    // );
}
