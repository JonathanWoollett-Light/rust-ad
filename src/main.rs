#![allow(dead_code)]

extern crate rust_ad;

use rust_ad_core_macros::*;
use rust_ad_core::*;

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

forward_derivative_macro!(add_f32,"0f32","1f32","1f32");

fn main() {
    // let temp_str = "whatever";
    // let temp2 = String::from("yu boi");
    // compose!(
    //     "{0} hello {1} goodbye {2} bruh",
    //     "we starting",
    //     temp_str,
    //     temp2
    // );
}
