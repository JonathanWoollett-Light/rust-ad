#![allow(dead_code)]
extern crate rust_ad;
#[allow(unused_imports)]
use rust_ad::{forward, forward_autodiff, reverse, reverse_autodiff};
// #[allow(unused_imports)]
// use rust_ad_core_macros::compose;

// #[reverse_autodiff]
// fn complex(x: f32, y: f32, z: f32) -> f32 {
//     let a = x.powi(2i32);
//     let b = x * 2f32 / z;
//     let c = 2f32 / (z.sqrt() + y);
//     let f = a + b + c;
//     return f;
// }

#[forward_autodiff]
fn tuple_function(x1: f32, x2: f32, x3: f32, y1: f32, y2: f32, y3: f32) -> f32 {
    let a1 = x1 + y1;
    let a2 = x2 + y2;
    let a3 = x3 + y3;
    let b = a1 + a2;
    return b;
}

// #[forward_autodiff]
// fn array_function(x: Array1<f32>, y: Array1<f32>) -> f32 {
//     let a = x + y;
//     let b = a.sum();
//     return b;
// }

fn main() {
    // let test = compose!("-{0}/({1}*{1})","one","two");
    // let (_x, _der_x, _der_y) = forward!(log_fn, 3f32, 2f32);
}
