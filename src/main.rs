#![allow(dead_code)]
extern crate rust_ad;
#[allow(unused_imports)]
use rust_ad::{forward, forward_autodiff, reverse, reverse_autodiff, unweave};

// To run in:
// - debug: `cargo rustc --bin rust-ad --profile=dev -- -Zunpretty=expanded`
// - release: `cargo rustc --bin rust-ad --profile=release -- -Zunpretty=expanded`
// (I think `cargo expand --bin rust-ad` just does debug)

// #[forward_autodiff]
// fn empty(x: f32) -> f32 {
//     return x;
// }

// #[forward_autodiff]
// fn plus(x: f32) -> f32 {
//     return x + 1f32;
// }

// #[forward_autodiff]
// fn quad(x: f32) -> f32 {
//     let a = x.powi(2i32);
//     let b = x * 2f32;
//     let c = 2f32;
//     let f = a + b + c;
//     return f;
// }

// #[forward_autodiff]
// fn multi(x: f32, y: f32) -> f32 {
//     let a = x.powi(2i32);
//     let b = x * 2f32;
//     let c = 2f32 / y;
//     let f = a + b + c;
//     return f;
// }

// #[forward_autodiff]
// fn complex(x: f32, y: f32, z: f32) -> f32 {
//     let a = x.powi(2i32);
//     let b = x * 2f32 / z;
//     let c = 2f32 / (z.sqrt() + y);
//     let f = a + b + c;
//     return f;
// }

// #[reverse_autodiff]
// fn powi_fn(x: f32, y: f32) -> f32 {
//     let a = x.powi(2i32);
//     let b = x * 2f32 * a;
//     let c = 2f32 / y;
//     let f = a + b + c;
//     return f;
// }

fn main() {
    // assert_eq!(
    //     ((8f32, 16f32), ((1f32, 1f32), (2f32, 2f32))),
    //     forward!(tuple_function, 3f32, 5f32)
    // );
}
