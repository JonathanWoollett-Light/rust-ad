extern crate rust_ad;

use num::Zero;

// File for testing.

// #[rust_ad::forward_autodiff]
// fn forward(x: f64, y: f64) -> f32 {
//     let a = some_function(x + 4., y);
//     return a;
// }

// #[rust_ad::reverse_autodiff]
// fn forward(x: f32, y: i32) -> f32 {
//     let a = (x + 4.2f32).powi(y + 4i32);
//     return a;
// }

// #[rust_ad::forward_autodiff]
// fn function_name(x: f32, y: f32) -> f32 {
//     let a = 7.0f32 * x;
//     let b = 3f32 * x;
//     let c = x + b;
//     let d = y + b + c;
//     return d;
// }

#[rust_ad::forward_autodiff]
fn plus(x: f32) -> f32 {
    return x+1f32;
}


fn main() {}
