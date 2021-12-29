#![allow(dead_code)]

extern crate rust_ad;

// File for testing.

// #[rust_ad::reverse_autodiff]
// fn empty(x: f32) -> f32 {
//     return x;
// }

#[rust_ad::forward_autodiff]
fn multi(x: f32, y: f32) -> f32 {
    let a = x.blah(2i32);
    let b = x * 2f32;
    let c = 2f32 / y;
    let f = a + b + c;
    return f;
}

fn main() {
    let (_f, _der_x, _der_y) = rust_ad::forward!(multi, 3f32, 5f32);
}
