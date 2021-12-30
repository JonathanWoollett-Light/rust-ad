#![allow(dead_code)]

extern crate rust_ad;

use rust_ad::{forward_autodiff,forward};

#[forward_autodiff]
fn quad(x: f32) -> f32 {
    let a = x.powi(2i32);
    let b = x * 2f32;
    let c = 2f32;
    let f = a + b + c;
    return f;
}

fn main() {
    let (x, der_x) = forward!(quad, 3f32);
    assert_eq!(x, 17f32);
    assert_eq!(der_x, 8f32);
}
