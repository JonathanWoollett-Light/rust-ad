#![allow(dead_code)]

use crate::{forward, forward_autodiff};

#[forward_autodiff]
fn empty(x: f32) -> f32 {
    return x;
}
#[forward_autodiff]
fn plus(x: f32) -> f32 {
    return x + 1f32;
}
#[forward_autodiff]
fn quad(x: f32) -> f32 {
    let a = x.powi(2i32);
    let b = x * 2f32;
    let c = 2f32;
    let f = a + b + c;
    return f;
}
#[forward_autodiff]
fn multi(x: f32, y: f32) -> f32 {
    let a = x.powi(2i32);
    let b = x * 2f32;
    let c = 2f32 / y;
    let f = a + b + c;
    return f;
}

#[test]
fn empty_test() {
    let (x, der_x) = forward!(empty, 1f32, 1f32);
    assert_eq!(x, 1.);
    assert_eq!(der_x, 1.);
}
#[test]
fn plus_test() {
    let (x, der_x) = forward!(plus, 1f32, 1f32);
    assert_eq!(x, 2f32);
    assert_eq!(der_x, 1f32);
}
#[test]
fn quad_test() {
    let (x, der_x) = forward!(quad, 3f32, 1f32);
    assert_eq!(x, 17f32);
    assert_eq!(der_x, 8f32);
}
#[test]
fn multi_test() {
    let (f, der_x, der_y) = forward!(multi, 3f32, 5f32, 1f32, 1f32);
    assert_eq!(f, 15.4f32);
    assert_eq!(der_x, 8f32); // 2(x+1)
    assert_eq!(der_y, -0.08f32); // -2/y^2
}
