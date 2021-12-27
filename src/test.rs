#![allow(dead_code)]

use crate::{forward, forward_autodiff};

#[forward_autodiff]
fn empty(x: f32) -> f32 {
    return x;
}

#[forward_autodiff]
fn plus(x: f32) -> f32 {
    return x + 3;
}

#[test]
fn empty_test() {
    let (x, der_x) = forward!(empty, 1., 1.);
    assert_eq!(x, 1.);
    assert_eq!(der_x, 1.);
}
#[test]
fn empty_test() {
    let (x, der_x) = forward!(empty, 1., 1.);
    assert_eq!(x, 1.);
    assert_eq!(der_x, 1.);
}
