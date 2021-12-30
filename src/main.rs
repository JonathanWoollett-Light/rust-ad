#![allow(dead_code)]
extern crate rust_ad;
#[allow(unused_imports)]
use rust_ad::{forward, forward_autodiff};
#[allow(unused_imports)]
use rust_ad_core_macros::compose;

#[forward_autodiff]
fn log_fn(x: f32, y: f32) -> f32 {
    let c = 2f32 / y;
    let f = x + c;
    return f;
}
fn main() {
    // let test = compose!("-{0}/({1}*{1})","one","two");
    let (_x, _der_x, _der_y) = forward!(log_fn, 3f32, 2f32);
}
