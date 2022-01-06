#![allow(dead_code)]
extern crate rust_ad;
#[allow(unused_imports)]
use rust_ad::{forward, forward_autodiff, reverse, reverse_autodiff};

// To run in:
// - debug: `cargo rustc --bin rust-ad --profile=dev -- -Zunpretty=expanded`
// - release: `cargo rustc --bin rust-ad --profile=release -- -Zunpretty=expanded`
// (I think `cargo expand --bin rust-ad` just does debug)

#[reverse_autodiff]
fn tuple_function(x: f32, y: f32) -> (f32, f32) {
    let a1 = x + y;
    let b = a1 + 2f32;
    let c = b;
    return (c, a1);
}

fn main() {}
