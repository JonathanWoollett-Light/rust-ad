extern crate rust_ad;

// File for testing.

#[rust_ad::forward_autodiff]
fn multi(x: f32, y: f32) -> f32 {
    let a = x.powi(2i32);
    let b = x * 2f32;
    let c = 2f32 / y;
    let f = a + b + c;
    return f;
}

fn main() {}
