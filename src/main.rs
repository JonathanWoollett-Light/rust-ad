extern crate rust_ad;

// File for testing.

#[rust_ad::forward_autodiff]
fn forward(x: f64, y: f64) -> f32 {
    let a = some_function(x + 4., y);
    return a;
}

fn main() {}
