extern crate rust_ad;

// File for testing.

#[rust_ad::reverse_autodiff]
fn forward(x: f64, y: f64) -> f64 {
    let a = 7. * x;
    let b = 3. * x;
    let c = x + b;
    let d = y + b + c;
    return d;
}

fn main() {}
