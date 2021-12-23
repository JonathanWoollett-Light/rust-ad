extern crate rust_ad;

// File for testing.

// #[rust_ad::forward_autodiff]
// fn forward(x: f64, y: f64) -> f32 {
//     let a = some_function(x + 4., y);
//     return a;
// }

#[rust_ad::forward_autodiff]
fn forward(x: f32, y: i32) -> f32 {
    let a = (x + 4.).powi(y + 4.);
    return a;
}

fn main() {}
