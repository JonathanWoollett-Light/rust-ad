extern crate rust_ad;
use rust_ad::*;

// #[backward_autodiff]
// fn forward((x, y): (f32, f32)) -> f32 {
//     let p = 7. * x;
//     let r = 10. - y;
//     let q = p * x * 5.;
//     let v = 2. * p * q + 3. * r;
//     return v;
// }

#[backward_autodiff]
fn forward(x: f32, y: f32) -> f32 {
    let a = 7. * x;
    let b = 3. * x;
    let c = x + b;
    let d = y + b + c;
}

fn main() {}
