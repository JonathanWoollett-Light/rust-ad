extern crate rust_ad;
use rust_ad::*;

// #[reverse_autodiff]
// fn forward((x, y): (f32, f32)) -> f32 {
//     let p = 7. * x;
//     let r = 10. - y;
//     let q = p * x * 5.;
//     let v = 2. * p * q + 3. * r;
//     return v;
// }

#[reverse_autodiff]
fn forward(x: f32, y: f32) -> f32 {
    let a = 7. * x;
    let b = 3. * x;
    let c = x + b;
    let d = y + b + c;
    return d;
}
// fn forward(x: f32, y: f32) -> f32 {
//     let a = 7. * x;
//     let b = 3. * x;
//     let c = x + b;
//     let _d = y + b;
//     let d = _d + c
// }

fn main() {
    // dup!(x,2);
}

// fn forward2(x: f32, y: f32, der_d: f32) {
//     let (x0, x1, x2) = dup!(x, 3);
//     let a = 7. * x0;
//     let b = 3. * x1;
//     let (b0, b1) = dup!(b, 2);
//     let c = x2 + b0;
//     let _d = y + b1;
//     let d = _d + c;
//     // --------------------------------------
//     let (der__d, der_c) = dup!(d, 2);
//     let (der_y, der_b1) = dup!(der__d, 2);
//     let (der_x2, der_b0) = dup!(der_c, 2);
//     let der_b = der_b0 + der_b1;
//     let der_x1 = 3. * der_b;
//     // let der_x0 = 7. * der_a;
//     let der_x = /* x0 + */ x1 + x2;
//     (der_x, der_y)
// }
