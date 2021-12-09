extern crate rad;
use rad::*;

#[forward_autodiff]
#[allow(non_snake_case)]
fn forward((x, y): (f32, f32)) -> (f32, f32) {
    let p = 7. * x;
    let r = 10. - y;
    let q = p * x * 5.;
    let v = 2. * p * q + 3. * r;
    return v;
}

fn main() {}
