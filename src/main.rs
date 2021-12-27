extern crate rust_ad;

// File for testing.

// #[rust_ad::reverse_autodiff]
// fn empty(x: f32) -> f32 {
//     return x;
// }

#[rust_ad::reverse_autodiff]
    fn plus(x: f32) -> f32 {
        return x + 1f32;
    }

fn main() {}
