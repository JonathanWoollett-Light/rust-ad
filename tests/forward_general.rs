#![allow(dead_code)]
use rust_ad::*;
const TOLERANCE: f32 = 0.01;
pub fn is_near(a: f32, b: f32) -> Result<(), String> {
    if (a - b).abs() < TOLERANCE {
        Ok(())
    } else {
        Err(format!("{} is not near {}", a, b))
    }
}

#[test]
fn empty_test() {
    let (x, der_x) = forward!(empty, 1f32);
    assert_eq!(x, 1.);
    assert_eq!(der_x, 1.);

    #[forward_autodiff]
    fn empty(x: f32) -> f32 {
        return x;
    }
}
#[test]
fn plus_test() {
    let (x, der_x) = forward!(plus, 1f32);
    assert_eq!(x, 2f32);
    assert_eq!(der_x, 1f32);

    #[forward_autodiff]
    fn plus(x: f32) -> f32 {
        return x + 1f32;
    }
}
#[test]
fn quad_test() {
    let (x, der_x) = forward!(quad, 3f32);
    assert_eq!(x, 17f32);
    assert_eq!(der_x, 8f32);

    #[forward_autodiff]
    fn quad(x: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32;
        let c = 2f32;
        let f = a + b + c;
        return f;
    }
}
#[test]
fn multi_test() {
    let (f, der_x, der_y) = forward!(multi, 3f32, 5f32);
    assert_eq!(f, 15.4f32);
    assert_eq!(der_x, 8f32); // 2(x+1)
    assert_eq!(der_y, -0.08f32); // -2/y^2

    /// f = x^2 + 2x + 2/y
    /// δx|y=5 = 2x + 2
    /// δy|x=3 = 2
    #[forward_autodiff]
    fn multi(x: f32, y: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}

#[test]
fn complex_test() {
    let (f, der_x, der_y, der_z) = forward!(complex, 3f32, 5f32, 7f32);
    is_near(f, 10.1187260448).unwrap();
    is_near(der_x, 6.28571428571).unwrap();
    is_near(der_y, -0.034212882033).unwrap();
    is_near(der_z, -0.128914606556).unwrap();

    // f(x,y,z) = x^2 + 2x/z + 2/(y+z^0.5)
    // ∂x = 2(x+1/z)
    // ∂y = -2 / (y+z^0.5)^2
    // ∂z = -2x/z^2 -1/(z^0.5 * (y+z^0.5)^2)
    // Therefore:
    // f(3,5,7) = 10.1187260448
    // ∂x| = 6.28571428571
    // ∂y| = −0.034212882033
    // ∂z| = −0.128914606556
    #[forward_autodiff]
    fn complex(x: f32, y: f32, z: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32 / z;
        let c = 2f32 / (z.sqrt() + y);
        let f = a + b + c;
        return f;
    }
}
