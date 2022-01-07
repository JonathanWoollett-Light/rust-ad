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
fn powi_test() {
    let (f, (der_x, der_y)) = reverse!(powi_fn, (3f32, 5f32), (1f32));
    assert_eq!(f, 63.4f32);
    assert_eq!(der_x, 60f32);
    assert_eq!(der_y, -0.08f32);

    /// Equations:
    /// - f = x^2 + 2x^3 + 2/y
    /// - ∂x|y=5 = 2x(1+3x)
    /// - ∂y|x=3 = -2/y^2
    /// Values:
    /// - f(3,5) = 9 + 54 + 2.5 = 63.4
    /// - ∂x|y=5(3) = 60
    /// - ∂y|x=3(5) = -0.08
    #[reverse_autodiff]
    fn powi_fn(x: f32, y: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32 * a;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
#[test]
fn powf_test() {
    let (f, (der_x, der_y)) = reverse!(powf_fn, (3f32, 5f32), (1f32));
    assert_eq!(f, 63.4f32);
    assert_eq!(der_x, 60f32);
    assert_eq!(der_y, -0.08f32);

    /// Equations:
    /// - f = x^2 + 2x^3 + 2/y
    /// - ∂x|y=5 = 2x(1+3x)
    /// - ∂y|x=3 = -2/y^2
    /// Values:
    /// - f(3,5) = 9 + 54 + 2.5 = 63.4
    /// - ∂x|y=5(3) = 60
    /// - ∂y|x=3(5) = -0.08
    #[reverse_autodiff]
    fn powf_fn(x: f32, y: f32) -> f32 {
        let a = x.powf(2f32);
        let b = x * 2f32 * a;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
#[test]
fn sqrt_test() {
    let (f, (der_x, der_y)) = reverse!(sqrt_fn, (3f32, 5f32), (1f32));
    is_near(f, 12.524355653f32).unwrap();
    is_near(der_x, 5.4848275573f32).unwrap();
    is_near(der_y, -0.08f32).unwrap();

    /// Equations:
    /// - f = x^0.5 + 2x*x^0.5 + 2/y
    /// - ∂x|y=5 = (6x+1)/(2x^0.5)
    /// - ∂y|x=3 = -2/y^2
    /// Values:
    /// - f(3,5) = 12.524355653
    /// - ∂x|y=5(3) = 5.4848275573
    /// - ∂y|x=3(5) = -0.08
    #[reverse_autodiff]
    fn sqrt_fn(x: f32, y: f32) -> f32 {
        let a = x.sqrt();
        let b = x * 2f32 * a;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
#[test]
fn ln_test() {
    let (f, (der_x, der_y)) = reverse!(ln_fn, (3f32, 5f32), (1f32));
    is_near(f, 8.09028602068f32).unwrap();
    is_near(der_x, 4.53055791067f32).unwrap();
    is_near(der_y, -0.08f32).unwrap();

    /// Equations:
    /// - f = ln(x) + 2x*ln(x)+ 2/y
    /// - ∂x|y=5 = (1/x) + 2*log(x)+2
    /// - ∂y|x=3 = -2/y^2
    /// Values:
    /// - f(3,5) = 8.09028602068
    /// - ∂x|y=5(3) = 4.53055791067
    /// - ∂y|x=3(5) = -0.08
    #[reverse_autodiff]
    fn ln_fn(x: f32, y: f32) -> f32 {
        let a = x.ln();
        let b = x * 2f32 * a;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
#[test]
fn log_test() {
    let (f, (der_x, der_y)) = reverse!(log_fn, (3f32, 5f32), (1f32));
    is_near(f, 11.494737505f32).unwrap();
    is_near(der_x, 6.53621343018f32).unwrap();
    is_near(der_y, -0.08f32).unwrap();

    /// Equations:
    /// - f = log2(x) + 2x*log2(x)+ 2/y
    /// - ∂x|y=5 = ( 2x + 2x*ln(x)+1 ) / (x*ln(2))
    /// - ∂y|x=3 = -2/y^2
    /// Values:
    /// - f(3,5) = 11.494737505
    /// - ∂x|y=5(3) = 6.53621343018
    /// - ∂y|x=3(5) = -0.08
    #[reverse_autodiff]
    fn log_fn(x: f32, y: f32) -> f32 {
        let a = x.log(2f32);
        let b = x * 2f32 * a;
        let c = 2f32 / y;
        let f = a + b + c;
        return f;
    }
}
