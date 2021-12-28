/// General tests for forward auto-diff
#[cfg(test)]
mod forward_general {
    #![allow(dead_code)]
    use rust_ad::{forward, forward_autodiff};

    #[forward_autodiff]
    fn empty(x: f32) -> f32 {
        return x;
    }
    #[forward_autodiff]
    fn plus(x: f32) -> f32 {
        return x + 1f32;
    }
    #[forward_autodiff]
    fn quad(x: f32) -> f32 {
        let a = x.powi(2i32);
        let b = x * 2f32;
        let c = 2f32;
        let f = a + b + c;
        return f;
    }
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

    #[test]
    fn empty_test() {
        let (x, der_x) = forward!(empty, 1f32);
        assert_eq!(x, 1.);
        assert_eq!(der_x, 1.);
    }
    #[test]
    fn plus_test() {
        let (x, der_x) = forward!(plus, 1f32);
        assert_eq!(x, 2f32);
        assert_eq!(der_x, 1f32);
    }
    #[test]
    fn quad_test() {
        let (x, der_x) = forward!(quad, 3f32);
        assert_eq!(x, 17f32);
        assert_eq!(der_x, 8f32);
    }
    #[test]
    fn multi_test() {
        let (f, der_x, der_y) = forward!(multi, 3f32, 5f32);
        assert_eq!(f, 15.4f32);
        assert_eq!(der_x, 8f32); // 2(x+1)
        assert_eq!(der_y, -0.08f32); // -2/y^2
    }
}

// TODO Tests for rest of operations.
/// Tests for specific methods for forward auto-diff
///
/// Replacing `my_method` with the respective method.
/// ```ignore
/// #[forward_autodiff]
/// fn base(x: f32, y: f32) -> f32 {
///     let a = x.my_method(2i32);
///     let b = x * 2f32 * a;
///     let c = 2f32 / y;
///     let f = a + b + c;
///     return f;
/// }
/// ```
#[cfg(test)]
mod forward_operations {
    #![allow(dead_code)]
    use rust_ad::{forward, forward_autodiff};

    /// Tolerance
    const TOLERANCE: f32 = 0.01;
    fn is_near(a: f32, b: f32) -> Result<(), String> {
        if (a - b).abs() < TOLERANCE {
            Ok(())
        } else {
            Err(format!("{} is not near {}", a, b))
        }
    }

    #[test]
    fn powi_test() {
        let (f, der_x, der_y) = forward!(powi_fn, 3f32, 5f32);
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
        #[forward_autodiff]
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
        let (f, der_x, der_y) = forward!(powf_fn, 3f32, 5f32);
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
        #[forward_autodiff]
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
        let (f, der_x, der_y) = forward!(sqrt_fn, 3f32, 5f32);
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
        #[forward_autodiff]
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
        let (f, der_x, der_y) = forward!(ln_fn, 3f32, 5f32);
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
        #[forward_autodiff]
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
        let (f, der_x, der_y) = forward!(log_fn, 3f32, 5f32);
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
        #[forward_autodiff]
        fn log_fn(x: f32, y: f32) -> f32 {
            let a = x.log(2f32);
            let b = x * 2f32 * a;
            let c = 2f32 / y;
            let f = a + b + c;
            return f;
        }
    }
}
