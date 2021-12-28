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
    /// f/δx|y=5 = 2x + 2
    /// f/δy|x=3 = 2
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

    #[test]
    fn powi_test() {
        let (f, der_x, der_y) = forward!(powi_fn,3f32,5f32);
        assert_eq!(f, 63.4f32);
        assert_eq!(der_x, 60f32);
        assert_eq!(der_y, -0.08f32);

        /// Equations:
        /// - f = x^2 + 2x^3 + 2/y
        /// - dx|y=5 = 2x(1+3x)
        /// - dy|x=3 = -2/y^2
        /// Values:
        /// - f(3,5) = 9 + 54 + 2.5 = 63.4
        /// - dx|y=5(3) = 60
        /// - dy|x=3(5) = -0.08
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
        let (f, der_x, der_y) = forward!(powf_fn,3f32,5f32);
        assert_eq!(f, 63.4f32);
        assert_eq!(der_x, 60f32);
        assert_eq!(der_y, -0.08f32);

        /// Equations:
        /// - f = x^2 + 2x^3 + 2/y
        /// - dx|y=5 = 2x(1+3x)
        /// - dy|x=3 = -2/y^2
        /// Values:
        /// - f(3,5) = 9 + 54 + 2.5 = 63.4
        /// - dx|y=5(3) = 60
        /// - dy|x=3(5) = -0.08
        #[forward_autodiff]
        fn powf_fn(x: f32, y: f32) -> f32 {
            let a = x.powf(2f32);
            let b = x * 2f32 * a;
            let c = 2f32 / y;
            let f = a + b + c;
            return f;
        }
    }
}
