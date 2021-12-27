use crate::{forward, forward_autodiff};

#[forward_autodiff]
fn empty(x: f32) -> f32 {
    return x;
}

#[test]
fn empty_test() {
    assert_eq!(forward!(empty, 1.), 1.);
}
