# RustAD - Rust Auto-Differentiation

A super restrictive rough WIP beginnings of a library attempting to implement auto-differentiation in Rust.
  
### `forward_autodiff`

Currently only forward auto-differentiation is (roughly) implemented via a procedural attribute macro `forward_autodiff`:
```rust
#[forward_autodiff]
fn forward((x, y): (f32, f32)) -> f32 {
    let p = 7. * x;
    let r = 10. - y;
    let q = p * x * 5.;
    let v = 2. * p * q + 3. * r;
    return v;
}
```

Produces:
```rust
fn forward((x, y): (f32, f32), (der_x, der_y): (f32, f32)) -> (f32, f32) {
    let p = 7. * x;
    let der_p = x * 0f32 + 7. * der_x;
    let r = 10. - y;
    let der_r = 0f32 - der_y;
    let _q = p * x;
    let der__q = x * der_p + p * der_x;
    let q = _q * 5.;
    let der_q = 5. * der__q + _q * 0f32;
    let __v = 2. * p;
    let der___v = p * 0f32 + 2. * der_p;
    let _v = __v * q;
    let der__v = q * der___v + __v * der_q;
    let v_ = 3. * r;
    let der_v_ = r * 0f32 + 3. * der_r;
    let v = _v + v_;
    let der_v = der__v + der_v_;
    return (v, der_v);
}
```
### `unweave`

A procedural attribute macro `unweave` is also exposed:

```rust
#[unweave]
fn forward((x, y): (f32, f32)) -> f32 {
    let p = 7. * x;
    let r = 10. - y;
    let q = p * x * 5.;
    let v = 2. * p * q + 3. * r;
    return v;
}
```
Produces:
```rust
fn forward((x, y): (f32, f32)) -> f32 {
    let p = 7. * x;
    let r = 10. - y;
    let _q = p * x;
    let q = _q * 5.;
    let __v = 2. * p;
    let _v = __v * q;
    let v_ = 3. * r;
    let v = _v + v_;
    return v;
}
```
