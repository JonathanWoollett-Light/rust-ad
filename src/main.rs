extern crate macro_test;
use macro_test::*;

trait Forward {
    type IN;
    type OUT;
    fn forward(&self, input: Self::IN) -> Self::OUT;
}
trait Backward {
    type LOSS;
    fn backward(&self, loss: Self::LOSS) -> Self;
}

#[allow(dead_code)]
struct SimpleStruct {
    // 2->3->1
    x: f32,
    y: f32,
}
#[autodiff]
impl Forward for SimpleStruct {
    type IN = ();
    type OUT = f32;
    fn forward(&self, _: Self::IN) -> Self::OUT {
        let p = 7 * self.x;
        let r = 1 / self.y;
        let q = p * self.x * 5;
        let v = 2 * p * q + 3 * r;
        v
    }
}
impl SimpleStruct {
    fn backward(&self) {
        let p = 7 * self.x;
        let r = 1 / self.y;
        let q__0 = p * self.x;
        let q = q__0 * 5;
        let v__0 = 2 * p;
        let v__1 = v__0 * q;
        let v__2 = 3 * r;
        let v = v__1 + v__2;
    }
}

fn main() {
    // println!("{}", answer());
    // println!("{}", testPrint());
}

// #[allow(dead_code)]
// struct MyStruct {
//     // 2->3->1
//     weights: ([[f32; 2]; 3], [f32; 3]),
//     biases: ([f32; 3], f32),
// }

// #[proc_macro_attribute]
// impl Forward for MyStruct {
//     type IN = (f32, f32);
//     type OUT = f32;
//     fn forward(&self, (a_0, a_1): Self::IN) -> Self::OUT {
//         // 2->3
//         let b_0 = (a_0 * self.weights.0[0][0] + a_1 * self.weights.0[0][1]) + self.biase.0[0];
//         let b_1 = (a_0 * self.weights.0[1][0] + a_1 * self.weights.0[1][1]) + self.biases.0[1];
//         let b_2 = (a_0 * self.weights.0[2][0] + a_1 * self.weights.0[2][1]) + self.biases.0[2];
//         // 3->1
//         let c_0 = (b_0 * self.weights.1[0] + b_1 * self.weights.1[1] + b_2 * self.weights.1[2])
//             + self.biase.1;
//         // Return
//         c_0
//     }
// }
// impl Backward for MyStruct {
//     type LOSS = f32;
//     fn backward(&self, loss: Self::LOSS) -> Self {
//         let b_0__1 = a_0 * self.weights.0[0][0];
//         let b_0__2 = a_1 * self.weights.0[0][1];
//         let b_0__3 = b_0__1 + b_0__2;
//         let b_0 = b_0__3 + self.biase.0[0];

//         let b_1__1 = a_0 * self.weights.0[0][0];
//         let b_1__2 = a_1 * self.weights.0[0][1];
//         let b_1__3 = b_1__1 + b_1__2;
//         let b_1 = b_1__3 + self.biase.0[0];

//         let b_1__1 = a_0 * self.weights.0[0][0];
//         let b_1__2 = a_1 * self.weights.0[0][1];
//         let b_1__3 = b_1__1 + b_1__2;
//         let b_1 = b_1__3 + self.biase.0[0];

//         Self {
//             weights: (
//                 loss * a_0 + loss * a_1 + loss*a_2
//             ),
//             biases: (
//                 loss
//             )
//         }
//     }
// }
