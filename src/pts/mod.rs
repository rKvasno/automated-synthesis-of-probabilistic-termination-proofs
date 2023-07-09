mod guard;
mod location;
mod transition;

use crate::pts::location::{Location, LocationHandle};
use std::cell::RefCell;

pub struct Variable(Box<str>);
pub struct LinearPolynomial(Vec<f64>);

#[repr(align(32))] // 32 bytes
pub struct PTS<'a> {
    locations: Vec<RefCell<Location<'a>>>,
    variables: Vec<Variable>,
    initial: LocationHandle<'a>,
}

// #[cfg(test)]
// mod tests {
//     use std::mem;
//     use super::PTS;
//     
//     #[test]
//     fn align_pts() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<PTS>(), 32);
//         }
//     }
//     
//     #[test]
//     fn size_pts() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<PTS>(), 32);
//         }
//     }
// }

