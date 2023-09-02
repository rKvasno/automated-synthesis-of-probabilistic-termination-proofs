use crate::pts::{linear_polynomial, location, variable_map};
use linear_polynomial::LinearPolynomial;
use location::LocationHandle;
use variable_map::Variable;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
#[repr(align(32))] // 32 bytes
pub struct Assignment(pub Variable, pub LinearPolynomial);

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct Transition {
    pub assignments: Vec<Assignment>,
    pub target: LocationHandle,
}

// #[cfg(test)]
// mod tests {
//     use std::mem;
//     use super::{Assignment, Transition};
//
//     #[test]
//     fn align_transition() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<Transition>(), 32);
//         }
//     }
//
//     #[test]
//     fn size_transition() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<Transition>(), 32);
//         }
//     }
//
//     #[test]
//     fn align_assignment() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::align_of::<Assignment>(), 32);
//         }
//     }
//     #[test]
//     fn size_assignment() {
//         if mem::size_of::<usize>() == 8{
//             assert_eq!(mem::size_of::<Assignment>(), 32);
//         }
//     }
// }
