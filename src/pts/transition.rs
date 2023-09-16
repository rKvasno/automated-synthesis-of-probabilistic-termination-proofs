use crate::pts::{linear_polynomial, location, variable_map};
use linear_polynomial::LinearPolynomial;
use location::LocationHandle;
use variable_map::Variable;

use super::{variable_map::VariableMap, DisplayLabel};

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

impl DisplayLabel for Assignment {
    fn label(&self, variable_map: &VariableMap) -> String {
        let mut label = self.0.to_string();
        label.push_str(" = ");
        label.push_str(self.1.label(variable_map).as_str());

        label
    }
}

#[cfg(test)]
mod tests {
    use crate::pts::{
        linear_polynomial::{constant::Constant, LinearPolynomial},
        variable_map::{Variable, VariableMap},
        DisplayLabel,
    };

    use super::Assignment;

    #[test]
    fn label() {
        let assign = Assignment(
            Variable::new("test"),
            LinearPolynomial::mock(vec![Constant(0.0)]),
        );
        let map = VariableMap::mock(Default::default());
        assert_eq!(assign.label(&map), "test = 0");

        let assign = Assignment(
            Variable::new("b"),
            LinearPolynomial::mock(vec![Constant(-5.0), Constant(-2.0), Constant(1.0)]),
        );
        let map = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);
        assert_eq!(assign.label(&map), "b = -2a + b - 5");
    }
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
