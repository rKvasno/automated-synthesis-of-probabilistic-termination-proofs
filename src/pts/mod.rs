pub mod guard;
pub mod location;
pub mod transition;
pub mod inequality;
pub mod variable_map;
pub mod linear_polynomial;

use location::Locations;
use variable_map::VariableMap;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct PTS {
    pub locations: Locations,
    pub variables: VariableMap,
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

