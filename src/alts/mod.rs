mod arithmetic;
mod formula;
mod guard;

use std::fmt;
use std::ptr::NonNull;
use crate::alts::guard::Guard;
use crate::alts::formula::Formula;

// by using *mut instead of a reference, I disable the borrow checker,
// allowing me to have multiple mutable pointers to the same location (aliasing)
// and marking all locations as UnsafeCells (allows mutability, while aliased)
#[repr(align(64))]
enum Location {
    Instruction{invariant: Formula,
                next: Option<NonNull<Location>>},
    Branching{invariant: Formula,
              guard: Guard,
              left: Option<NonNull<Location>>,
              right: Option<NonNull<Location>>}
}

// Annotated Labeled Transition System
// assumes the 0th index of locations is the beginning and any location
// which transition is None is connected to the end location
pub struct ALTS {
    end_invariant: Formula,
    locations: Vec<Location>
}

#[cfg(test)]
mod tests {
    use std::mem;
    use crate::alts::{ALTS, Location};
    
    #[test]
    #[ignore]
    fn cache_optimized_ARM64() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Location>(), 64);
            assert_eq!(mem::align_of::<Location>(), 64);
        }
    }
}

