use crate::pts;
use pts::guard::{Guards, GuardsError};
use pts::inequality::InequalitySystem;

use std::cell::RefCell;
use std::iter::Map;
use std::ops::Range;

// use of handles across different instances of Locations is undefined
pub type LocationHandle = Option<usize>;
pub type LocationIter = Map<Range<usize>, fn(usize) -> LocationHandle>;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(64))] // 64 bytes
struct Location {
    invariant: InequalitySystem,
    outgoing: Guards,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct Locations {
    data: Vec<Location>,
    pub initial: LocationHandle,
    terminating_invariant: InequalitySystem
}


impl<'a> Locations {
    pub fn new_location(&mut self) -> LocationHandle {
        self.data.push(Location::default());
        Some(self.data.len()-1)
    }

    pub fn get_initial_location(&self) -> LocationHandle {
        self.initial
    }

    pub fn get_terminating_location(&self) -> LocationHandle {
        None
    }

    pub fn new_n_locations(&mut self, n: usize) -> LocationIter {
        self.data.resize_with(self.data.len() + n, Default::default);
        Range{ start: self.data.len(), end: self.data.len() + n }.map(|x| Some(x))
    }

    pub fn set_outgoing(&mut self, location: LocationHandle, guards: Guards) -> Result<(), GuardsError> {
        if location.is_none() {
            return Err(GuardsError::TerminatingLocation);
        }

        if guards.is_empty() {
            return Err(GuardsError::Empty);
        }

        self.data[location.unwrap()].outgoing = guards;
        Ok(())
    }
    
    pub fn set_invariant(&mut self, location: LocationHandle, invariant: InequalitySystem) {
        if location.is_none() {
            self.terminating_invariant = invariant;
        }
        else {
            self.data[location.unwrap()].invariant = invariant;
        }
    }
}

