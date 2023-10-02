use crate::pts;
use pts::guard::{Guards, GuardsError};
use pts::system::System;

use std::iter::once;
use std::iter::{Chain, Map, Once};
use std::ops::Range;

// use of handles across different instances of Locations is undefined
pub type LocationHandle = Option<usize>;
pub type NonterminatingLocationIter = Map<Range<usize>, IndexToHandleFn>;
pub type LocationIter = Chain<Map<Range<usize>, IndexToHandleFn>, Once<LocationHandle>>;
type IndexToHandleFn = fn(usize) -> LocationHandle;

// must implement Display that doesnt violate DOT label restrictions and Clone
pub type LocationID = usize;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(64))] // 64 bytes
struct Location {
    invariant: System,
    outgoing: Guards,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct Locations {
    data: Vec<Location>,
    pub initial: LocationHandle,
    terminating_invariant: System,
}

impl<'a> Locations {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn new_location(&mut self) -> LocationHandle {
        self.data.push(Location::default());
        Some(self.data.len() - 1)
    }

    pub fn get_initial_location(&self) -> LocationHandle {
        self.initial
    }

    pub fn get_terminating_location(&self) -> LocationHandle {
        None
    }

    pub fn is_terminating_location(&self, handle: LocationHandle) -> bool {
        handle.is_none()
    }

    pub fn is_nonterminating_location(&self, handle: LocationHandle) -> bool {
        handle.is_none()
    }

    // Gets a unique id for a location
    // Ids get invalidated when locations are added
    pub fn get_id(&self, handle: LocationHandle) -> LocationID {
        match handle {
            Some(i) => i,
            None => self.len(),
        }
    }

    pub fn new_n_locations(&mut self, n: usize) -> NonterminatingLocationIter {
        self.data.resize_with(self.data.len() + n, Default::default);
        Range {
            start: self.data.len() - n,
            end: self.data.len(),
        }
        .map((|x| Some(x)) as IndexToHandleFn)
    }

    pub fn set_outgoing(
        &mut self,
        location: LocationHandle,
        guards: Guards,
    ) -> Result<(), GuardsError> {
        if location.is_none() {
            return Err(GuardsError::TerminatingLocation);
        }

        if guards.is_empty() {
            return Err(GuardsError::Empty);
        }

        self.data[location.unwrap()].outgoing = guards;
        Ok(())
    }

    pub fn set_invariant(&mut self, location: LocationHandle, invariant: System) {
        if location.is_none() {
            self.terminating_invariant = invariant;
        } else {
            self.data[location.unwrap()].invariant = invariant;
        }
    }

    pub fn iter(&self) -> LocationIter {
        Range {
            start: 0,
            end: self.data.len(),
        }
        .map((|x| Some(x)) as IndexToHandleFn)
        .chain(once::<Option<usize>>(None))
    }

    pub fn get_invariant(&self, handle: LocationHandle) -> Option<&System> {
        match handle {
            Some(n) => self.data.get(n).map(|x| &x.invariant),
            None => Some(&self.terminating_invariant),
        }
    }

    pub fn get_outgoing(&self, handle: LocationHandle) -> Option<&Guards> {
        match handle {
            Some(n) => self.data.get(n).map(|x| &x.outgoing),
            None => None,
        }
    }
}
