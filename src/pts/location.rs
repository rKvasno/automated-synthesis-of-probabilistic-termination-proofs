use super::{
    guard::{Guards, GuardsError},
    invariant::Invariant,
};

// use of handles across different instances of Locations is undefined
pub type LocationHandle = Option<usize>;
pub type NonterminatingLocationIter = std::iter::Map<std::ops::Range<usize>, IndexToHandleFn>;
pub type LocationIter = std::iter::Chain<
    std::iter::Map<std::ops::Range<usize>, IndexToHandleFn>,
    std::iter::Once<LocationHandle>,
>;
type IndexToHandleFn = fn(usize) -> LocationHandle;

// must implement Display that doesnt violate DOT label restrictions and Clone
pub type LocationID = usize;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
struct Location {
    invariant: Invariant,
    outgoing: Guards,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct Locations {
    data: Vec<Location>,
    pub initial: LocationHandle,
    terminating_invariant: Invariant,
}

impl Locations {
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
    pub fn get_id(&self, handle: LocationHandle) -> Option<LocationID> {
        match handle {
            None => Some(self.len()),
            Some(i) => {
                if self.len() <= i {
                    None
                } else {
                    Some(i)
                }
            }
        }
    }

    pub fn new_n_locations(&mut self, n: usize) -> NonterminatingLocationIter {
        self.data.resize_with(self.data.len() + n, Default::default);
        std::ops::Range {
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
            return Err(GuardsError::StateTerminatingLocation);
        }

        if guards.is_empty() {
            return Err(GuardsError::Empty);
        }

        self.data[location.unwrap()].outgoing = guards;
        Ok(())
    }

    pub fn set_invariant(&mut self, location: LocationHandle, invariant: Invariant) {
        if location.is_none() {
            self.terminating_invariant = invariant;
        } else {
            self.data[location.unwrap()].invariant = invariant;
        }
    }

    pub fn iter(&self) -> LocationIter {
        std::ops::Range {
            start: 0,
            end: self.data.len(),
        }
        .map((|x| Some(x)) as IndexToHandleFn)
        .chain(std::iter::once::<Option<usize>>(None))
    }

    pub fn get_invariant(&self, handle: LocationHandle) -> Option<&Invariant> {
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
