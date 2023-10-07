use crate::pts;
use pts::guard::{Guards, GuardsError};
use pts::system::System;

use std::iter::once;
use std::iter::{Chain, Map, Once};
use std::ops::Range;

use super::DisplayLabel;

// use of handles across different instances of Locations is undefined
pub type LocationHandle = Option<usize>;
pub type NonterminatingLocationIter = Map<Range<usize>, IndexToHandleFn>;
pub type LocationIter = Chain<Map<Range<usize>, IndexToHandleFn>, Once<LocationHandle>>;
type IndexToHandleFn = fn(usize) -> LocationHandle;

// must implement Display that doesnt violate DOT label restrictions and Clone
pub type LocationID = usize;

// test only, uses mock_relation
#[cfg(test)]
#[macro_export]
macro_rules! mock_invariant {
    [
        $(
            [
                $(

                   $sign:literal, $( $x:expr ),*

                );*
            ]
        ),* $(,)?
    ] => {


        {
            $crate::pts::location::Invariant{ assertions:
                vec![
                    $($crate::system![
                        $( $crate::mock_relation![
                            $sign, $($x, )*
                        ], ) *
                    ], )*
                ]
            }
        }
    };
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(64))] // 64 bytes
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

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
pub struct Invariant {
    pub assertions: Vec<System>,
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

    pub fn extend_invariant(&mut self, location: LocationHandle, assertion: System) {
        if location.is_none() {
            self.terminating_invariant.assertions.push(assertion);
        } else {
            self.data[location.unwrap()]
                .invariant
                .assertions
                .push(assertion);
        }
    }

    pub fn set_invariant(&mut self, location: LocationHandle, invariant: Invariant) {
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
impl DisplayLabel for Invariant {
    fn label(&self, variables: &pts::variable_map::VariableMap) -> String {
        let mut label: String = Default::default();
        let mut iter = self.assertions.iter();

        let assertion = iter.next();
        if assertion.is_some() {
            label.push_str(assertion.unwrap().label(variables).as_str());

            for assertion in iter {
                label.push_str(format!("\n\n{}", assertion.label(variables)).as_str());
            }
        }
        label
    }
}

#[cfg(test)]
mod tests {
    use crate::{mock_relation, pts::location::Invariant, system};

    #[test]
    fn mock_invariant() {
        assert_eq!(
            Invariant {
                assertions: vec![system!(
                    mock_relation!(">", 0.0, 12.4, -3.5, 2.4, -0.0),
                    mock_relation!("<=", -1.0, -2.4),
                    mock_relation!("=", 0.0, 0.0, 0.0, 0.0),
                    mock_relation!("!=", 111.111)
                )]
            },
            mock_invariant!(

                [
                    ">", 0.0, 12.4, -3.5, 2.4, -0.0;
                    "<=", -1.0, -2.4;
                    "=", 0.0, 0.0, 0.0, 0.0;
                    "!=", 111.111
                ]
            ),
        );
    }
}
