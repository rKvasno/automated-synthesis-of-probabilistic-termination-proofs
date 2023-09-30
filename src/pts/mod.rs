pub mod guard;
pub mod linear_polynomial;
pub mod location;
pub mod relation;
pub mod system;
pub mod transition;
pub mod variable_map;

use location::Locations;
use variable_map::VariableMap;

use dot;
use std::borrow::Cow;

use self::location::LocationHandle;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default)]
#[repr(align(32))] // 32 bytes
pub struct PTS {
    pub locations: Locations,
    pub variables: VariableMap,
}

trait DisplayLabel {
    fn label(&self, _: &VariableMap) -> String;
}

type Edge = (LocationHandle, LocationHandle);

impl<'a> dot::GraphWalk<'a, LocationHandle, Edge> for PTS {
    fn nodes(&self) -> dot::Nodes<'a, LocationHandle> {
        Cow::Owned(self.locations.iter().collect())
    }

    fn edges(&self) -> dot::Edges<'a, Edge> {
        let mut edges: Vec<Edge> = Vec::default();
        for loc in self.locations.iter() {
            match self.locations.get_outgoing(loc) {
                None => (),
                Some(guard) => {
                    for branch in guard.iter() {
                        edges.push((loc, branch.as_transition().target));
                    }
                }
            }
        }
        Cow::Owned(edges)
    }

    fn source(&'a self, edge: &Edge) -> LocationHandle {
        edge.0
    }

    fn target(&'a self, edge: &Edge) -> LocationHandle {
        edge.1
    }
}

impl<'a> dot::Labeller<'a, LocationHandle, Edge> for PTS {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("PTS").unwrap()
    }

    fn node_id(&'a self, node: &LocationHandle) -> dot::Id<'a> {
        match node {
            Some(n) => dot::Id::new(format!("Location_{n}")).unwrap(),
            None => dot::Id::new(format!("Location_terminating")).unwrap(),
        }
    }

    fn node_label(&'a self, n: &LocationHandle) -> dot::LabelText<'a> {
        // invariant
        dot::LabelText::LabelStr(Cow::Owned(
            self.locations
                .get_invariant(n.clone())
                .unwrap()
                .label(&self.variables),
        ))
    }

    fn edge_label(&'a self, e: &Edge) -> dot::LabelText<'a> {
        // guards
        let transition = self
            .locations
            .get_outgoing(e.0)
            .unwrap()
            .iter()
            .find(|x| x.as_transition().target == e.1);

        dot::LabelText::LabelStr(Cow::Owned(
            transition
                .map(|x| x.label(&self.variables))
                .unwrap_or(String::default()),
        ))
    }

    fn node_style(&'a self, node: &LocationHandle) -> dot::Style {
        // highlight starting and terminating locations
        if self.locations.initial == *node {
            dot::Style::Bold
        } else {
            dot::Style::Solid
        }
    }
    fn node_shape(&'a self, node: &LocationHandle) -> Option<dot::LabelText<'a>> {
        // mark terminating location
        if node.is_none() {
            Some(dot::LabelText::LabelStr(Cow::Owned(
                "doublecircle".to_string(),
            )))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        misc::read_test_string,
        pts::{
            guard::Guards,
            linear_polynomial::{constant::Constant, LinearPolynomial},
            relation::{Relation, RelationType},
            transition::{Assignment, Transition},
            variable_map::Variable,
        },
        system,
    };

    use super::{location::Locations, variable_map::VariableMap, PTS};

    #[test]
    fn dot_default() {
        let mut string = Vec::<u8>::default();
        let pts = PTS {
            locations: Locations::default(),
            variables: VariableMap::default(),
        };
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        assert_eq!(string, read_test_string("dot/default.gv"));
    }

    #[test]
    fn dot_trivial_program() {
        let mut locations = Locations::default();
        let handle = locations.new_location();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(
            handle,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(-1.0)]),
            )),
        );
        // 2
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();
        // 3
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        assert_eq!(string, read_test_string("dot/trivial_program.gv"));
    }

    #[test]
    fn dot_simple_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let handle = locations_iter.next().unwrap();
        locations.initial = handle;

        // line #
        // 1
        locations.set_invariant(
            handle,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0)]),
            )),
        );
        // 2
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0), Constant(0.0)]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();
        // 3
        let handle = next_location;
        locations.set_invariant(
            handle,
            system!(
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(1.0), Constant(0.0), Constant(-1.0)]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0), Constant(0.0)]),
                )
            ),
        );

        // 4
        let next_location = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("c"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(1.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();

        // 5
        let handle = next_location;
        locations.set_invariant(
            handle,
            system!(
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(0.0),
                        Constant(-1.0),
                    ]),
                )
            ),
        );
        // 6
        let next_location = locations.get_terminating_location();
        locations
            .set_outgoing(
                handle,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(2.0),
                            Constant(0.0),
                            Constant(1.0),
                        ]),
                    )],
                    target: next_location,
                })),
            )
            .unwrap();
        // 7
        let handle = next_location;
        locations.set_invariant(
            handle,
            system!(
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(0.0),
                        Constant(-1.0),
                        Constant(0.0),
                        Constant(0.0),
                    ]),
                ),
                Relation::mock(
                    RelationType::StrictInequality,
                    LinearPolynomial::mock(vec![
                        Constant(1.0),
                        Constant(0.0),
                        Constant(0.0),
                        Constant(-1.0),
                    ]),
                )
            ),
        );

        let variables = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/simple_program.gv"));
    }

    #[test]
    fn dot_simple_if_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        let br_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    (
                        // 2
                        system!(Relation::mock(
                            RelationType::NonstrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(-1.0),
                                Constant(1.0),
                            ]),
                        )),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 6
                        system!(
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            ),
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            )
                        ),
                        Transition {
                            assignments: vec![],
                            target: br_2,
                        },
                    ),
                    (
                        // 10
                        system!(
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(1.0),
                                    Constant(-1.0),
                                ]),
                            ),
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![
                                    Constant(0.0),
                                    Constant(0.0),
                                    Constant(-1.0),
                                    Constant(1.0),
                                ]),
                            )
                        ),
                        Transition {
                            assignments: vec![],
                            target: br_3,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 11
        locations.set_invariant(
            br_3,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )),
        );

        // 12
        locations
            .set_outgoing(
                br_3,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 14
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )),
        );

        // 15
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![
                            Constant(0.0),
                            Constant(1.0),
                            Constant(0.0),
                            Constant(0.0),
                        ]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )),
        );

        let variables = VariableMap::mock(vec![
            Variable::new("a"),
            Variable::new("b"),
            Variable::new("c"),
        ]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/simple_if_program.gv"));
    }

    #[test]
    fn dot_trivial_if_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    (
                        // 2
                        system!(Relation::mock(
                            RelationType::NonstrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(-1.0),
                                Constant(1.0),
                            ]),
                        )),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 5
                        system!(Relation::mock(
                            RelationType::StrictInequality,
                            LinearPolynomial::mock(vec![
                                Constant(0.0),
                                Constant(1.0),
                                Constant(-1.0),
                            ]),
                        )),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/trivial_if_program.gv"));
    }

    #[test]
    fn dot_simple_odds_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(4);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    (
                        // 2
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 6
                        Constant(1.0),
                        Transition {
                            assignments: vec![],
                            target: br_2,
                        },
                    ),
                    (
                        // 9
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            system!(Relation::mock(
                RelationType::NonstrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(-1.0)]),
            )),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("b"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 10
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 11
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0), Constant(0.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 12
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/simple_odds_program.gv"));
    }

    #[test]
    fn dot_trivial_odds_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    (
                        // 2
                        Constant(0.5),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    (
                        // 9
                        Constant(0.5),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/trivial_odds_program.gv"));
    }

    #[test]
    fn dot_simple_nondet_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(5);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        let br_2 = locations_iter.next().unwrap();
        let br_3 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //6
                    Transition {
                        assignments: vec![],
                        target: br_2,
                    },
                    //10
                    Transition {
                        assignments: vec![],
                        target: br_3,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 7
        locations.set_invariant(
            br_2,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 8
        locations
            .set_outgoing(
                br_2,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 11
        locations.set_invariant(
            br_3,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 12
        locations
            .set_outgoing(
                br_3,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();

        // 14
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 15
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 16
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/simple_nondet_program.gv"));
    }

    #[test]
    fn dot_trivial_nondet_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //5
                    Transition {
                        assignments: vec![],
                        target: junction,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: junction,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/trivial_nondet_program.gv"));
    }

    #[test]
    fn dot_logic_while_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Logic(vec![
                    // 2
                    (
                        system!(
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                            Relation::mock(
                                RelationType::StrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            )
                        ),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    //5
                    (
                        system!(
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            ),
                            Relation::mock(
                                RelationType::NonstrictInequality,
                                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
                            )
                        ),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/while_logic_program.gv"));
    }

    #[test]
    fn dot_prob_while_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Probabilistic(vec![
                    // 2
                    (
                        Constant(1.0),
                        Transition {
                            assignments: vec![],
                            target: br_1,
                        },
                    ),
                    //5
                    (
                        Constant(0.0),
                        Transition {
                            assignments: vec![],
                            target: junction,
                        },
                    ),
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/while_prob_program.gv"));
    }

    #[test]
    fn dot_nondet_while_program() {
        let mut locations = Locations::default();
        let mut locations_iter = locations.new_n_locations(3);

        let start = locations_iter.next().unwrap();
        locations.initial = start;

        // line #
        // 1
        locations.set_invariant(
            start,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0)]),
            )),
        );
        let junction = locations_iter.next().unwrap();
        let br_1 = locations_iter.next().unwrap();
        locations
            .set_outgoing(
                start,
                Guards::Nondeterministic(vec![
                    // 2
                    Transition {
                        assignments: vec![],
                        target: br_1,
                    },
                    //5
                    Transition {
                        assignments: vec![],
                        target: junction,
                    },
                ]),
            )
            .unwrap();

        // 3
        locations.set_invariant(
            br_1,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0)]),
            )),
        );

        // 4
        locations
            .set_outgoing(
                br_1,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: start,
                })),
            )
            .unwrap();
        // 6
        locations.set_invariant(
            junction,
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        // 7
        locations
            .set_outgoing(
                junction,
                Guards::Unguarded(Box::new(Transition {
                    assignments: vec![Assignment(
                        Variable::new("a"),
                        LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(1.0)]),
                    )],
                    target: locations.get_terminating_location(),
                })),
            )
            .unwrap();

        // 8
        locations.set_invariant(
            locations.get_terminating_location(),
            system!(Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![Constant(0.0), Constant(0.0), Constant(0.0)]),
            )),
        );

        let variables = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);

        let pts = PTS {
            locations,
            variables,
        };

        let mut string = Vec::<u8>::default();
        dot::render(&pts, &mut string).unwrap();
        let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
        print!("{}", string);
        assert_eq!(string, read_test_string("dot/while_nondet_program.gv"));
    }
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
