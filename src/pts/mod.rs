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
    mod dot {
        use crate::{
            guards,
            misc::tests::pts::{
                DEFAULT, SIMPLE_IF_PROGRAM, SIMPLE_NONDET_PROGRAM, SIMPLE_ODDS_PROGRAM,
                SIMPLE_PROGRAM, TRIVIAL_IF_PROGRAM, TRIVIAL_NONDET_PROGRAM, TRIVIAL_ODDS_PROGRAM,
                TRIVIAL_PROGRAM, WHILE_LOGIC_PROGRAM, WHILE_NONDET_PROGRAM, WHILE_PROB_PROGRAM,
            },
            mock_invariant, mock_relation, mock_varmap,
            pts::{location::Locations, variable_map::VariableMap, PTS},
            system, transition,
        };

        #[test]
        fn dot_default() {
            let mut string = Vec::<u8>::default();
            let pts = PTS {
                locations: Locations::default(),
                variables: VariableMap::default(),
            };
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            assert_eq!(string, DEFAULT);
        }

        #[test]
        fn dot_trivial_program() {
            let mut locations = Locations::default();
            let handle = locations.new_location();
            locations.initial = handle;

            // line #
            // 1
            locations.set_invariant(handle, mock_invariant!(["<", -1.0]));
            // 2
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(locations.get_terminating_location(); "a", 1.0, 0.0)),
                )
                .unwrap();
            // 3
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 1.0, 0.0]),
            );

            let variables = mock_varmap!("a");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            assert_eq!(string, TRIVIAL_PROGRAM);
        }

        #[test]
        fn dot_simple_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let handle = locations_iter.next().unwrap();
            locations.initial = handle;

            // line #
            // 1
            locations.set_invariant(handle, mock_invariant!(["<", 0.0, -1.0]));

            // 2
            let next_location = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(next_location; "b", 1.0, 0.0, 0.0)),
                )
                .unwrap();

            // 3
            let handle = next_location;
            locations.set_invariant(
                handle,
                mock_invariant!(
                [
                    "<=", 1.0, 0.0, -1.0;
                    "<", 0.0, -1.0, 0.0
                ]),
            );

            // 4
            let next_location = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(next_location; "c", 0.0, 1.0, 1.0, 0.0)),
                )
                .unwrap();

            // 5
            let handle = next_location;
            locations.set_invariant(
                handle,
                mock_invariant!(
                [
                    "<=", 1.0, 0.0, -1.0, 0.0;
                    "<", 0.0, -1.0, 0.0, 0.0;
                    "<", 1.0, 0.0, 0.0, -1.0
                ]),
            );

            // 6
            let next_location = locations.get_terminating_location();
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(next_location; "b", 0.0, 2.0, 0.0, 1.0)),
                )
                .unwrap();

            // 7
            let handle = next_location;
            locations.set_invariant(
                handle,
                mock_invariant!(
                [
                    "<", 1.0, 0.0, -1.0, 0.0;
                    "<", 0.0, -1.0, 0.0, 0.0;
                    "<", 1.0, 0.0, 0.0, -1.0
                ]),
            );

            let variables = mock_varmap!("a", "b", "c");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, SIMPLE_PROGRAM);
        }

        #[test]
        fn dot_simple_if_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(5);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            let branch_2 = locations_iter.next().unwrap();
            let branch_3 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 2
                        system!(mock_relation!("<=", 0.0, -1.0, 1.0)),
                        transition!(branch_1),
                        // 6
                        system!(
                            mock_relation!(">", 0.0, 1.0, -1.0),
                            mock_relation!("<=", 0.0, 0.0, 1.0, -1.0)
                        ),
                        transition!(branch_2),
                        // 10
                        system!(
                            mock_relation!(">", 0.0, 1.0, -1.0),
                            mock_relation!(">", 0.0, 0.0, -1.0, 1.0)
                        ),
                        transition!(branch_3)
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0, 0.0)))
                .unwrap();

            // 7
            locations.set_invariant(branch_2, mock_invariant!(["<", 0.0, 0.0, 0.0, 0.0]));

            // 8
            locations
                .set_outgoing(
                    branch_2,
                    guards!(transition!(junction; "a", 0.0, 1.0, 0.0, 0.0)),
                )
                .unwrap();

            // 11
            locations.set_invariant(branch_3, mock_invariant!(["<", 0.0, 0.0, 0.0, 0.0]));

            // 12
            locations
                .set_outgoing(
                    branch_3,
                    guards!(transition!(junction; "a", 0.0, 1.0, 0.0, 0.0)),
                )
                .unwrap();

            // 14
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0, 0.0]));

            // 15
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(); "a", 0.0, 1.0, 0.0, 0.0)
                    ),
                )
                .unwrap();

            // 16
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b", "c");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, SIMPLE_IF_PROGRAM);
        }

        #[test]
        fn dot_trivial_if_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));
            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 22
                        system!(mock_relation!("<=", 0.0, -1.0, 1.0)),
                        transition!(branch_1),

                        // 5
                        system!(mock_relation!("<", 0.0, 1.0, -1.0)),
                        transition!(junction),

                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0, 0.0)))
                .unwrap();

            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 1.0, 0.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, TRIVIAL_IF_PROGRAM);
        }

        #[test]
        fn dot_simple_odds_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(4);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            let branch_2 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(P:
                        // 2
                        0.0,
                        transition!(branch_1),
                        // 6
                        1.0,
                        transition!(branch_2),
                        // 9
                        0.0,
                        transition!(junction),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();

            // 7
            locations.set_invariant(branch_2, mock_invariant!(["<=", 0.0, 1.0, -1.0]));

            // 8
            locations
                .set_outgoing(branch_2, guards!(transition!(junction; "b", 0.0, 1.0, 0.0)))
                .unwrap();

            // 10
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 11
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 1.0, 0.0)),
                )
                .unwrap();

            // 12
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, SIMPLE_ODDS_PROGRAM);
        }

        #[test]
        fn dot_trivial_odds_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(P:
                        0.5,
                        transition!(branch_1),
                        0.5,
                        transition!(junction),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();

            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 1.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0]),
            );

            let variables = mock_varmap!("a");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, TRIVIAL_ODDS_PROGRAM);
        }

        #[test]
        fn dot_simple_nondet_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(5);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            let branch_2 = locations_iter.next().unwrap();
            let branch_3 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(
                        // 2
                        transition!(branch_1),
                        // 6
                        transition!(branch_2),
                        // 10
                        transition!(branch_3),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();

            // 7
            locations.set_invariant(branch_2, mock_invariant!(["<", 0.0, 0.0]));

            // 8
            locations
                .set_outgoing(branch_2, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();

            // 11
            locations.set_invariant(branch_3, mock_invariant!(["<", 0.0, 0.0]));

            // 12
            locations
                .set_outgoing(branch_3, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();

            // 14
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0]));

            // 15
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 1.0)),
                )
                .unwrap();

            // 16
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0]),
            );

            let variables = mock_varmap!("a");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, SIMPLE_NONDET_PROGRAM);
        }

        #[test]
        fn dot_trivial_nondet_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(
                        // 2
                        transition!(branch_1),
                        // 5
                        transition!(junction),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(junction; "a", 0.0, 1.0)))
                .unwrap();
            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 1.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0]),
            );

            let variables = mock_varmap!("a");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, TRIVIAL_NONDET_PROGRAM);
        }

        #[test]
        fn dot_logic_while_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 2
                        system!(
                            mock_relation!(
                                "<", 0.0, 0.0
                            ),
                            mock_relation!(
                                "<", 0.0, 0.0
                            )
                        ),
                        transition!(branch_1),
                        // 5
                        system!(
                            mock_relation!(
                                "<=", 0.0, 0.0
                            ),
                            mock_relation!(
                                "<=", 0.0, 0.0
                            )
                        ),
                        transition!(junction)
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(start; "a", 0.0, 0.0, 1.0)))
                .unwrap();

            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 0.0, 1.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, WHILE_LOGIC_PROGRAM);
        }

        #[test]
        fn dot_prob_while_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(P:
                            // 2
                            1.0,
                            transition!(branch_1),
                            // 5
                            0.0,
                            transition!(junction),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(start; "a", 0.0, 0.0, 1.0)))
                .unwrap();

            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 0.0, 1.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, WHILE_PROB_PROGRAM);
        }

        #[test]
        fn dot_nondet_while_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations.set_invariant(start, mock_invariant!(["<", 0.0]));

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(
                        // 2
                        transition!(branch_1),
                        // 5
                        transition!(junction),
                    ),
                )
                .unwrap();

            // 3
            locations.set_invariant(branch_1, mock_invariant!(["<", 0.0, 0.0]));

            // 4
            locations
                .set_outgoing(branch_1, guards!(transition!(start; "a", 0.0, 0.0, 1.0)))
                .unwrap();
            // 6
            locations.set_invariant(junction, mock_invariant!(["<", 0.0, 0.0, 0.0]));

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(); "a", 0.0, 0.0, 1.0)),
                )
                .unwrap();

            // 8
            locations.set_invariant(
                locations.get_terminating_location(),
                mock_invariant!(["<", 0.0, 0.0, 0.0]),
            );

            let variables = mock_varmap!("a", "b");

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, WHILE_NONDET_PROGRAM);
        }
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
