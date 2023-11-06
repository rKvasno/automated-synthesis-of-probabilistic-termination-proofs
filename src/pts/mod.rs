use std::{borrow::Cow, collections::hash_map::RandomState, hash::BuildHasher};

use self::{
    location::{LocationHandle, Locations},
    variable::{program_variable::ProgramVariable, set::VariableSet},
};

pub mod guard;
pub mod invariant;
pub mod linear_polynomial;
pub mod location;
pub mod relation;
pub mod system;
pub mod transition;
pub mod variable;

#[derive(Debug, Default)]
pub struct PTS<S: BuildHasher + Default = RandomState> {
    // the implementation assumes that every variable is in variables
    pub locations: Locations,
    pub variables: VariableSet<ProgramVariable, S>,
}

impl PTS {
    // Optimizes the structure
    pub fn finalize(&mut self) {
        self.variables.shrink_to_fit();
    }
}

#[cfg(test)]
impl PartialEq for PTS {
    fn eq(&self, other: &Self) -> bool {
        self.locations == other.locations && self.variables == other.variables
    }
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
                .get_invariant(n.to_owned())
                .unwrap()
                .to_string(),
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
                .map(|x| x.to_string())
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
            guards, invariant,
            misc::tests::pts::{
                DEFAULT, SIMPLE_CHOOSE_PROGRAM, SIMPLE_IF_PROGRAM, SIMPLE_ODDS_PROGRAM,
                SIMPLE_PROGRAM, TRIVIAL_CHOOSE_PROGRAM, TRIVIAL_IF_PROGRAM, TRIVIAL_ODDS_PROGRAM,
                TRIVIAL_PROGRAM, WHILE_LOGIC_PROGRAM, WHILE_NONDETERMINISTIC_PROGRAM,
                WHILE_PROB_PROGRAM,
            },
            pts::{location::Locations, variable::program_variable::ProgramVariables, PTS},
            state_system, transition, variables,
        };

        #[test]
        fn dot_default() {
            let mut string = Vec::<u8>::default();
            let pts = PTS {
                locations: Locations::default(),
                variables: variables!(),
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

            let mut variables: ProgramVariables = variables!("a");

            // line #
            // 1
            locations
                .set_invariant(handle, invariant!(&mut variables, ["<", -1.0]))
                .unwrap();
            // 2
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 1.0, 0.0, "a")),
                )
                .unwrap();
            // 3
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 1.0, 0.0, "a"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let handle = locations_iter.next().unwrap();
            locations.initial = handle;

            // line #
            // 1
            locations
                .set_invariant(handle, invariant!(&mut variables, ["<", 0.0, -1.0, "a"]))
                .unwrap();

            // 2
            let next_location = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    handle,
                    guards!(
                        transition!(next_location, &mut variables; "b", 1.0, 0.0, "a", 0.0, "b")
                    ),
                )
                .unwrap();

            // 3
            let handle = next_location;
            locations
                .set_invariant(
                    handle,
                    invariant!(&mut variables, [
                        "<=", 1.0, 0.0, "a", -1.0, "b";
                        "<", 0.0, -1.0, "a", 0.0, "b"
                    ]),
                )
                .unwrap();

            // 4
            let next_location = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    handle,
                    guards!(transition!(next_location, &mut variables; "c", 0.0, 1.0, "a", 1.0, "b", 0.0, "c")),
                )
                .unwrap();

            // 5
            let handle = next_location;
            locations
                .set_invariant(
                    handle,
                    invariant!(&mut variables, [
                        "<=", 1.0, 0.0, "a", -1.0, "b", 0.0, "c";
                        "<", 0.0, -1.0, "a", 0.0, "b", 0.0, "c";
                        "<", 1.0, 0.0, "a", 0.0, "b", -1.0, "c"
                    ]),
                )
                .unwrap();

            // 6
            let next_location = locations.get_terminating_location();
            locations
                .set_outgoing(
                    handle,
                    guards!(
                        transition!(next_location, &mut variables; "b", 0.0, 2.0, "a", 0.0, "b", 1.0, "c")
                    ),
                )
                .unwrap();

            // 7
            let handle = next_location;
            locations
                .set_invariant(
                    handle,
                    invariant!(&mut variables, [
                        "<", 1.0, 0.0, "a", -1.0, "b", 0.0, "c";
                        "<", 0.0, -1.0, "a", 0.0, "b", 0.0, "c";
                        "<", 1.0, 0.0, "a", 0.0, "b", -1.0, "c"
                    ]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a", "b", "c");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            let branch_2 = locations_iter.next().unwrap();
            let branch_3 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 2
                        state_system!(&mut variables;"<=", 0.0, -1.0, "a", 1.0, "b"),
                        transition!(branch_1),
                        // 6
                        state_system!(&mut variables;
                            ">", 0.0, -1.0, "a", 1.0, "b";
                            "<=", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"
                        ),
                        transition!(branch_2),
                        // 10
                        state_system!(&mut variables;
                            ">", 0.0, -1.0, "a", 1.0, "b";
                            ">", 0.0, 0.0, "a", 1.0, "b", -1.0, "c"
                        ),
                        transition!(branch_3)
                    ),
                )
                .unwrap();

            // 3
            locations
                .set_invariant(
                    branch_1,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")),
                )
                .unwrap();

            // 7
            locations
                .set_invariant(
                    branch_2,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
                )
                .unwrap();

            // 8
            locations
                .set_outgoing(
                    branch_2,
                    guards!(
                        transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                    ),
                )
                .unwrap();

            // 11
            locations
                .set_invariant(
                    branch_3,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
                )
                .unwrap();

            // 12
            locations
                .set_outgoing(
                    branch_3,
                    guards!(
                        transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                    ),
                )
                .unwrap();

            // 14
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
                )
                .unwrap();

            // 15
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a", 0.0, "b", 0.0, "c")
                    ),
                )
                .unwrap();

            // 16
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b", 0.0, "c"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();
            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 2
                        state_system!(&mut variables; "<=", 0.0, -1.0, "a", 1.0, "b"),
                        transition!(branch_1),

                        // 5
                        state_system!(&mut variables; "<", 0.0, 1.0, "a", -1.0, "b"),
                        transition!(junction),

                    ),
                )
                .unwrap();

            // 3
            locations
                .set_invariant(
                    branch_1,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")),
                )
                .unwrap();

            // 6
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")
                    ),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 7
            locations
                .set_invariant(
                    branch_2,
                    invariant!(&mut variables, ["<=", 0.0, 1.0, "a", -1.0, "b"]),
                )
                .unwrap();

            // 8
            locations
                .set_outgoing(
                    branch_2,
                    guards!(transition!(junction, &mut variables; "b", 0.0, 1.0, "a", 0.0, "b")),
                )
                .unwrap();

            // 10
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 11
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a", 0.0, "b")
                    ),
                )
                .unwrap();

            // 12
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 6
            locations
                .set_invariant(junction, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 7
            locations
                .set_invariant(branch_2, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 8
            locations
                .set_outgoing(
                    branch_2,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 11
            locations
                .set_invariant(branch_3, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 12
            locations
                .set_outgoing(
                    branch_3,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 14
            locations
                .set_invariant(junction, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 15
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 16
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
                )
                .unwrap();

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, SIMPLE_CHOOSE_PROGRAM);
        }

        #[test]
        fn dot_trivial_nondet_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(junction, &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();
            // 6
            locations
                .set_invariant(junction, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 1.0, "a")),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a"]),
                )
                .unwrap();

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, TRIVIAL_CHOOSE_PROGRAM);
        }

        #[test]
        fn dot_logic_while_program() {
            let mut locations = Locations::default();
            let mut locations_iter = locations.new_n_locations(3);
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

            let junction = locations_iter.next().unwrap();
            let branch_1 = locations_iter.next().unwrap();
            locations
                .set_outgoing(
                    start,
                    guards!(L:
                        // 2
                        state_system!(&mut variables;
                                "<", 0.0, 0.0, "a"
                                ;
                                "<", 0.0, 0.0, "a"
                        ),
                        transition!(branch_1),
                        // 5
                        state_system!(&mut variables;
                                "<=", 0.0, 0.0, "a"
                            ;
                                "<=", 0.0, 0.0, "a"
                        ),
                        transition!(junction)
                    ),
                )
                .unwrap();

            // 3
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
                )
                .unwrap();

            // 6
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")
                    ),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
                )
                .unwrap();

            // 6
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")
                    ),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

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
            let mut variables: ProgramVariables = variables!("a");

            let start = locations_iter.next().unwrap();
            locations.initial = start;

            // line #
            // 1
            locations
                .set_invariant(start, invariant!(&mut variables, ["<", 0.0]))
                .unwrap();

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
            locations
                .set_invariant(branch_1, invariant!(&mut variables, ["<", 0.0, 0.0, "a"]))
                .unwrap();

            // 4
            locations
                .set_outgoing(
                    branch_1,
                    guards!(transition!(start, &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")),
                )
                .unwrap();
            // 6
            locations
                .set_invariant(
                    junction,
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            // 7
            locations
                .set_outgoing(
                    junction,
                    guards!(
                        transition!(locations.get_terminating_location(), &mut variables; "a", 0.0, 0.0, "a", 1.0, "b")
                    ),
                )
                .unwrap();

            // 8
            locations
                .set_invariant(
                    locations.get_terminating_location(),
                    invariant!(&mut variables, ["<", 0.0, 0.0, "a", 0.0, "b"]),
                )
                .unwrap();

            let pts = PTS {
                locations,
                variables,
            };

            let mut string = Vec::<u8>::default();
            dot::render(&pts, &mut string).unwrap();
            let string = std::str::from_utf8(string.as_slice()).unwrap().to_string();
            print!("{}", string);
            assert_eq!(string, WHILE_NONDETERMINISTIC_PROGRAM);
        }
    }
}
