use std::ops::Not;
use std::slice::Iter;

use super::relation::Relation;
use super::variable_map::VariableMap;
use super::DisplayLabel;

pub type RelationIter<'a> = Iter<'a, Relation>;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Default, Clone)]
pub struct System {
    relations: Vec<Relation>,
}

impl System {
    pub fn push(&mut self, inequality: Relation) {
        self.relations.push(inequality);
    }

    pub fn append(&mut self, system: &mut System) {
        self.relations.append(&mut system.relations);
    }

    pub fn len(&self) -> usize {
        self.relations.len()
    }

    pub fn get(&self, index: usize) -> Option<&Relation> {
        self.relations.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Relation> {
        self.relations.get_mut(index)
    }

    pub fn iter<'a>(&'a self) -> RelationIter<'a> {
        self.relations.iter()
    }
}

impl DisplayLabel for System {
    fn label(&self, variable_map: &VariableMap) -> String {
        //ineq (\n ineq)*
        let mut label = String::default();
        let mut iter = self.relations.iter().map(|x| x.label(variable_map));
        match iter.next() {
            Some(line) => label.push_str(line.as_str()),
            _ => (),
        }
        for line in iter {
            label.push_str("\n");
            label.push_str(line.as_str());
        }
        label
    }
}

impl Not for System {
    type Output = Self;
    fn not(self) -> Self::Output {
        System {
            relations: self.relations.into_iter().map(|x| !x).collect(),
        }
    }
}

#[cfg(test)]
impl System {
    pub fn mock(relations: Vec<Relation>) -> Self {
        System { relations }
    }
}

#[cfg(test)]
mod tests {
    use crate::pts::{
        linear_polynomial::{constant::Constant, LinearPolynomial},
        relation::{Relation, RelationType},
        variable_map::{Variable, VariableMap},
        DisplayLabel,
    };

    use super::System;

    #[test]
    fn label() {
        let system = System {
            relations: vec![
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0)]),
                ),
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0)]),
                ),
            ],
        };
        let map = VariableMap::mock(vec![]);
        assert_eq!(system.label(&map), "0 <= 0\n0 <= 0");

        let system = System {
            relations: vec![
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0), Constant(1.0)]),
                ),
                Relation::mock(
                    RelationType::NonstrictInequality,
                    LinearPolynomial::mock(vec![Constant(0.0), Constant(-1.0)]),
                ),
            ],
        };
        let map = VariableMap::mock(vec![Variable::new("a"), Variable::new("b")]);
        assert_eq!(system.label(&map), "a <= 0\na >= 0");

        let system = System {
            relations: vec![Relation::mock(
                RelationType::StrictInequality,
                LinearPolynomial::mock(vec![
                    Constant(0.0),
                    Constant(-1.0),
                    Constant(0.0),
                    Constant(0.0),
                ]),
            )],
        };
        let map = VariableMap::mock(vec![
            Variable::new("test1"),
            Variable::new("test2"),
            Variable::new("test3"),
        ]);
        assert_eq!(system.label(&map), "test1 > 0");
    }
}
