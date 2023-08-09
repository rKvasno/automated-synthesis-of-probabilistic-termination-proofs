use crate::pts::{linear_polynomial, variable_map};
use linear_polynomial::LinearPolynomial;
use linear_polynomial::constant::Constant;
use variable_map::{VariableMap, Variable};

pub fn setup_map() -> VariableMap {
    let mut map = VariableMap::new();
    map.find_or_add(Variable::new("a"));
    map.find_or_add(Variable::new("b"));
    map.find_or_add(Variable::new("c"));
    map
}

pub fn check_terms(pol: &LinearPolynomial, map: &VariableMap, vec: Vec<Option<Constant>>) {
    assert_eq!(map.len() +1, vec.len());
    assert!(map.len() <= pol.len());
    for (index, element) in vec.iter().enumerate() {
        if element.is_some() {
            assert_eq!(pol.get_coefficient(index).unwrap(), element.unwrap());
        }
    }
}

