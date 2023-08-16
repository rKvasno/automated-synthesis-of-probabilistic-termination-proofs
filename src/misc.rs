use crate::pts::{linear_polynomial::{self, term::Term}, variable_map};
use linear_polynomial::LinearPolynomial;
use linear_polynomial::constant::Constant;
use variable_map::{VariableMap, Variable};

pub fn setup_test_map() -> VariableMap {
    let mut map = VariableMap::default();
    map.find_or_add(Variable::new("a"));
    map.find_or_add(Variable::new("b"));
    map.find_or_add(Variable::new("c"));
    map
}

pub fn setup_test_polynomial(map: &VariableMap, constant: Constant, a: Constant, b: Constant, c: Constant) -> LinearPolynomial {
        let mut pol = LinearPolynomial::new();
        let mut var = Option::<&Variable>::cloned(map.get_variable(0));
        pol.try_add_term(&map, Term::new(var, constant)).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(1));
        pol.try_add_term(&map, Term::new(var, a)).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(2));
        pol.try_add_term(&map, Term::new(var, b)).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(3));
        pol.try_add_term(&map, Term::new(var, c)).unwrap();
        check_terms(&pol, &map, vec!(Some(constant), Some(a), Some(b), Some(c)));
        pol
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

