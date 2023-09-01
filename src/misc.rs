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
        let mut pol = LinearPolynomial::default();
        let mut var = Option::<&Variable>::cloned(map.get_variable(0));
        pol.try_add_term(&map, Term{ variable: var, coefficient: constant }).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(1));
        pol.try_add_term(&map, Term{ variable: var, coefficient: a }).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(2));
        pol.try_add_term(&map, Term{ variable: var, coefficient: b }).unwrap();
        var = Option::<&Variable>::cloned(map.get_variable(3));
        pol.try_add_term(&map, Term{ variable: var, coefficient: c }).unwrap();
        pol
}

