use crate::pts::{
    linear_polynomial::{self, term::Term},
    variable_map,
};
use linear_polynomial::constant::Constant;
use linear_polynomial::LinearPolynomial;
use variable_map::{Variable, VariableMap};

use pest::{Parser, RuleType};
use std::env::var;
use std::fs::read_to_string;

pub fn setup_test_map() -> VariableMap {
    let mut map = VariableMap::default();
    map.find_or_add(Variable::new("a"));
    map.find_or_add(Variable::new("b"));
    map.find_or_add(Variable::new("c"));
    map
}

pub fn setup_test_polynomial(
    map: &VariableMap,
    constant: Constant,
    a: Constant,
    b: Constant,
    c: Constant,
) -> LinearPolynomial {
    let mut pol = LinearPolynomial::default();
    let mut var = Option::<&Variable>::cloned(map.get_variable(0).unwrap());
    pol.try_add_term(
        &map,
        Term {
            variable: var,
            coefficient: constant,
        },
    )
    .unwrap();
    var = Option::<&Variable>::cloned(map.get_variable(1).unwrap());
    pol.try_add_term(
        &map,
        Term {
            variable: var,
            coefficient: a,
        },
    )
    .unwrap();
    var = Option::<&Variable>::cloned(map.get_variable(2).unwrap());
    pol.try_add_term(
        &map,
        Term {
            variable: var,
            coefficient: b,
        },
    )
    .unwrap();
    var = Option::<&Variable>::cloned(map.get_variable(3).unwrap());
    pol.try_add_term(
        &map,
        Term {
            variable: var,
            coefficient: c,
        },
    )
    .unwrap();
    pol
}

pub fn _print_rule_parsing<R: RuleType, P: Parser<R>>(rule: R, input: &str) {
    let parsed = P::parse(rule, input).unwrap();
    for pair in parsed.clone().flatten() {
        print!(
            "{:?}:\n{}\n######################## ",
            pair.as_rule(),
            pair.as_str()
        );
    }
    panic!("{}", parsed);
}

pub fn read_test_input(input_file: &str) -> String {
    let dir = var("CARGO_MANIFEST_DIR").unwrap() + "/test_programs/" + input_file;
    read_to_string(dir.clone()).unwrap_or_else(|_| panic!("Can't find {}", dir))
}
