use crate::pts::{
    linear_polynomial::{self, term::Term},
    variable_map,
};
use linear_polynomial::constant::Constant;
use linear_polynomial::LinearPolynomial;
use variable_map::{Variable, VariableMap};

use pest::{Parser, RuleType};

#[cfg(test)]
pub mod test_data {
    pub mod dot {
        pub const DEFAULT: &'static str = include_str!("../test_data/dot/default.gv");
        pub const SIMPLE_IF_PROGRAM: &'static str =
            include_str!("../test_data/dot/simple_if_program.gv");
        pub const SIMPLE_NONDET_PROGRAM: &'static str =
            include_str!("../test_data/dot/simple_nondet_program.gv");
        pub const SIMPLE_ODDS_PROGRAM: &'static str =
            include_str!("../test_data/dot/simple_odds_program.gv");
        pub const SIMPLE_PROGRAM: &'static str = include_str!("../test_data/dot/simple_program.gv");
        pub const TRIVIAL_IF_PROGRAM: &'static str =
            include_str!("../test_data/dot/trivial_if_program.gv");
        pub const TRIVIAL_NONDET_PROGRAM: &'static str =
            include_str!("../test_data/dot/trivial_nondet_program.gv");
        pub const TRIVIAL_ODDS_PROGRAM: &'static str =
            include_str!("../test_data/dot/trivial_odds_program.gv");
        pub const TRIVIAL_PROGRAM: &'static str =
            include_str!("../test_data/dot/trivial_program.gv");
        pub const WHILE_LOGIC_PROGRAM: &'static str =
            include_str!("../test_data/dot/while_logic_program.gv");
        pub const WHILE_NONDET_PROGRAM: &'static str =
            include_str!("../test_data/dot/while_nondet_program.gv");
        pub const WHILE_PROB_PROGRAM: &'static str =
            include_str!("../test_data/dot/while_prob_program.gv");
    }
    pub mod code {
        pub mod default {

            pub const COMPLEX_PROGRAM: &'static str =
                include_str!("../test_data/code/default/complex_program");
            pub const INVARIANTS_SNIPPET: &'static str =
                include_str!("../test_data/code/default/invariants_snippet");
            pub const SIMPLE_IF_PROGRAM: &'static str =
                include_str!("../test_data/code/default/simple_if_program");
            pub const SIMPLE_IF_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_if_snippet");
            pub const SIMPLE_LOGIC_WHILE_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_logic_while_snippet");
            pub const SIMPLE_NONDET_PROGRAM: &'static str =
                include_str!("../test_data/code/default/simple_nondet_program");
            pub const SIMPLE_NONDET_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_nondet_snippet");
            pub const SIMPLE_NONDET_WHILE_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_nondet_while_snippet");
            pub const SIMPLE_ODDS_PROGRAM: &'static str =
                include_str!("../test_data/code/default/simple_odds_program");
            pub const SIMPLE_ODDS_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_odds_snippet");
            pub const SIMPLE_PROB_WHILE_SNIPPET: &'static str =
                include_str!("../test_data/code/default/simple_prob_while_snippet");
            pub const SIMPLE_PROGRAM: &'static str =
                include_str!("../test_data/code/default/simple_program");
            pub const TRIVIAL_IF_PROGRAM: &'static str =
                include_str!("../test_data/code/default/trivial_if_program");
            pub const TRIVIAL_NONDET_PROGRAM: &'static str =
                include_str!("../test_data/code/default/trivial_nondet_program");
            pub const TRIVIAL_ODDS_PROGRAM: &'static str =
                include_str!("../test_data/code/default/trivial_odds_program");
            pub const TRIVIAL_PROGRAM: &'static str =
                include_str!("../test_data/code/default/trivial_program");
            pub const WHILE_LOGIC_PROGRAM: &'static str =
                include_str!("../test_data/code/default/while_logic_program");
            pub const WHILE_NONDET_PROGRAM: &'static str =
                include_str!("../test_data/code/default/while_nondet_program");
            pub const WHILE_PROB_PROGRAM: &'static str =
                include_str!("../test_data/code/default/while_prob_program");
        }
    }
}

pub fn setup_test_map() -> VariableMap {
    let mut map = VariableMap::default();
    map.get_or_push(&Some(Variable::new("a")));
    map.get_or_push(&Some(Variable::new("b")));
    map.get_or_push(&Some(Variable::new("c")));
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
