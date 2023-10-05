use crate::pts::{
    linear_polynomial::{self, term::Term},
    variable_map,
};
use linear_polynomial::constant::Constant;
use linear_polynomial::LinearPolynomial;
use variable_map::{Variable, VariableMap};

#[cfg(test)]
pub mod tests {
    pub mod pts {
        pub const DEFAULT: &'static str = include_str!("../tests/pts/default.gv");
        pub const SIMPLE_IF_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_if_program.gv");
        pub const SIMPLE_NONDET_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_nondet_program.gv");
        pub const SIMPLE_ODDS_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_odds_program.gv");
        pub const SIMPLE_PROGRAM: &'static str = include_str!("../tests/pts/simple_program.gv");
        pub const TRIVIAL_IF_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_if_program.gv");
        pub const TRIVIAL_NONDET_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_nondet_program.gv");
        pub const TRIVIAL_ODDS_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_odds_program.gv");
        pub const TRIVIAL_PROGRAM: &'static str = include_str!("../tests/pts/trivial_program.gv");
        pub const WHILE_LOGIC_PROGRAM: &'static str =
            include_str!("../tests/pts/while_logic_program.gv");
        pub const WHILE_NONDET_PROGRAM: &'static str =
            include_str!("../tests/pts/while_nondet_program.gv");
        pub const WHILE_PROB_PROGRAM: &'static str =
            include_str!("../tests/pts/while_prob_program.gv");
    }
    pub mod parsers {
        pub mod default {
            pub const SIMPLE_IF_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_if_program");
            pub const SIMPLE_NONDET_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_nondet_program");
            pub const SIMPLE_ODDS_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_odds_program");
            pub const SIMPLE_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_program");
            pub const TRIVIAL_IF_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_if_program");
            pub const TRIVIAL_NONDET_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_nondet_program");
            pub const TRIVIAL_ODDS_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_odds_program");
            pub const TRIVIAL_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_program");
            pub const WHILE_LOGIC_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_logic_program");
            pub const WHILE_NONDET_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_nondet_program");
            pub const WHILE_PROB_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_prob_program");
        }
    }
}

#[macro_export]
macro_rules! consume {
    ($x:expr) => {{
        $x;
    }};
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
