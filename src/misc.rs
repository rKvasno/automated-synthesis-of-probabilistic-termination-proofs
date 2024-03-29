#[macro_export]
macro_rules! consume {
    ($x:expr) => {{
        $x;
    }};
}

#[cfg(test)]
pub mod tests {
    pub mod pts {
        pub const DEFAULT: &'static str = include_str!("../tests/pts/default.gv");
        pub const SIMPLE_IF_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_if_program.gv");
        pub const SIMPLE_CHOOSE_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_choose_program.gv");
        pub const SIMPLE_ODDS_PROGRAM: &'static str =
            include_str!("../tests/pts/simple_odds_program.gv");
        pub const SIMPLE_PROGRAM: &'static str = include_str!("../tests/pts/simple_program.gv");
        pub const TRIVIAL_IF_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_if_program.gv");
        pub const TRIVIAL_CHOOSE_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_choose_program.gv");
        pub const TRIVIAL_ODDS_PROGRAM: &'static str =
            include_str!("../tests/pts/trivial_odds_program.gv");
        pub const TRIVIAL_PROGRAM: &'static str = include_str!("../tests/pts/trivial_program.gv");
        pub const WHILE_LOGIC_PROGRAM: &'static str =
            include_str!("../tests/pts/while_logic_program.gv");
        pub const WHILE_NONDETERMINISTIC_PROGRAM: &'static str =
            include_str!("../tests/pts/while_nondeterministic_program.gv");
        pub const WHILE_PROB_PROGRAM: &'static str =
            include_str!("../tests/pts/while_prob_program.gv");
    }
    pub mod parsers {
        pub mod default {
            pub const INVARIANT_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/invariant_program");
            pub const SIMPLE_IF_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_if_program");
            pub const SIMPLE_CHOOSE_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_choose_program");
            pub const SIMPLE_ODDS_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_odds_program");
            pub const SIMPLE_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/simple_program");
            pub const TRIVIAL_IF_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_if_program");
            pub const TRIVIAL_CHOOSE_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_choose_program");
            pub const TRIVIAL_ODDS_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_odds_program");
            pub const TRIVIAL_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/trivial_program");
            pub const WHILE_LOGIC_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_logic_program");
            pub const WHILE_NONDETERMINISTIC_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_nondeterministic_program");
            pub const WHILE_PROB_PROGRAM: &'static str =
                include_str!("../tests/parsers/default/while_prob_program");
        }
    }
}
