use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parsers/grammars/default.pest"]
#[grammar = "parsers/grammars/linear_polynomial.pest"]
pub struct DefaultParser;

#[cfg(test)]
mod tests {
    use super::DefaultParser;
    use super::Rule;
    use pest::{ parses_to, consumes_to};
    use pest::Parser;
    use crate::parsers::grammars::misc::read_test_input;

    #[test]
    fn simple_program() {
        let input = read_test_input("simple_program");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::program,
            tokens: [
                program(0, 22, [
                    location(0, 13, [
                        invariants(0, 8, [
                            logic_condition(2, 7, [
                                linear_polynomial(2, 3, [
                                    term(2, 3, [
                                        constant_expr(2, 3, [
                                            constant(2, 3)
                                        ])
                                    ])
                                ]),
                                comparison_op(4, 5),
                                linear_polynomial(6, 7, [
                                    term(6, 7, [
                                        constant_expr(6, 7, [
                                            constant(6, 7)
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                        assign(8, 13, [
                            variable(8, 9),
                            linear_polynomial(12, 13, [
                                term(12, 13, [
                                    constant_expr(12, 13, [
                                        constant(12, 13)
                                    ])
                                ])
                            ])
                        ])
                    ]),
                    invariants(14, 22, [
                        logic_condition(16, 21, [
                            linear_polynomial(16, 17, [
                                term(16, 17, [
                                    constant_expr(16, 17, [
                                        constant(16, 17)
                                    ])
                                ])
                            ]),
                            comparison_op(18, 19),
                            linear_polynomial(20, 21, [
                                term(20, 21, [
                                    constant_expr(20, 21, [
                                        constant(20, 21)
                                    ])
                                ])
                            ])
                        ])
                    ]),
                    EOI(22, 22)
                ])
            ]
        };
    }

    #[test]
    fn simple_odds() {
        let input = read_test_input("simple_odds_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                odds(5, 55, [
                    constant(5, 6),
                    odds(7, 32, [
                        constant(7, 8),
                        constant(9, 10),
                        block(11, 32, [
                            location(15, 30, [
                                invariants(15, 25, [
                                    logic_condition(17, 22, [
                                        linear_polynomial(17, 18, [
                                            term(17, 18, [
                                                variable(17, 18)
                                            ])
                                        ]),
                                        comparison_op(19, 20),
                                        linear_polynomial(21, 22, [
                                            term(21, 22, [
                                                variable(21, 22)
                                            ])
                                        ])
                                    ])
                                ]),
                                assign(25, 30, [
                                    variable(25, 26),
                                    linear_polynomial(29, 30, [
                                        term(29, 30, [
                                            variable(29, 30)
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                    block(32, 55, [
                        location(37, 53, [
                            invariants(37, 48, [
                                logic_condition(39, 45, [
                                    linear_polynomial(39, 40, [
                                        term(39, 40, [
                                            variable(39, 40)
                                        ])
                                    ]),
                                    comparison_op(41, 43),
                                    linear_polynomial(44, 45, [
                                        term(44, 45, [
                                            variable(44, 45)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(48, 53, [
                                variable(48, 49),
                                linear_polynomial(52, 53, [
                                    term(52, 53, [
                                        variable(52, 53)
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn simple_branching() {
        let input = read_test_input("simple_branching_snippet");
        
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                branch(3, 95, [
                    logic_condition(3, 10, [
                        linear_polynomial(3, 4, [
                            term(3, 4, [
                                variable(3, 4)
                            ])
                        ]), 
                        comparison_op(5, 7),
                        linear_polynomial(8, 9, [
                            term(8, 9, [
                                variable(8, 9)
                            ])
                        ])
                    ]),
                    block(10, 31, [
                        location(14, 29, [
                            invariants(14, 24, [
                                logic_condition(16, 21, [
                                    linear_polynomial(16, 17, [
                                        term(16, 17, [
                                            variable(16, 17)
                                        ])
                                    ]),
                                    comparison_op(18, 19),
                                    linear_polynomial(20, 21, [
                                        term(20, 21, [
                                            variable(20, 21)
                                        ])
                                    ])
                                ])
                            ]), 
                            assign(24, 29, [
                                variable(24, 25), 
                                linear_polynomial(28, 29, [
                                    term(28, 29, [
                                        variable(28, 29)
                                    ])
                                ])
                            ])
                        ])
                    ]), 
                    elseif(37, 95, [
                        branch(40, 95, [
                            logic_condition(40, 47, [
                                linear_polynomial(40, 41, [
                                    term(40, 41, [
                                        variable(40, 41)
                                    ])
                                ]), 
                                comparison_op(42, 44),
                                linear_polynomial(45, 46, [
                                    term(45, 46, [
                                        variable(45, 46)
                                    ])
                                ])
                            ]),
                            block(47, 68, [
                                location(51, 66, [
                                    invariants(51, 61, [
                                        logic_condition(53, 58, [
                                            linear_polynomial(53, 54, [
                                                term(53, 54, [
                                                    variable(53, 54)
                                                ])
                                            ]),
                                            comparison_op(55, 56),
                                            linear_polynomial(57, 58, [
                                                term(57, 58, [
                                                    variable(57, 58)
                                                ])
                                            ])
                                        ])
                                    ]),
                                    assign(61, 66, [
                                        variable(61, 62),
                                        linear_polynomial(65, 66, [
                                            term(65, 66, [
                                                variable(65, 66)
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                            elseif(74, 95, [
                                block(74, 95, [
                                    location(78, 93, [
                                        invariants(78, 88, [
                                            logic_condition(80, 85, [
                                                linear_polynomial(80, 81, [
                                                    term(80, 81, [
                                                        variable(80, 81)
                                                    ])
                                                ]),
                                                comparison_op(82, 83),
                                                linear_polynomial(84, 85, [
                                                    term(84, 85, [
                                                        variable(84, 85)
                                                    ])
                                                ])
                                            ])
                                        ]),
                                        assign(88, 93, [
                                            variable(88, 89),
                                            linear_polynomial(92, 93, [
                                                term(92, 93, [
                                                    variable(92, 93)
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]
        }
    }

    #[test]
    fn simple_nondet() {
        let input = read_test_input("simple_nondet_snippet");
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                nondet(7, 78, [
                    block(7, 28, [
                        location(11, 26, [
                            invariants(11, 21, [
                                logic_condition(13, 18, [
                                    linear_polynomial(13, 14, [
                                        term(13, 14, [
                                            variable(13, 14)
                                        ])
                                    ]),
                                    comparison_op(15, 16),
                                    linear_polynomial(17, 18, [
                                        term(17, 18, [
                                            variable(17, 18)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(21, 26, [
                                variable(21, 22),
                                linear_polynomial(25, 26, [
                                    term(25, 26, [
                                        variable(25, 26)
                                    ])
                                ])
                            ])
                        ])
                    ]), 
                    nondet(32, 78, [
                        block(32, 53, [
                            location(36, 51, [
                                invariants(36, 46, [
                                    logic_condition(38, 43, [
                                        linear_polynomial(38, 39, [
                                            term(38, 39, [
                                                variable(38, 39)
                                            ])
                                        ]),
                                        comparison_op(40, 41),
                                        linear_polynomial(42, 43, [
                                            term(42, 43, [
                                                variable(42, 43)
                                            ])
                                        ])
                                    ])
                                ]),
                                assign(46, 51, [
                                    variable(46, 47),
                                    linear_polynomial(50, 51, [
                                        term(50, 51, [
                                            variable(50, 51)
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                        nondet(57, 78, [
                            block(57, 78, [
                                location(61, 76, [
                                    invariants(61, 71, [
                                        logic_condition(63, 68, [
                                            linear_polynomial(63, 64, [
                                                term(63, 64, [
                                                    variable(63, 64)
                                                ])
                                            ]),
                                            comparison_op(65, 66),
                                            linear_polynomial(67, 68, [
                                                term(67, 68, [
                                                    variable(67, 68)
                                                ])
                                            ])
                                        ])
                                    ]),
                                    assign(71, 76, [
                                        variable(71, 72),
                                        linear_polynomial(75, 76, [
                                            term(75, 76, [
                                                variable(75, 76)
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn simple_logic_while() {
        let input = read_test_input("simple_logic_while_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                dowhile(6, 47, [
                    logic_condition(6, 21, [
                        linear_polynomial(6, 7, [
                            term(6, 7, [
                                variable(6, 7)
                            ])
                        ]),
                        comparison_op(8, 9),
                        linear_polynomial(10, 11, [
                            term(10, 11, [
                                variable(10, 11)
                            ])
                        ]),
                        linear_polynomial(16, 17, [
                            term(16, 17, [
                                variable(16, 17)
                            ])
                        ]),
                        comparison_op(18, 19),
                        linear_polynomial(20, 21, [
                            term(20, 21, [
                                variable(20, 21)
                            ])
                        ])
                    ]),
                    block(22, 47, [
                        location(28, 45, [
                            invariants(28, 40, [
                                logic_condition(30, 35, [
                                    linear_polynomial(30, 31, [
                                        term(30, 31, [
                                            variable(30, 31)
                                        ])
                                    ]),
                                    comparison_op(32, 33),
                                    linear_polynomial(34, 35, [
                                        term(34, 35, [
                                            variable(34, 35)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(40, 45, [
                                variable(40, 41),
                                linear_polynomial(44, 45, [
                                    term(44, 45, [
                                        variable(44, 45)
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn simple_prob_while() {
        let input = read_test_input("simple_prob_while_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                dowhile(6, 37, [
                    prob_condition(6, 11, [
                        constant(6, 7),
                        constant(10, 11)
                    ]),
                    block(12, 37, [
                        location(18, 35, [
                            invariants(18, 30, [
                                logic_condition(20, 25, [
                                    linear_polynomial(20, 21, [
                                        term(20, 21, [
                                            variable(20, 21)
                                        ])
                                    ]),
                                    comparison_op(22, 23),
                                    linear_polynomial(24, 25, [
                                        term(24, 25, [
                                            variable(24, 25)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(30, 35, [
                                variable(30, 31),
                                linear_polynomial(34, 35, [
                                    term(34, 35, [
                                        variable(34, 35)
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])      
            ]
        };
    }

    #[test]
    fn simple_nondet_while() {
        let input = read_test_input("simple_nondet_while_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                dowhile(6, 38, [
                    nondet_condition(6, 12),
                    block(13, 38, [
                        location(19, 36, [
                            invariants(19, 31, [
                                logic_condition(21, 26, [
                                    linear_polynomial(21, 22, [
                                        term(21, 22, [
                                            variable(21, 22)
                                        ])
                                    ]),
                                    comparison_op(23, 24),
                                    linear_polynomial(25, 26, [
                                        term(25, 26, [
                                            variable(25, 26)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(31, 36, [
                                variable(31, 32),
                                linear_polynomial(35, 36, [
                                    term(35, 36, [
                                        variable(35, 36)
                                    ])
                                ])
                            ])
                        ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn invariants() {
        let input = read_test_input("invariants_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::location,
            tokens: [
                location(0, 29, [
                    invariants(0, 24, [
                        logic_condition(2, 7, [
                            linear_polynomial(2, 3, [
                                term(2, 3, [
                                    variable(2, 3)
                                ])
                            ]),
                            comparison_op(4, 5),
                            linear_polynomial(6, 7, [
                                term(6, 7, [
                                    variable(6, 7)
                                ])
                            ])
                        ]),
                        logic_condition(10, 15, [
                            linear_polynomial(10, 11, [
                                term(10, 11, [
                                    variable(10, 11)
                                ])
                            ]),
                            comparison_op(12, 13),
                            linear_polynomial(14, 15, [
                                term(14, 15, [
                                    variable(14, 15)
                                ])
                            ])
                        ]),
                        logic_condition(18, 23, [
                            linear_polynomial(18, 19, [
                                term(18, 19, [
                                    variable(18, 19)
                                ])
                            ]),
                            comparison_op(20, 21),
                            linear_polynomial(22, 23, [
                                term(22, 23, [
                                    variable(22, 23)
                                ])
                            ])
                        ])
                    ]),
                    assign(24, 29, [
                        variable(24, 25),
                        linear_polynomial(28, 29, [
                            term(28, 29, [
                                variable(28, 29)
                            ])
                        ])
                    ])
                ])
            ]
        }
    }

    #[test]
    fn complex_program() {
        let input = read_test_input("complex_program");
        assert!(DefaultParser::parse(Rule::program, &input).is_ok());
    }
    
}

