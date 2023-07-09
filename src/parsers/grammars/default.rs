use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parsers/grammars/default.pest"]
#[grammar = "parsers/grammars/linear_polynomial.pest"]
pub struct DefaultParser;

#[cfg(test)]
mod tests {
    use super::DefaultParser;
    use super::Rule;
    use pest::RuleType;
    use pest::{ parses_to, fails_with, consumes_to};
    use std::fs::read_to_string;
    use std::env::var;
    use pest::Parser;
    
    fn print_rule_parsing<R: RuleType, P: Parser<R>>(input: &str, rule: R) {
        let parsed = P::parse(rule, input).unwrap();
        for pair in parsed.clone().flatten() {
            print!("{:?}:\n'{}'\n", pair.as_rule(), pair.as_str());
        }
        panic!("{}", parsed);
    }

    fn read_test_input(input_file: &str) -> String {
        let dir = var("CARGO_MANIFEST_DIR").unwrap() + "/src/parsers/grammars/default_programs/" + input_file;
        read_to_string(dir.clone()).unwrap_or_else(|_| panic!("Can't find {}", dir))
    }

    #[test]
    fn simple_program() {
        let input = read_test_input("simple_program");
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::program,
            tokens: [
                program(0, 14, [
                    location(0, 13, [
                        invariants(0, 8, [
                            condition(2, 7, [
                                linear_polynomial(2, 3, [
                                    term(2, 3, [
                                        constant_expr(2, 3, [
                                            constant(2, 3)
                                        ])
                                    ])
                                ]),
                                comparator(4, 5),
                                linear_polynomial(6, 7, [
                                    term(6, 7, [
                                        constant_expr(6, 7, [
                                            constant(6, 7)
                                        ])
                                    ])
                                ]),
                            ]),
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
                    EOI(14, 14)
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
                odds(5, 53, [
                    constant(5, 6),
                    odds(7, 30, [
                        constant(7, 8), 
                        block(9, 30, [
                            location(13, 28, [
                                invariants(13, 23, [
                                    condition(15, 20, [
                                        linear_polynomial(15, 16, [
                                            term(15, 16, [
                                                variable(15, 16)
                                            ])
                                        ]), 
                                        comparator(17, 18),
                                        linear_polynomial(19, 20, [
                                            term(19, 20, [
                                                 variable(19, 20)
                                            ])
                                        ])
                                    ])
                                ]), 
                                assign(23, 28, [
                                    variable(23, 24), 
                                    linear_polynomial(27, 28, [
                                        term(27, 28, [
                                            variable(27, 28)
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]), 
                    block(30, 53, [
                        location(35, 51, [
                            invariants(35, 46, [
                                condition(37, 43, [
                                    linear_polynomial(37, 38, [
                                        term(37, 38, [
                                            variable(37, 38)
                                        ])
                                    ]), 
                                    comparator(39, 41), 
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
                    condition(3, 9, [
                        linear_polynomial(3, 4, [
                            term(3, 4, [
                                variable(3, 4)
                            ])
                        ]), 
                        comparator(5, 7),
                        linear_polynomial(8, 9, [
                            term(8, 9, [
                                variable(8, 9)
                            ])
                        ])
                    ]),
                    block(10, 31, [
                        location(14, 29, [
                            invariants(14, 24, [
                                condition(16, 21, [
                                    linear_polynomial(16, 17, [
                                        term(16, 17, [
                                            variable(16, 17)
                                        ])
                                    ]),
                                    comparator(18, 19),
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
                            condition(40, 46, [
                                linear_polynomial(40, 41, [
                                    term(40, 41, [
                                        variable(40, 41)
                                    ])
                                ]), 
                                comparator(42, 44),
                                linear_polynomial(45, 46, [
                                    term(45, 46, [
                                        variable(45, 46)
                                    ])
                                ])
                            ]),
                            block(47, 68, [
                                location(51, 66, [
                                    invariants(51, 61, [
                                        condition(53, 58, [
                                            linear_polynomial(53, 54, [
                                                term(53, 54, [
                                                    variable(53, 54)
                                                ])
                                            ]),
                                            comparator(55, 56),
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
                                            condition(80, 85, [
                                                linear_polynomial(80, 81, [
                                                    term(80, 81, [
                                                        variable(80, 81)
                                                    ])
                                                ]),
                                                comparator(82, 83),
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
    fn simple_while() {
        let input = read_test_input("simple_while_snippet");

        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                dowhile(6, 37, [
                    condition(6, 11, [
                        linear_polynomial(6, 7, [
                            term(6, 7, [
                                variable(6, 7)
                            ])
                        ]),
                        comparator(8, 9),
                        linear_polynomial(10, 11, [
                            term(10, 11, [
                                variable(10, 11)
                            ])
                        ])
                    ]),
                    block(12, 37, [
                        location(18, 35, [
                            invariants(18, 30, [
                                condition(20, 25, [
                                    linear_polynomial(20, 21, [
                                        term(20, 21, [
                                            variable(20, 21)
                                        ])
                                    ]),
                                    comparator(22, 23),
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
    fn simple_nondet() {
        let input = read_test_input("simple_nondet_snippet");
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
                nondet(8, 79, [
                    block(8, 29, [
                        location(12, 27, [
                            invariants(12, 22, [
                                condition(14, 19, [
                                    linear_polynomial(14, 15, [
                                        term(14, 15, [
                                            variable(14, 15)
                                        ])
                                    ]),
                                    comparator(16, 17),
                                    linear_polynomial(18, 19, [
                                        term(18, 19, [
                                            variable(18, 19)
                                        ])
                                    ])
                                ])
                            ]),
                            assign(22, 27, [
                                variable(22, 23),
                                linear_polynomial(26, 27, [
                                    term(26, 27, [
                                        variable(26, 27)
                                    ])
                                ])
                            ])
                        ])
                    ]), 
                    nondet(33, 79, [
                        block(33, 54, [
                            location(37, 52, [
                                invariants(37, 47, [
                                    condition(39, 44, [
                                        linear_polynomial(39, 40, [
                                            term(39, 40, [
                                                variable(39, 40)
                                            ])
                                        ]),
                                        comparator(41, 42),
                                        linear_polynomial(43, 44, [
                                            term(43, 44, [
                                                variable(43, 44)
                                            ])
                                        ])
                                    ])
                                ]),
                                assign(47, 52, [
                                    variable(47, 48),
                                    linear_polynomial(51, 54, [
                                        term(51, 52, [
                                            variable(51, 52)
                                        ])
                                    ])
                                ])
                            ])
                        ]),
                        nondet(58, 79, [
                            block(58, 79, [
                                location(62, 77, [
                                    invariants(62, 72, [
                                        condition(64, 69, [
                                            linear_polynomial(64, 65, [
                                                term(64, 65, [
                                                    variable(64, 65)
                                                ])
                                            ]),
                                            comparator(66, 67),
                                            linear_polynomial(68, 69, [
                                                term(68, 69, [
                                                    variable(68, 69)
                                                ])
                                            ])
                                        ])
                                    ]),
                                    assign(72, 77, [
                                        variable(72, 73),
                                        linear_polynomial(76, 77, [
                                            term(76, 77, [
                                                variable(76, 77)
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
    fn invariants() {
        let input = read_test_input("invariants_snippet");
        print_rule_parsing::<Rule, DefaultParser>(&input, Rule::invariants);
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
            ]
        }
    }

    #[test]
    fn complex_program() {
        let input = read_test_input("complex_program");
        print_rule_parsing::<Rule, DefaultParser>(&input, Rule::instruction);
        parses_to! {
            parser: DefaultParser,
            input: input.as_str(),
            rule: Rule::instruction,
            tokens: [
            ]
        }
    }

        // TODO odds rule sa da rozbalit s queue, cize asi ho nechat tak, i ked asi by sa to dalo
        // lepsie povedat
        //
        // inicializaciu netreba, to vydefaultujeme na 0
        //
        //
        // TODO rn mam log/prob/nondet branching, co je turing complete, ale nemam looping taky, co
        // je trosku chybajuca syntax, co je problem, kedze samotny algoritmus je citlivy na syntax
        // TODO rovnako nemam probabilisticke if or nothing, co je equivalentne
        // if {} else {nothing}, co nie je validne a aj keby bolo, je to ina syntax, cize mozno
        // pridat do odds extra odd pre nothing.... co sa bude este horsie rozbalovat
}

