use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parsers/grammars/linear_polynomial.pest"]
pub struct LinearPolynomialTestParser;

#[cfg(test)]
mod tests {
    use super::LinearPolynomialTestParser;
    use super::Rule;
    use pest::{consumes_to, fails_with, parses_to};

    #[test]
    fn single_constant() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "-1.2",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 4, [
                    additive_op(0, 1),
                    term(1, 4, [
                         constant_expr(1, 4, [
                            constant(1, 4)
                         ])
                    ])
                ])
            ]
        };
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "- 1.2",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 5, [
                    additive_op(0, 1),
                    term(2, 5, [
                         constant_expr(2, 5, [
                            constant(2, 5)
                         ])
                    ])
                ])
            ]
        };
    }

    #[test]
    fn single_variable() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a1b2c3",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 6, [
                    term(0, 6, [
                        variable(0, 6)
                    ])
                ])
            ]
        };
    }

    #[test]
    fn complex_term() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "( 3 * 0 - (1^2/12) )a",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 21, [
                    term(0, 21, [
                        constant_expr(0, 20, [
                            constant_expr(2, 3, [
                                constant(2, 3)
                            ]),
                            multiplicative_op(4, 5),
                            constant_expr(6, 7, [
                                constant(6, 7)
                            ]),
                            additive_op(8, 9),
                            constant_expr(10, 18, [
                                constant_expr(11, 12, [
                                    constant(11, 12)
                                ]),
                                power_op(12, 13),
                                constant_expr(13, 14, [
                                    constant(13, 14)
                                ]),
                                multiplicative_op(14, 15),
                                constant_expr(15, 17, [
                                    constant(15, 17)
                                ])
                            ])
                        ]),
                        variable(20, 21)
                    ])
                ])
            ]

        };
    }

    #[test]
    fn term_variations() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "1a",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 2, [
                    term(0, 2, [
                        constant_expr(0, 1, [
                            constant(0, 1)
                        ]),
                        variable(1, 2)
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "5 * a",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 5, [
                    term(0, 5, [
                        constant_expr(0, 1, [
                            constant(0, 1),
                        ]),
                        variable(4, 5)
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a * 0",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 5, [
                    term(0, 5, [
                        variable(0, 1),
                        constant_expr(4, 5, [
                            constant(4, 5)
                        ]),
                    ])
                ])
            ]
        };
    }

    #[test]
    fn simple_polynomial() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a + 2b - 3 * c + 0 - 0",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 22, [
                    term(0, 1, [
                        variable(0, 1),
                    ]),
                    additive_op(2, 3),
                    term(4, 6, [
                        constant_expr(4, 5, [
                            constant(4, 5)
                        ]),
                        variable(5, 6)
                    ]),
                    additive_op(7, 8),
                    term(9, 14, [
                        constant_expr(9, 10, [
                            constant(9, 10)
                        ]),
                        variable(13, 14)
                    ]),
                    additive_op(15, 16),
                    term(17, 18, [
                        constant_expr(17, 18, [
                            constant(17, 18)
                        ])
                    ]),
                    additive_op(19, 20),
                    term(21, 22, [
                        constant_expr(21, 22, [
                            constant(21, 22)
                        ])
                    ]),
                ])
            ]
        };
    }

    #[test]
    fn invalid_polynomials() {
        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a * b",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 1, [
                    term(0, 1, [
                        variable(0, 1),
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a + + b",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 1, [
                    term(0, 1, [
                        variable(0, 1),
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "a b",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 1, [
                    term(0, 1, [
                        variable(0, 1)
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "5 a",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 1, [
                    term(0, 1, [
                        constant_expr(0, 1, [
                            constant(0, 1),
                        ]),
                    ])
                ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "1 2",
            rule: Rule::linear_polynomial,
            tokens: [
                linear_polynomial(0, 1, [
                    term(0, 1, [
                        constant_expr(0, 1, [
                            constant(0, 1)
                        ])
                    ])
                ])
            ]
        };

        fails_with! {
        parser: LinearPolynomialTestParser,
        input: "* 5",
        rule: Rule::linear_polynomial,
        positives: vec![Rule::linear_polynomial],
        negatives: vec![],
        pos: 0
        };

        fails_with! {
        parser: LinearPolynomialTestParser,
        input: "",
        rule: Rule::linear_polynomial,
        positives: vec![Rule::linear_polynomial],
        negatives: vec![],
        pos: 0
        };
    }

    #[test]
    fn invalid_constant_expr() {
        fails_with! {
            parser: LinearPolynomialTestParser,
            input: "-1",
            rule: Rule::constant_expr,
            positives: vec![Rule::constant],
            negatives: vec![],
            pos: 0
        };

        fails_with! {
            parser: LinearPolynomialTestParser,
            input: "(-1)",
            rule: Rule::constant_expr,
            positives: vec![Rule::constant],
            negatives: vec![],
            pos: 1
        }

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "2 - 2",
            rule: Rule::constant_expr,
            tokens: [
               constant_expr(0, 1, [
                    constant(0, 1)
               ])
            ]
        };

        parses_to! {
            parser: LinearPolynomialTestParser,
            input: "(3 + 4)(3 * 0)",
            rule: Rule::constant_expr,
            tokens: [
                constant_expr(0, 7, [
                    constant_expr(1, 2, [
                        constant(1, 2)
                    ]),
                    additive_op(3, 4),
                    constant_expr(5, 6, [
                        constant(5, 6)
                    ])
                ])
            ]
        };
    }
}
