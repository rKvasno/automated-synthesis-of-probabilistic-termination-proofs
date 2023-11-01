use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parsers/grammars/linear_polynomial.pest"]
pub struct PolynomialTestParser;

#[cfg(test)]
mod tests {

    use pest_test_gen::pest_tests;
    #[pest_tests(
        crate::parsers::grammars::linear_polynomial::PolynomialTestParser,
        crate::parsers::grammars::linear_polynomial::Rule,
        "linear_polynomial",
        dir = "tests/parsers/grammars/linear_polynomial",
        no_eoi = true
    )]
    #[cfg(test)]
    mod pest_test {}

    use super::PolynomialTestParser;
    use super::Rule;
    use pest::{consumes_to, fails_with, parses_to};

    #[test]
    fn invalid_polynomials() {
        parses_to! {
            parser: PolynomialTestParser,
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
            parser: PolynomialTestParser,
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
            parser: PolynomialTestParser,
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
            parser: PolynomialTestParser,
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
            parser: PolynomialTestParser,
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
        parser: PolynomialTestParser,
        input: "* 5",
        rule: Rule::linear_polynomial,
        positives: vec![Rule::linear_polynomial],
        negatives: vec![],
        pos: 0
        };

        fails_with! {
        parser: PolynomialTestParser,
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
            parser: PolynomialTestParser,
            input: "-1",
            rule: Rule::constant_expr,
            positives: vec![Rule::constant],
            negatives: vec![],
            pos: 0
        };

        fails_with! {
            parser: PolynomialTestParser,
            input: "(-1)",
            rule: Rule::constant_expr,
            positives: vec![Rule::constant],
            negatives: vec![],
            pos: 1
        }

        parses_to! {
            parser: PolynomialTestParser,
            input: "2 - 2",
            rule: Rule::constant_expr,
            tokens: [
               constant_expr(0, 1, [
                    constant(0, 1)
               ])
            ]
        };

        parses_to! {
            parser: PolynomialTestParser,
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
