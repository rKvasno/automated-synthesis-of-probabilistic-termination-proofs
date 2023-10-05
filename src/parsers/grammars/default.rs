use pest_derive::Parser;
#[derive(Parser)]
#[grammar = "parsers/grammars/default.pest"]
#[grammar = "parsers/grammars/linear_polynomial.pest"]
pub struct DefaultParser;

#[cfg(test)]
mod tests {
    use pest_test_gen::pest_tests;
    #[pest_tests(
        crate::parsers::grammars::default::DefaultParser,
        crate::parsers::grammars::default::Rule,
        "program",
        skip_rule("term", "additive_op"),
        dir = "tests/parsers/grammars/default",
        no_eoi = true,
        lazy_static = true
    )]
    #[cfg(test)]
    mod pest_test {}
}
