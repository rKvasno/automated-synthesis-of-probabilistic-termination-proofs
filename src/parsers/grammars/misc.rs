use pest::{Parser, RuleType};

use std::fs::read_to_string;
use std::env::var;

pub fn print_rule_parsing<R: RuleType, P: Parser<R>>(rule: R, input: &str) {
    let parsed = P::parse(rule, input).unwrap();
    for pair in parsed.clone().flatten() {
        print!("{:?}:\n{}\n######################## ", pair.as_rule(), pair.as_str());
    }
    panic!("{}", parsed);
}

pub fn read_test_input(input_file: &str) -> String {
    let dir = var("CARGO_MANIFEST_DIR").unwrap() + "/src/parsers/grammars/default_programs/" + input_file;
    read_to_string(dir.clone()).unwrap_or_else(|_| panic!("Can't find {}", dir))
}
