use super::{alts, ParserError, Read};

pub fn parse<Reader: Read>(input: Reader) -> Result<alts::ALTS, ParserError> {
    todo!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn sanity() {
        assert_eq!(1, 1);
    }
}
