use super::{alts, ParserError};

pub fn parse<InputStream: Iterator<char>>(input: InputStream)
                                        -> Result<alts::ALTS, ParserError> {
    todo!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn sanity() {
        assert_eq!(1, 1);
    }
}
