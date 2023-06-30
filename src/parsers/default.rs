use super::{pts, ParserError, Read};

pub fn parse<'a, Reader: Read>(input: Reader) -> Result<pts::PTS<'a>, ParserError> {
    todo!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn sanity() {
        assert_eq!(1, 1);
    }
}
