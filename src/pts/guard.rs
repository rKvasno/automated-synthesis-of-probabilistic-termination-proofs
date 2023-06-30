use crate::pts::transition::Transition;
use crate::pts::LinearPolynomial;

pub type Odds = u64;

// 32 bytes
pub enum Guards<'a>{
    Logic(Vec<(LinearPolynomial, Transition<'a>)>),
    Probabilistic(Vec<(Odds, Transition<'a>)>),
    Nondeterministic(Vec<Transition<'a>>),
    Unguarded(Box<Transition<'a>>)
}

#[cfg(test)]
mod tests {
    use std::mem;
    use super::{Odds, Guards};
    
    #[test]
    fn align_odds() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<Odds>(), 8);
        }
    }

    #[test]
    fn size_odds() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Odds>(), 8);
        }
    }

    #[test]
    fn align_guards() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::align_of::<Guards>(), 8);
        }
    }

    #[test]
    fn size_guards() {
        if mem::size_of::<usize>() == 8{
            assert_eq!(mem::size_of::<Guards>(), 32);
        }
    }
    
}

