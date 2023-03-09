use std::fmt;
use crate::alts::formula::Formula;

pub enum Guard {
    Conditional(Box<Formula>),
    Probabilistic{probability: f64},
    Nondeterministic
}

impl fmt::Display for Guard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nondeterministic => write!(f, "?"),
            Self::Probabilistic { probability } => write!(f, "{:.3}", probability),
            Self::Conditional( formula ) => write!(f, "{}", formula),
        }
    }
}


