use std::fmt;
use crate::alts::arithmetic::Arithmetic;

pub enum Formula {
    Disjunction(Box<Formula>, Box<Formula>),
    Conjuntion(Box<Formula>, Box<Formula>),
    Negation(Box<Formula>),
    Expression(Box<Arithmetic>),
    Constant(bool)
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) ->  fmt::Result {
        match self {
            Self::Expression( expr ) => write!(f, "{}", expr),
            Self::Negation( form ) => write!(f, "!({})", form),
            Self::Conjuntion(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
            Self::Disjunction(lhs, rhs) => write!(f, "({} || {})", lhs, rhs),
            Self::Constant(value) => write!(f, "{}", value)
        }
    }
}


