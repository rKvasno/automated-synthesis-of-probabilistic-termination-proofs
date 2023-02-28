use core::fmt;

enum Arithmetic {
    Variable{name: Box<str>},
    Constant{value: f64},
    Sum(Box<Arithmetic>, Box<Arithmetic>),
    Product(Box<Arithmetic>, Box<Arithmetic>)
}

enum Formula {
    Disjunction(Box<Formula>),
    Conjuntion(Box<Formula>),
    Negation(Box<Formula>),
    Expression(Box<Arithmetic>)
}

enum Guard {
    Conditional(Box<Formula>),
    Probabilistic{probability: f64},
    Nondeterministic
}

enum Location {
    Exit{invariant: Formula},
    Instruction{invariant: Formula, next: Box<Location>},
    Branching{invariant: Formula,
              guard: Guard,
              left: Box<Location>,
              right: Box<Location>}
}

// Annotated Labeled Transition System
pub struct ALTS {
    start: Box<Location>,
}

impl fmt::Display for Arithmetic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) ->  fmt::Result {
        match self {
            Self::Variable { name } => write!(f, "{}", name),
            Self::Constant { value } => write!(f, "{}", value),
            Self::Sum(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Self::Product(lhs, rhs) => write!(f, "({} * {})", lhs, rhs)
        }
    }
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) ->  fmt::Result {
        match self {
            Self::Expression( expr ) => write!(f, "{}", expr),
            Self::Negation( form ) => write!(f, "!({})", form),
            Self::Conjuntion(lhs, rhs) => write!(f, "({} && {})", lhs, rhs),
            Self::Disjunction(lhs, rhs) => write!(f, "({} || {})", lhs, rhs)
        }
    }
}

