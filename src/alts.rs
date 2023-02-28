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

