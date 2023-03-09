use std::fmt;

pub enum Arithmetic {
    Variable{name: Box<str>},
    Constant{value: f64},
    Sum(Box<Arithmetic>, Box<Arithmetic>),
    Product(Box<Arithmetic>, Box<Arithmetic>)
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
