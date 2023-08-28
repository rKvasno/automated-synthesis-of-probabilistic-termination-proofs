use std::num::ParseFloatError;
use std::ops::{Neg, 
    AddAssign, SubAssign, MulAssign, DivAssign,
    Add, Sub, Mul, Div};
use std::str::FromStr;
use std::iter::Sum;

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Constant {
    value:f64
}

pub const ZERO: Constant = Constant{ value: 0.0 };
pub const ONE: Constant = Constant{ value: 1.0 };

impl Constant {
    pub fn new(value: f64) -> Self {
        Constant{value}
    }

    pub fn pow(self, exponent: Self) -> Self {
        Constant::new(self.value.powf(exponent.value))
    }
}

impl FromStr for Constant {
    type Err = ParseFloatError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        src.parse::<f64>().map(Constant::new)
    }
}

impl Neg for Constant {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::new(-self.value)
    }
}

impl AddAssign for Constant {
    fn add_assign(&mut self, other: Self) {
        self.value += other.value;
    }
}

impl Add for Constant {
    type Output = Self;

    fn add(mut self, other: Self) -> Self::Output {
        self += other;
        self
    }
}

impl SubAssign for Constant {
    fn sub_assign(&mut self, other: Self) {
        self.value -= other.value;
    }
}

impl Sub for Constant {
    type Output = Self;

    fn sub(mut self, other: Self) -> Self::Output {
        self -= other;
        self
    }
}

impl MulAssign for Constant {
    fn mul_assign(&mut self, other: Self) {
        self.value *= other.value;
    }
}

impl Mul for Constant {
    type Output = Self;

    fn mul(mut self, other: Self) -> Self::Output {
        self *= other;
        self
    }
}

impl DivAssign for Constant {
    fn div_assign(&mut self, other: Self) {
        self.value /= other.value;
    }
}

impl Div for Constant {
    type Output = Self;

    fn div(mut self, other: Self) -> Self::Output {
        self /= other;
        self
    }
}

impl Sum for Constant {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        iter.fold(ZERO, |a, b| a + b)
    }
}
