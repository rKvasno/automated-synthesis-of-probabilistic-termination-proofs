use std::num::ParseFloatError;
use std::ops::{Neg, 
    AddAssign, SubAssign, MulAssign, DivAssign,
    Add, Sub, Mul, Div};
use std::str::FromStr;
use std::iter::Sum;
use std::convert::From;

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Constant(pub f64);

impl Constant {
    pub fn pow(self, exponent: Self) -> Self {
        Constant(self.0.powf(exponent.0))
    }
}

impl FromStr for Constant {
    type Err = ParseFloatError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        src.parse::<f64>().map(Constant)
    }
}

impl From<f64> for Constant {
    fn from(val: f64) -> Self {
        Constant(val)
    }
}

impl Neg for Constant {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl AddAssign for Constant {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
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
        self.0 -= other.0;
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
        self.0 *= other.0;
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
        self.0 /= other.0;
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
        iter.fold(Constant(0.0), |a, b| a + b)
    }
}

