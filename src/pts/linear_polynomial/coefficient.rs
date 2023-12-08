use std::{
    borrow::{Borrow, BorrowMut},
    fmt::Debug,
    iter::Sum,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign},
};

pub trait Coefficient:
    Default
    + Debug
    + PartialEq
    + Clone
    + AddAssign
    + SubAssign
    + Add<Output = Self>
    + Sub<Output = Self>
    + Neg<Output = Self>
    + Sum
{
    fn zero() -> Self;

    fn is_zero(&self) -> bool {
        self == &Self::zero()
    }

    fn mul_by_constant(&mut self, n: Constant);
}

// HINT: if something goes horribly wrong, it might just be -0.0
#[derive(Default, PartialEq, PartialOrd, Clone, Copy)]
pub struct Constant(pub f64);

impl std::fmt::Debug for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl Constant {
    pub const ONE: Self = Constant(1.0);

    pub fn pow(self, exponent: Self) -> Self {
        Constant(self.0.powf(exponent.0))
    }

    pub fn is_negative(&self) -> bool {
        // .is_sign_negative returns true for -NaN, -0.0 and -inf
        self.0 < 0.0
    }

    pub fn is_nonnegative(&self) -> bool {
        // .is_sign_negative returns true for -NaN, -0.0 and -inf
        self.0 >= 0.0
    }

    pub fn is_one(&self) -> bool {
        self == &Self::ONE
    }

    pub fn is_neg_one(&self) -> bool {
        self == &-Self::ONE
    }
}

impl Coefficient for Constant {
    fn zero() -> Self {
        Constant(0.0)
    }

    fn mul_by_constant(&mut self, n: Constant) {
        *self *= n;
    }
}

impl std::str::FromStr for Constant {
    type Err = std::num::ParseFloatError;

    fn from_str(src: &str) -> Result<Self, Self::Err> {
        src.parse::<f64>().map(Constant)
    }
}

impl From<f64> for Constant {
    fn from(val: f64) -> Self {
        Constant(val)
    }
}

impl Into<f64> for Constant {
    fn into(self) -> f64 {
        self.0
    }
}

impl Neg for Constant {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl Borrow<f64> for Constant {
    fn borrow(&self) -> &f64 {
        &self.0
    }
}

impl BorrowMut<f64> for Constant {
    fn borrow_mut(&mut self) -> &mut f64 {
        &mut self.0
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
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.fold(Constant(0.0), |a, b| a + b)
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Display::fmt(&self.0, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::pts::linear_polynomial::coefficient::{Coefficient, Constant};

    #[test]
    fn is_zero() {
        assert!(!Constant(f64::NAN).is_zero())
    }

    #[test]
    fn is_nonnegative() {
        assert!(!Constant(f64::NAN).is_nonnegative())
    }
}
