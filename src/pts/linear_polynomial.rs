use crate::pts::variables::{Variable, VariableMap, VariableError};

#[derive(Debug)]
pub struct LinearPolynomial {
    coefficients: Vec<f64>
}

impl LinearPolynomial {
    pub fn new() -> Self {
        LinearPolynomial { coefficients: vec!() }
    }

    fn len(&self) -> usize {
        self.coefficients.len()
    }

    pub fn add_term(&mut self, 
                map: &VariableMap,
                var: &Variable,
                coefficient: f64) -> Result<(), VariableError>{
        let index = map.get_index(&var);

        // if variable is not in the map, its not a program variable
        if index.is_none() {
            Err(VariableError::new(var))   
        }
        else {
            if self.len() < map.len() {
                self.coefficients.resize(map.len(), 0.0);
            }
            self.coefficients[index.unwrap()] += coefficient;
            Ok(())
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<f64> {
        self.coefficients.get(index).map(|x| x.clone())
    }

}
