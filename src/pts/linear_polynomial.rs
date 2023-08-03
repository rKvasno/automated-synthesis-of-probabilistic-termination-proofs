use crate::pts::variable_map::{Variable, VariableMap, VariableError};

pub type Constant = f64;

#[derive(Debug)]
pub struct LinearPolynomial {
    coefficients: Vec<Constant>
}

impl LinearPolynomial {
    pub fn new() -> Self {
        LinearPolynomial { coefficients: vec!(0.0) }
    }

    fn len(&self) -> usize {
        self.coefficients.len()
    }

    fn resize(&mut self, map: &VariableMap) {
        if self.len() < map.len() {
            // +1 for constant term
            self.coefficients.resize(map.len() + 1, 0.0);
        }
    }

    pub fn add_term(&mut self, 
                    map: &VariableMap,
                    var: Option<&Variable>,
                    coefficient: Constant) -> Result<(), VariableError> {
        self.resize(map);
        if var.is_none() {
            self.coefficients[0] += coefficient;
            Ok(())
        }
        else{
            let index = map.get_index(&var.unwrap());
            // if variable is not in the map, its not a program variable
            if index.is_none() {
                Err(VariableError::new(var.unwrap()))   
            }
            else {
                self.coefficients[index.unwrap()] += coefficient;
                Ok(())
            }
        }
    }

    pub fn get_coefficient(&self, index: usize) -> Option<Constant> {
        self.coefficients.get(index).map(|x| x.clone())
    }

}

#[cfg(test)]
mod tests {
    use super::{LinearPolynomial, VariableMap, Variable, VariableError, Constant};

    fn setup_map() -> VariableMap {
        let mut map = VariableMap::new();
        map.find_or_add(Variable::new("a"));
        map.find_or_add(Variable::new("b"));
        map.find_or_add(Variable::new("c"));
        map
    }

    fn check_terms(pol: &LinearPolynomial, map: &VariableMap, vec: Vec<Option<Constant>>) {
        assert_eq!(map.len() +1, vec.len());
        assert!(map.len() <= pol.len());
        for (index, element) in vec.iter().enumerate() {
            if element.is_some() {
                assert_eq!(pol.get_coefficient(index).unwrap(), element.unwrap());
            }
        }
    }

    #[test]
    fn add_resizing() {
        let mut pol = LinearPolynomial::new();
        assert_eq!(pol.len(), 1);
        let map = setup_map();
        pol.add_term(&map, map.get_variable(1), 0.0).unwrap();
        assert_eq!(pol.len(), map.len() + 1);
        check_terms(&pol, &map, vec!(Some(0.0), Some(0.0), Some(0.0), Some(0.0)));
    }
    
    #[test]
    fn add_variable() {
        let mut pol = LinearPolynomial::new();
        let map = setup_map();
        let b = Variable::new("b");
        pol.add_term(&map, Some(&b), 1.0).unwrap();
        check_terms(&pol, &map, vec!(Some(0.0), Some(0.0), Some(1.0), Some(0.0)));
        pol.add_term(&map, Some(&b), 1.0).unwrap();
        check_terms(&pol, &map, vec!(Some(0.0), Some(0.0), Some(2.0), Some(0.0)));
    }

    #[test]
    fn add_constant() {
        let mut pol = LinearPolynomial::new();
        let map = setup_map();
        pol.add_term(&map, None, 1.0).unwrap();
        check_terms(&pol, &map, vec!(Some(1.0), Some(0.0), Some(0.0), Some(0.0)));
        pol.add_term(&map, None, 1.0).unwrap();
        check_terms(&pol, &map, vec!(Some(2.0), Some(0.0), Some(0.0), Some(0.0)));
    }

    #[test]
    fn add_out_of_bounds() {
        let mut pol = LinearPolynomial::new();
        let map = setup_map();
        let e = Variable::new("e");
        assert_eq!(pol.add_term(&map, Some(&e), 1.0), Err(VariableError::new(&e)));
        check_terms(&pol, &map, vec!(Some(0.0), Some(0.0), Some(0.0), Some(0.0)));
    }

}
