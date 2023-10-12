
use std::{collections::HashMap, rc::Rc};
use serde::{Serialize, Deserialize};

type ID = String;
type Interpretation = HashMap<ID, f64>;

macro_rules! interit (
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = Interpretation::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
     };
);

pub trait PropT {
    fn interpret(&self, interpretation: &Interpretation) -> f64;
    fn negate(&self) -> Rc<Proposition>;
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Proposition {
    AtLeast(AtLeast),
    AtMost(AtMost),
    Variable(Variable),
    And(And),
    Or(Or),
    Xor(Xor),
    XNor(XNor),
    Implies(Implies),
}

impl PropT for Proposition {
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        match self {
            Proposition::AtLeast(at_least) => at_least.interpret(interpretation),
            Proposition::AtMost(at_most) => at_most.interpret(interpretation),
            Proposition::Variable(variable) => variable.interpret(interpretation),
            Proposition::And(and) => and.interpret(interpretation),
            Proposition::Or(or) => or.interpret(interpretation),
            Proposition::Xor(xor) => xor.interpret(interpretation),
            Proposition::XNor(xnor) => xnor.interpret(interpretation),
            Proposition::Implies(implies) => implies.interpret(interpretation),
        }
    }
    fn negate(&self) -> Rc<Proposition> {
        match self {
            Proposition::AtLeast(at_least) => at_least.negate(),
            Proposition::AtMost(at_most) => at_most.negate(),
            Proposition::Variable(variable) => Rc::new(
                Proposition::Variable(
                    variable.clone(),
                ),
            ),
            Proposition::And(and) => and.negate(),
            Proposition::Or(or) => or.negate(),
            Proposition::Xor(xor) => xor.negate(),
            Proposition::XNor(xnor) => xnor.negate(),
            Proposition::Implies(implies) => implies.negate(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Variable {
    pub id: ID,
}

impl Variable {
    pub fn interpret(&self, interpretation: &Interpretation) -> f64 {
        *interpretation.get(&self.id).unwrap_or(&0.0)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AtLeast {
    pub id: Option<ID>,
    pub value: f64,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for AtLeast {

    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() >= self.value
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtMost(
                AtMost {
                    value: self.value - 1.0,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AtMost {
    pub id: Option<ID>,
    pub value: f64,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for AtMost {

    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() <= self.value
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtLeast(
                AtLeast {
                    value: self.value - 1.0,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct And {
    pub id: Option<ID>,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for And {
    
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() - (self.variables.len() as f64) >= 0.0
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtMost(
                AtMost {
                    value: (self.variables.len() as f64) - 1.0,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Or {
    pub id: Option<ID>,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for Or {
        
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() >= 1.0
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtLeast(
                AtLeast {
                    value: 1.0,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Xor {
    pub id: Option<ID>,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for Xor {
        
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() == 1.0
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::XNor(
                XNor {
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct XNor {
    pub id: Option<ID>,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for XNor {
        
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<f64>() != 1.0
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::Xor(
                Xor {
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Implies {
    pub id: Option<ID>,
    pub left: Rc<Proposition>,
    pub right: Rc<Proposition>,
}

impl PropT for Implies {
            
    fn interpret(&self, interpretation: &Interpretation) -> f64 {
        (
            (1.0 -self.left.interpret(interpretation)) + self.right.interpret(interpretation) >= 1.0
        ) as i32 as f64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::And(
                And {
                    variables: vec![
                        Rc::clone(&self.left),
                        self.right.negate(),
                    ],
                    id: self.id.clone(),
                }
            )
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct PropKey {
    prop: Proposition,
    key: String,
}

impl PropKey {
    fn new(prop: Proposition, key: String) -> PropKey {
        PropKey {
            prop,
            key,
        }
    }
    pub fn evaluate(&self, interpretation: &Interpretation) -> Option<String> {
        if self.prop.interpret(interpretation) == 1.0 {
            Some(self.key.clone())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let a = Implies {
            left: Rc::new (
                Proposition::And(
                    And {
                        variables: vec![
                            Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                            Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                        ],
                        id: None,
                    }
                )
            ),
            right: Rc::new (
                Proposition::And( 
                    And {
                        variables: vec![
                            Rc::new( Proposition::Variable( Variable {id: String::from("x") } ) ),
                            Rc::new( Proposition::Variable( Variable {id: String::from("y") } ) ),
                            Rc::new( Proposition::Variable( Variable {id: String::from("z") } ) ),
                        ],
                        id: None,
                    }
                )
            ),
            id: None,
        };
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 0.0
                }
            ), 
            1.0
        );
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 1.0,
                    String::from("b") => 1.0
                }
            ), 
            0.0
        );
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 1.0,
                    String::from("b") => 1.0,
                    String::from("x") => 1.0,
                    String::from("y") => 1.0,
                    String::from("z") => 1.0
                }
            ), 
            1.0
        );

        let b = a.negate();
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 0.0
                }
            ), 
            0.0
        );
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 1.0,
                    String::from("b") => 1.0
                }
            ), 
            1.0
        );
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 1.0,
                    String::from("b") => 1.0,
                    String::from("x") => 1.0,
                    String::from("y") => 1.0,
                    String::from("z") => 1.0
                }
            ), 
            0.0
        );
    }
}
