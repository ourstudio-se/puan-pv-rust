
use std::{collections::HashMap, rc::Rc};
use serde::{Serialize, Deserialize};
use sha2::{Digest, Sha256};
use hex::encode;

type ID = String;
type Interpretation = HashMap<ID, i64>;

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

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Direction {
    Positive,
    Negative,
}

impl Direction {
    pub fn flip(&self, value: i64) -> i64 {
        match self {
            Direction::Positive => value,
            Direction::Negative => -value,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum SDICType {
    Composite(SDIC),
    Primitive(ID),
}

impl SDICType {

    pub fn id(&self) -> ID {
        match self {
            SDICType::Composite(composite) => composite.id(),
            SDICType::Primitive(primitive) => primitive.clone(),
        }
    }

    pub fn interpret(&self, interpretation: &Interpretation) -> i64 {
        match self {
            SDICType::Composite(composite) => composite.interpret(interpretation),
            SDICType::Primitive(primitive) => *interpretation.get(primitive).unwrap_or(&0),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SDIC {
    direction: Direction,
    bias: i64,
    variables: Vec<Rc<SDICType>>,
}

impl SDIC {

    pub fn id(&self) -> String {
        let mut hasher = Sha256::new();

        // Hash the direction
        hasher.update(format!("{:?}", self.direction));

        // Hash the bias
        hasher.update(self.bias.to_be_bytes());

        // Hash each variable in the vector
        for variable in self.variables.iter().cloned() {
            hasher.update(format!("{:?}", variable));
        }

        // Finalize the hash and convert it to a string
        let hash_result = hasher.finalize();
        encode(hash_result)
    }

    pub fn interpret(&self, interpretation: &Interpretation) -> i64 {
        return (
            self.variables.iter().map(
                |variable| self.direction.flip(variable.interpret(interpretation))
            ).sum::<i64>() + self.bias >= 0
        ) as i64;
    }

}

pub trait PropT {
    fn interpret(&self, interpretation: &Interpretation) -> i64;
    fn negate(&self) -> Rc<Proposition>;
    fn to_sdic(&self) -> SDICType;
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
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
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
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
    fn to_sdic(&self) -> SDICType {
        match self {
            Proposition::AtLeast(at_least) => at_least.to_sdic(),
            Proposition::AtMost(at_most) => at_most.to_sdic(),
            Proposition::Variable(variable) => SDICType::Primitive(
                variable.id.clone(),
            ),
            Proposition::And(and) => and.to_sdic(),
            Proposition::Or(or) => or.to_sdic(),
            Proposition::Xor(xor) => xor.to_sdic(),
            Proposition::XNor(xnor) => xnor.to_sdic(),
            Proposition::Implies(implies) => implies.to_sdic(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Variable {
    pub id: ID,
}

impl Variable {
    pub fn interpret(&self, interpretation: &Interpretation) -> i64 {
        *interpretation.get(&self.id).unwrap_or(&0)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AtLeast {
    pub value: i64,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for AtLeast {

    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() >= self.value
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtMost(
                AtMost {
                    value: self.value - 1,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Positive,
                bias: -self.value,
                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AtMost {
    pub value: i64,
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for AtMost {

    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() <= self.value
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtLeast(
                AtLeast {
                    value: self.value - 1,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Negative,
                bias: self.value,
                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct And {
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for And {
    
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() - (self.variables.len() as i64) >= 0
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtMost(
                AtMost {
                    value: (self.variables.len() as i64) - 1,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Positive,
                bias: -(self.variables.len() as i64),
                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Or {
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for Or {
        
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() >= 1
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::AtLeast(
                AtLeast {
                    value: 1,
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Positive,
                bias: -1,
                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Xor {
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for Xor {
        
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() == 1
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::XNor(
                XNor {
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Positive,
                bias: -2,
                variables: vec![
                    Rc::new(
                        SDICType::Composite(
                            SDIC {
                                direction: Direction::Positive,
                                bias: -1,
                                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
                            }
                        )
                    ),
                    Rc::new(
                        SDICType::Composite(
                            SDIC {
                                direction: Direction::Negative,
                                bias: 1,
                                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
                            }
                        )
                    ),
                ],
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct XNor {
    pub variables: Vec<Rc<Proposition>>
}

impl PropT for XNor {
        
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            self.variables
                .iter()
                .map(|variable| variable.interpret(interpretation)) 
                .sum::<i64>() != 1
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::Xor(
                Xor {
                    variables: self.variables.iter().map(
                        |variable| 
                        Rc::clone(&variable)
                    ).collect(),
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Negative,
                bias: 1,
                variables: vec![
                    Rc::new(
                        SDICType::Composite(
                            SDIC {
                                direction: Direction::Positive,
                                bias: -1,
                                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
                            }
                        )
                    ),
                    Rc::new(
                        SDICType::Composite(
                            SDIC {
                                direction: Direction::Negative,
                                bias: 1,
                                variables: self.variables.iter().map(|variable| Rc::new(variable.to_sdic())).collect(),
                            }
                        )
                    ),
                ],
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Implies {
    pub left: Rc<Proposition>,
    pub right: Rc<Proposition>,
}

impl PropT for Implies {
            
    fn interpret(&self, interpretation: &Interpretation) -> i64 {
        (
            (1 -self.left.interpret(interpretation)) + self.right.interpret(interpretation) >= 1
        ) as i32 as i64
    }

    fn negate(&self) -> Rc<Proposition> {
        Rc::new(
            Proposition::And(
                And {
                    variables: vec![
                        Rc::clone(&self.left),
                        self.right.negate(),
                    ],
                }
            )
        )
    }

    fn to_sdic(&self) -> SDICType {
        SDICType::Composite(
            SDIC {
                direction: Direction::Positive,
                bias: -1,
                variables: vec![
                    Rc::new(self.left.negate().to_sdic()),
                    Rc::new(self.right.to_sdic()),
                ],
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::iproduct;

    #[test]
    fn test_sdic_structure() {
        let s = SDIC {
            direction: Direction::Positive,
            bias: -3,
            variables: vec![
                Rc::new(
                    SDICType::Primitive(
                        String::from("a")
                    )
                ),
                Rc::new(
                    SDICType::Primitive(
                        String::from("b")
                    )
                ),
                Rc::new(
                    SDICType::Primitive(
                        String::from("c")
                    )
                ),
            ],
        };
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("c") => 1
                }
            ), 
            1
        );
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("c") => 0
                }
            ), 
            0
        );

        let s: SDIC = SDIC {
            direction: Direction::Positive,
            bias: -2,
            variables: vec![
                Rc::new(
                    SDICType::Composite(
                        SDIC {
                            direction: Direction::Positive,
                            bias: -1,
                            variables: vec![
                                Rc::new(
                                    SDICType::Primitive(
                                        String::from("a")
                                    )
                                ),
                                Rc::new(
                                    SDICType::Primitive(
                                        String::from("b")
                                    )
                                ),
                            ]
                        }
                    )
                ),
                Rc::new(
                    SDICType::Composite(
                        SDIC {
                            direction: Direction::Positive,
                            bias: -1,
                            variables: vec![
                                Rc::new(
                                    SDICType::Primitive(
                                        String::from("c")
                                    )
                                ),
                                Rc::new(
                                    SDICType::Primitive(
                                        String::from("d")
                                    )
                                ),
                            ]
                        }
                    )
                ),
            ]
        };
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("c") => 1,
                    String::from("d") => 1
                }
            ), 
            1
        );
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 0,
                    String::from("b") => 0,
                    String::from("c") => 1,
                    String::from("d") => 1
                }
            ), 
            0
        );
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("c") => 0,
                    String::from("d") => 0
                }
            ), 
            0
        );
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 0,
                    String::from("b") => 1,
                    String::from("c") => 1,
                    String::from("d") => 0
                }
            ), 
            1
        );
        assert_eq!(
            s.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 0,
                    String::from("c") => 0,
                    String::from("d") => 1
                }
            ), 
            1
        );
    }

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
                    }
                )
            ),
        };
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 0
                }
            ), 
            1
        );
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1
                }
            ), 
            0
        );
        assert_eq!(
            a.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("x") => 1,
                    String::from("y") => 1,
                    String::from("z") => 1
                }
            ), 
            1
        );

        let b = a.negate();
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 0
                }
            ), 
            0
        );
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1
                }
            ), 
            1
        );
        assert_eq!(
            b.interpret(
                &interit! {
                    String::from("a") => 1,
                    String::from("b") => 1,
                    String::from("x") => 1,
                    String::from("y") => 1,
                    String::from("z") => 1
                }
            ), 
            0
        );
    }

    #[test]
    fn test_prop_sdic_conversion() {

        fn evaluate(prop: Proposition, sdic: SDICType) {
            for (a, b, c, d) in iproduct!([0,1], [0,1], [0,1], [0,1]) {
                assert_eq!(
                    prop.interpret(
                        &interit! {
                            String::from("a") => a,
                            String::from("b") => b,
                            String::from("c") => c,
                            String::from("d") => d
                        }
                    ), 
                    sdic.interpret(
                        &interit! {
                            String::from("a") => a,
                            String::from("b") => b,
                            String::from("c") => c,
                            String::from("d") => d
                        }
                    )
                );
            }
        }

        let prop = Proposition::AtLeast(
            AtLeast {
                value: 2,
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);

        let prop = Proposition::AtMost(
            AtMost {
                value: 2,
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);

        let prop = Proposition::And(
            And {
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);

        let prop = Proposition::Or(
            Or {
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);
        
        let prop = Proposition::Xor(
            Xor {
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);
        
        let prop = Proposition::XNor(
            XNor {
                variables: vec![
                    Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                    Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                    Rc::new( 
                        Proposition::Or(
                            Or {
                                variables: vec![
                                    Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                    Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                                ],
                            }
                        )
                    ),
                ],
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);
        
        let prop = Proposition::Implies(
            Implies {
                left: Rc::new( 
                    Proposition::And(
                        And {
                            variables: vec![
                                Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
                                Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
                            ],
                        }
                    )
                 ),
                right: Rc::new( 
                    Proposition::Or(
                        Or {
                            variables: vec![
                                Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
                                Rc::new( Proposition::Variable( Variable {id: String::from("d") } ) ),
                            ],
                        }
                    )
                ),
            }
        );
        let s = prop.to_sdic();
        evaluate(prop, s);
    }
}
