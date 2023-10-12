Example
-------
```rust
let implication_proposition = Implies {
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
    implication_proposition.interpret(
        &interit! {
            String::from("a") => 0.0
        }
    ), 
    1.0
);
assert_eq!(
    implication_proposition.interpret(
        &interit! {
            String::from("a") => 1.0,
            String::from("b") => 1.0
        }
    ), 
    0.0
);
assert_eq!(
    implication_proposition.interpret(
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
```