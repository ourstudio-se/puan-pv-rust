
# What is dis?
A simple proposition-to-value, or just proposition, package. Works perfectly if you'd like to model a logical system resulting in values depending on later evaluated interpretations of the system. Each proposition is recursively definined, meaning an element can be a proposition itself.

--------

# Proposition system
The first part of this package is a logical proposition system with predefinied logic structs. Each definied proposition can be evaluated (or interpreted) and negated.
## Examples
### AtLeast proposition

Defining an at-least-2 proposition as following

```rust
use std::rc::Rc;

let at_least = AtLeast {
    variables: vec![
        Rc::new( Proposition::Variable( Variable {id: String::from("a") } ) ),
        Rc::new( Proposition::Variable( Variable {id: String::from("b") } ) ),
        Rc::new( Proposition::Variable( Variable {id: String::from("c") } ) ),
    ],
    at_least: 2,
    id: None,
};

// Trying out different interpretations
// Only "a" should result in 0 or false
assert_eq!(
    implication_proposition.interpret(
        &interit! {
            String::from("a") => 1.0
        }
    ), 
    0.0
);

// "a" and "b" should result in 1 or true
assert_eq!(
    implication_proposition.interpret(
        &interit! {
            String::from("a") => 1.0
        }
    ), 
    1.0
);

```

Defining a "if a and b then x, y and z" proposition as following

```rust
### Implication proposition
```rust

use std::rc::Rc;

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