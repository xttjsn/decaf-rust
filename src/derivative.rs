use crate::regex::*;
use std::collections::HashMap;
use std::collections::BTreeSet;

type DerivativeClass<T> = BTreeSet<T>;

const kNID = 0;

struct Derivative<T, R> {
    m: HashMap<DerivativeClass<T>, R>,
    rest: R
}

trait Differentiable<T> {
    fn derivative(&self) -> Derivative<T, Self>
}

impl<T> Differentiable<T> for Regex<T> {
    fn derivative(&self) -> Derivative<T, Self> {
        match *self {
            Null => Derivative { m: HashMap::new(), rest: Null(kNID) }
            Empty => Derivative { m: HashMap::new(), rest: Null(kNID) }
            Chars => Derivative { }
        }
    }
}
