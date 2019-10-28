use std::iter::Peekable;
use std::cmp::Ordering;
use crate::regex::*;
use crate::regex::Regex::*;
use self::Set::*;

const kNID: u64 = 0;

struct Derivative<T, R> {
    m: Vec<(Vec<T>, R)>,
    rest: R
}

impl<T, R> Derivative<T, R> {
    fn map<F: FnMut(R) -> R>(self, f: F) -> Self {
        Derivative {
            m: self.m.into_iter().map(|(cs, r)| (cs, f(r))).collect(),
            rest: self.rest
        }
    }
}

trait Differentiable<T>: Sized {
    fn derivative(&self) -> Derivative<T, Self>;
}

struct Union<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<It1>,
    b : Peekable<It2>,
}
fn union<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Union<T, It1, It2> {
    Union { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Union<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        match match self.a.peek() {
            Some(av) => match self.b.peek() {
                Some(bv) => av.cmp(bv),
                None => Ordering::Less,
            },
            None => Ordering::Greater,
        } {
            Ordering::Less => {
                self.a.next()
            }
            Ordering::Greater => {
                self.b.next()
            }
            Ordering::Equal => {
                self.a.next();
                self.b.next()
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (a1, a2) = self.a.size_hint();
        let (b1, b2) = self.b.size_hint();
        (a1 + b1,
         if let (Some(a2), Some(b2)) = (a2, b2) {
             Some(a2 + b2)
         } else {
             None
         })
    }
}

struct Inter<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<It1>,
    b : Peekable<It2>,
}
fn inter<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Inter<T, It1, It2> {
    Inter { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Inter<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            match if let (Some(av), Some(bv)) = (self.a.peek(), self.b.peek()) {
                av.cmp(bv)
            } else {
                return None
            } {
                Ordering::Less => {
                    self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    return self.b.next();
                }
            }
        }
    }
}

struct Subtract<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> {
    a : Peekable<It1>,
    b : Peekable<It2>,
}
fn subtract<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>>(a: It1, b: It2) -> Subtract<T, It1, It2> {
    Subtract { a: a.peekable(), b: b.peekable() }
}
impl<T: Ord, It1: Iterator<Item=T>, It2: Iterator<Item=T>> Iterator for Subtract<T, It1, It2> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        loop {
            match match (self.a.peek(), self.b.peek()) {
                (Some(av), Some(bv)) => av.cmp(bv),
                (_, None) => Ordering::Less,
                (None, _) => return None,
            } {
                Ordering::Less => {
                    return self.a.next();
                }
                Ordering::Greater => {
                    self.b.next();
                }
                Ordering::Equal => {
                    self.a.next();
                    self.b.next();
                }
            }
        }
    }
}

enum Set<T> {
    Just(Vec<T>),
    Not(Vec<T>)
}

impl<T: Ord + Clone> Set<T> {
    fn inter(&self, b: &[T]) -> Set<T> {
        use self::Set::*;
        match *self {
            Just(ref a) => {
                Just(inter(a.iter().cloned(), b.iter().cloned()).collect())
            }
            Not(ref a) => {
                Just(subtract(b.iter().cloned(), a.iter().cloned()).collect())
            }
        }
    }
    fn subtract(&self, b: &[T]) -> Set<T> {
        use self::Set::*;
        match *self {
            Just(ref a) => {
                Just(subtract(a.iter().cloned(), b.iter().cloned()).collect())
            }
            Not(ref a) => {
                Not(union(a.iter().cloned(), b.iter().cloned()).collect())
            }
        }
    }
}

fn combine<T: Ord + Clone, R, S, F: FnMut(&[&R]) -> S>(v: &[Derivative<T, R>], mut f: F) -> Derivative<T, S> {
    fn go<'a, T: Ord + Clone, R, S, F: FnMut(&[&R]) -> S>(
        v: &'a [Derivative<T, R>],
        f: &mut F,
        what: Set<T>,
        current: &mut Vec<&'a R>,
        out: &mut (Vec<(Vec<T>, S)>, Option<S>)
    ) {
        if let Set::Just(ref v) = what {
            if v.len() == 0 {
                // prune
                return;
            }
        }
        if v.len() == 0 {
            let reg = f(&current);
            match what {
                Set::Just(c) => out.0.push((c, reg)),
                Set::Not(_) => {
                    assert!(out.1.is_none());
                    out.1 = Some(reg);
                }
            }
            return;
        }
        let (first, rest) = v.split_at(1);
        let first = &first[0];
        let mut all_chars = Vec::new();
        for &(ref chars, ref reg) in first.m.iter() {
            all_chars = union(all_chars.into_iter(), chars.iter().cloned()).collect();
            let inter = what.inter(&chars);
            current.push(reg);
            go(rest, f, inter, current, out);
            current.pop();
        }
        let inter = what.subtract(&all_chars);
        current.push(&first.rest);
        go(rest, f, inter, current, out);
        current.pop();
    }
    let mut result = (Vec::new(), None);
    let mut regexes = Vec::new();
    go(v, &mut f, Set::Not(Vec::new()), &mut regexes, &mut result);
    Derivative {
        m: result.0,
        rest: result.1.unwrap(),
    }
}

impl<T: Ord + Clone> Differentiable<T> for Regex<T> {
    fn derivative(&self) -> Derivative<T, Self> {
        match *self {
            Null(_) => Derivative { m: Vec::new(), rest: Null(kNID) },
            Empty(_) => Derivative { m: Vec::new(), rest: Null(kNID) },
            Chars(ref cs, _) => {
                if cs.len() == 0 {
                    Derivative { m: Vec::new(), rest: Null(kNID) }
                }
                else if cs.len() == 1 {
                    Derivative { m: Vec::new(), rest: Empty(kNID) }
                } else {
                    Derivative { m: vec![(vec![cs[0].clone()],
                                          Chars(cs[1..].iter().map(|c| c.clone()).collect(), kNID))],
                                 rest: Null(kNID) }
                }
            }
            Kleene(ref r, _) => {
                r.derivative().map(|x| Cat(vec![x, Kleene(r.clone(), kNID)], kNID))
            }
            Cat(ref rs, _) => {
                let mut vs = Vec::new();
                for i in 0..rs.len() {
                    vs.push(rs[i].derivative().map(|x| {
                        let mut v = vec![x];
                        v.extend(rs[i..].iter().cloned());
                        Cat(v, kNID)
                    }));
                    if !rs[i].nullable() {
                        break;
                    }
                }
                combine(&vs, |regexes| Alt(regexes.iter().map(|r| (*r).clone()).collect(), kNID))
            },
            Alt(ref rs, _) => {
				let mut vs = Vec::new();
				vs.extend(rs.iter().map(Differentiable::derivative));
                combine(&vs, |regexes| Alt(regexes.iter().map(|r| (*r).clone()).collect(), kNID))
            },
            Regex::Not(ref r, _) => {
                r.derivative().map(|r| Regex::Not(Box::new(r), kNID))
            }
        }
    }
}

// Derivatives of "regular vectors", as described in "Regular-expression derivatives reexamined" by Owens et al.
impl<T: Ord + Clone, R: Differentiable<T> + Clone> Differentiable<T> for Vec<R> {
    fn derivative(&self) -> Derivative<T, Vec<R>> {
        let v: Vec<Derivative<T, R>> = self.iter().map(Differentiable::derivative).collect();
        combine(&*v, |xs: &[&R]| xs.iter().map(|&x| x.clone()).collect())
    }
}
