use crate::regex::*;
use crate::regex::Regex::*;
use self::Set::*;

enum Set<T> {
    Just(Vec<T>),
    Not(Vec<T>)
}

const kNID: u64 = 0;

struct Derivative<T, R> {
    m: Vec<(Set<T>, R)>,
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

fn combine<T, R>(v: Vec<Derivative<T, R>>, F: FnMut() -> R) -> Derivative<T, R> {
    if v.len() == 0 {
        Derivative {
            m: Vec::new(),
            rest: None
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


    let result = (Vec::new(), None);
    let mut regexes = Vec::new();
    go(v, &mut f, Set::Not(Vec::new()), &mut regexes, &mut result);
    Derivative {
        m: result.0,
        rest: result.1.unwrap(),
    }
}

impl<T: Clone> Differentiable<T> for Regex<T> {
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
                    Derivative { m: vec![(Just(vec![cs[0].clone()]),
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
                        let v = vec![x];
                        v.extend(rs[i..].iter().cloned());
                        Cat(v, kNID)
                    }));
                    if !rs[i].nullable() {
                        break;
                    }
                }
                combine(vs, |regexes| Alt(regexes.iter().map(|r| (*r).clone()).collect(), kNID))
            },
            Alt(ref rs, _) => {
                let mut vs = rs.iter().map(|r| r.derivative()).collect();
                combine(vs, |regexes| Alt(regexes.iter().map(|r| (*r).clone()).collect(), kNID))
            },
            Regex::Not(ref r, _) => {
                r.derivative().map(|r| Regex::Not(Box::new(r.clone()), kNID))
            }
        }
    }
}
