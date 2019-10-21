use self::Regex::*;
use self::ParseError::*;
use std::iter::Peekable;
use std::char;
/*

Start		-> Regex

Regex		->	Empty
			|	Chars
			|	Group
			|	CharClass
			|	Kleene
			|	Not
			|	Alt
			|	Cat

Empty		->	''

Char		->	<a single ascii symbol>
			| 	'\' 't'
			| 	'\' 'n'
			| 	'\' '\'

Chars		->	Char
			|	Char Chars

Group		-> '(' Regex ')'

CharClass	->	'[' RangeList ']'
			|	'[' '^' RangeList ']'

Range		->	Chars
			|	Chars '-' Char

RangeList	->	Range
			|	RangeList Range

Atom		->	Chars
			|	Group
			|	CharClass
			|	'.'

Kleene		->	Atom
			|	Kleene '+'
			|	Kleene '*'

Not			-> 	Kleene
			|	'~' Not

Alt			->	Not
			|	Not '|' Alt

Cat			->	Alt
			|	Alt Cat
 */
#[derive(Clone)]
pub enum Regex<T> {
    Null, 								// Match Nothing
    Empty, 								// Match empty string ""
    Chars(Vec<T>),						// Match the character sequence
    Kleene(Box<Regex<T>>),				// Match zero or more regexes
    Cat(Vec<Regex<T>>),					// Match the concatenation of the regexes
    Not(Box<Regex<T>>),					// Match anything other than the regexes
    Alt(Vec<Regex<T>>),					// Match any regexes listed
}

pub enum ParseError {
    UnexpectedCharError(&'static str, char),
    UnexpectedEOFError(&'static str),
	UnexpectedRangeError(&'static str, char, char),
	NotImplementedError,
}

type Res<T> = Result<T, ParseError>;

struct Parser<I: Iterator<Item=char>> {
	it: Peekable<I>,
}

impl<I: Iterator<Item=char>> Parser<I> {
	fn cat(&mut self) -> Res<Regex<char>> {
		let mut rs = vec![self.alt()?];
		// DONE: on a second thought, I probably wouldn't need the loop here
		// DONE: try removing the loop layer later
		// Ok, so the loop here is to avoid recursion
		loop {
			match self.it.peek() {
				Some(_c) => rs.push(self.alt()?),
				_ => break,
			}
		}
		return Ok(Cat(rs))
	}

	fn alt(&mut self) -> Res<Regex<char>> {
		let mut rs = vec![self.not()?];
		loop {
			match self.it.peek() {
				Some(&'|') => { self.it.next(); rs.push(self.not()?) }
				_ => break,
			}
		}
		return Ok(Alt(rs));
	}

	fn not(&mut self) -> Res<Regex<char>> {
		match self.it.peek() {
			Some(&'~') => { self.it.next(); Ok(Not(Box::new(self.not()?))) }
			_ => self.kleene()
		}
	}

	fn kleene(&mut self) -> Res<Regex<char>> {
		let mut rs = self.atom()?;
		loop {
			match self.it.peek() {
				Some(&'+') => {
					self.it.next();
					rs = Cat(vec![rs.clone(), Kleene(Box::new(rs))])
				}
				Some(&'*') => {
					self.it.next();
					rs = Kleene(Box::new(rs))
				}
				_ => break,
			}
		}
		Ok(rs)
	}

	fn char_first(c : char) -> bool {
		c != '[' && c != ']' && c != '|'
			&& c != '+' && c != '*'
			&& c != '(' && c != ')'
			&& c != '~'
	}

	fn char_group(c : char) -> bool {
		c != ']'
	}

	fn atom(&mut self) -> Res<Regex<char>> {
		match self.it.peek() {
			Some(&'(') => self.group(),
			Some(&'[') => self.char_class(),
			Some(&'.') => {
				self.it.next();
				Ok(Not(Box::new(Null)))
			}
			Some(&c) if	Parser::<I>::char_first(c) => self.chars(),
			Some(&c) => Err(UnexpectedCharError("bad character for atom", c)),
			None => Err(UnexpectedEOFError("atom not nullable")),
		}
	}

	fn chars(&mut self) -> Res<Regex<char>> {
		let mut rs = vec![];
		loop {
			match self.it.peek() {
				Some(&c) if	Parser::<I>::char_first(c) => {
					self.it.next();
					rs.push(c);
				}
				_ => break,
			}
		}
		match rs.len() {
			0 => {
				match self.it.peek() {
					Some(&c) => Err(UnexpectedCharError("chars not nullable", c)),
					None => Err(UnexpectedEOFError("chars not nullable"))
				}
			}
			_ => Ok(Chars(rs))
		}
	}

	fn char(&mut self) -> Res<char> {
		match self.it.next() {
			Some('\\') => {
				match self.it.next() {
					Some('r') => Ok('\r'),
                    Some('n') => Ok('\n'),
                    Some('t') => Ok('\t'),
					Some(c) => Ok(c),
					None => Err(UnexpectedEOFError("unfollowed '\\'"))
				}
			}
			Some(c) => Ok(c),
			None => panic!("char not nullable")
		}
	}

	fn group(&mut self) -> Res<Regex<char>> {
		match self.it.peek() {
			Some(&'(') => {
				self.it.next();
				let mut r = self.cat()?;
				match self.it.peek() {
					Some(&')') => Ok(r),
					Some(&c) => Err(UnexpectedCharError("bad character for group end", c)),
					_ => Err(UnexpectedEOFError("unmatched '('"))
				}
			}
			Some(&c) => Err(UnexpectedCharError("bad character for group start", c)),
			None =>	Err(UnexpectedEOFError("pre-terminated "))
		}
	}

	fn char_class(&mut self) -> Res<Regex<char>> {
		let mut rs;
		match self.it.peek() {
			Some(&'[') => {
				self.it.next();

				match self.it.peek() {
					Some(&'^') => {
						self.it.next();
						rs = Not(Box::new(self.range()?));
					}
					Some(&c) => {
						rs = self.range()?;
					}
					None => {
						return Err(UnexpectedEOFError("unterminated range"));
					}
				}

				match self.it.peek() {
					Some(&']') => Ok(rs),
					Some(&c) => Err(UnexpectedCharError("bad character for character class", c)),
					None => Err(UnexpectedEOFError("unmatched ']'"))
				}
			}
			Some(&c) => Err(UnexpectedCharError("bad character for character class start", c)),
			None => Err(UnexpectedEOFError("pre-terminated character class"))
		}
	}

	fn range(&mut self) -> Res<Regex<char>> {
		let mut v = Vec::new();
		loop {
			match self.it.peek() {
				Some(&c) if Parser::<I>::char_group(c) => {
					let c = self.char()?;
					if let Some(&'-') = self.it.peek() {
						self.it.next();
						if let None = self.it.peek() {
							return Err(UnexpectedEOFError("unterminated range"));
						}
						let d = self.char()?;
						for x in (c as u64)..(d as u64 + 1) {
							if let Some(x) = char::from_u32(x as u32) {
								v.push(Chars(vec![x]));
							} else {
								return Err(UnexpectedRangeError("range contains bad code points", c, d));
							}
						}
					} else {
						v.push(Chars(vec![c]));
					}
				}
				_ => break,
			}
		}
		Ok(Alt(v))
	}
}

