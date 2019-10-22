use self::Regex::*;
use self::ParseError::*;
use std::iter::Peekable;
use std::{char, str, fmt};
use syn::parse::{Parse, ParseStream};
use syn::{LitStr};
use syn;
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
#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Clone)]
pub enum Regex<T> {
    Null(u64), 									// Match nothing
	Empty(u64),									// Match empty string
    Chars(Vec<T>, u64),							// Match the character sequence
    Kleene(Box<Regex<T>>, u64),					// Match zero or more regexes
    Cat(Vec<Regex<T>>, u64),					// Match the concatenation of the regexes
    Not(Box<Regex<T>>, u64),					// Match anything other than the regexes
    Alt(Vec<Regex<T>>, u64),					// Match any regexes listed
}

pub enum ParseError {
    UnexpectedCharError(&'static str, char),
    UnexpectedEOFError(&'static str),
	UnexpectedRangeError(&'static str, char, char),
	NotImplementedError,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UnexpectedEOFError(s) => write!(f, "{}", s),
            UnexpectedCharError(s, c) => write!(f, "{}: `{}`", s, c),
            UnexpectedRangeError(s, c, d) => write!(f, "{}: `{}-{}`", s, c, d),
			NotImplementedError => write!(f, "feature not implemented")
        }
    }
}

type Res<T> = Result<T, ParseError>;

struct Parser<I: Iterator<Item=char>> {
	it: Peekable<I>,
    rid: u64
}

impl<I: Iterator<Item=char>> Parser<I> {
	fn next_id(&mut self) -> u64 {
		self.rid += 1;
		self.rid
	}

	fn cat(&mut self) -> Res<Regex<char>> {
		let mut rs = vec![self.alt()?];
		loop {
			match self.it.peek() {
				Some(&c) if Parser::<I>::cat_first(c) => rs.push(self.alt()?),
				_ => break,
			}
		}
		return Ok(Cat(rs, self.next_id()))
	}

	fn alt(&mut self) -> Res<Regex<char>> {
		let mut rs = vec![self.not()?];
		loop {
			match self.it.peek() {
				Some(&'|') => { self.it.next(); rs.push(self.not()?) }
				_ => break,
			}

		}
		return Ok(Alt(rs, self.next_id()));
	}

	fn not(&mut self) -> Res<Regex<char>> {
		match self.it.peek() {
			Some(&'~') => { self.it.next(); Ok(Not(Box::new(self.not()?), self.next_id())) }
			_ => self.kleene()
		}
	}

	fn kleene(&mut self) -> Res<Regex<char>> {
		let mut rs = self.atom()?;
		loop {
			match self.it.peek() {
				Some(&'+') => {
					self.it.next();
					rs = Cat(vec![rs.clone(), Kleene(Box::new(rs), self.next_id())], self.next_id())
				}
				Some(&'*') => {
					self.it.next();
					rs = Kleene(Box::new(rs), self.next_id())
				}
				_ => break,
			}
		}
		Ok(rs)
	}

	fn cat_first(c : char) -> bool {
		c != ']' && c != '|' && c != '+'
			&& c != '*' && c != ')'
	}

	fn char_first(c : char) -> bool {
		c != '[' && c != ']' && c != '|'
			&& c != '+' && c != '*'
			&& c != '(' && c != ')'
			&& c != '~' && c != '.'
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
				let tmp = Ok(Not(Box::new(Null(self.next_id())), self.next_id()));
				return tmp;
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
					rs.push(self.char()?);
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
			_ => Ok(Chars(rs, self.next_id()))
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
				let r = self.cat()?;
				match self.it.peek() {
					Some(&')') => { self.it.next(); Ok(r) }
					Some(&c) => Err(UnexpectedCharError("bad character for group end", c)),
					_ => Err(UnexpectedEOFError("unmatched '('"))
				}
			}
			Some(&c) => Err(UnexpectedCharError("bad character for group start", c)),
			None =>	Err(UnexpectedEOFError("pre-terminated "))
		}
	}

	fn char_class(&mut self) -> Res<Regex<char>> {
		let rs;
		match self.it.peek() {
			Some(&'[') => {
				self.it.next();

				match self.it.peek() {
					Some(&'^') => {
						self.it.next();
						rs = Not(Box::new(self.range()?), self.next_id());
					}
					Some(&_) => {
						rs = self.range()?;
					}
					None => {
						return Err(UnexpectedEOFError("unterminated range"));
					}
				}

				match self.it.peek() {
					Some(&']') => { self.it.next(); Ok(rs) }
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
								v.push(Chars(vec![x], self.next_id()));
							} else {
								return Err(UnexpectedRangeError("range contains bad code points", c, d));
							}
						}
					} else {
						v.push(Chars(vec![c], self.next_id()));
					}
				}
				_ => break,
			}
		}
		Ok(Alt(v, self.next_id()))
	}

	fn parse(it : I) -> Res<Regex<char>> {
		let mut parser = Parser { it: it.peekable(), rid: 0 };
		let r = parser.cat()?;
		if let Some(c) = parser.it.next() {
			Err(UnexpectedCharError("bad character in regex", c))
		} else {
			Ok(r)
		}
	}
}

impl Parse for Regex<char> {
	fn parse(input: ParseStream) -> syn::Result<Regex<char>> {
		let regex_src : LitStr = input.parse()?;
		match regex_src.value().parse() {
			Ok(r) => {
				Ok(r)
			}
			Err(er) => {
				regex_src
					.span()
					.unstable()
					.error(format!("invalid regular expression: {}", er))
					.emit();
				Ok(Null(0))
			}
		}
	}
}

impl str::FromStr for Regex<char> {
    type Err = ParseError;
    /// Parse a string as a regular expression.
    fn from_str(s: &str) -> Result<Regex<char>, ParseError> {
        Parser::parse(s.chars())
    }
}

