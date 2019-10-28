#![feature(proc_macro_diagnostic)]
#![feature(let_chains)]

extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate quote;


#[macro_use]
extern crate syn;

extern crate dot;

mod dfa;
use dfa::Normalize;
mod regex;
mod lexer;
mod viz;
mod derivative;

#[proc_macro]
pub fn regex_parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut r: regex::Regex<char> = parse_macro_input!(input as regex::Regex<char>);
	r = r.normalize();
	use std::fs::File;
	let mut f = File::create("regex.dot").unwrap();
	viz::render_graph(&r, &mut f);
	quote!().into()
}

