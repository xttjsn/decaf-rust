#![feature(proc_macro_diagnostic)]

extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate quote;


#[macro_use]
extern crate syn;

extern crate dot;

mod regex;
mod lexer;
mod viz;

#[proc_macro]
pub fn regex_parse(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let r: regex::Regex<char> = parse_macro_input!(input as regex::Regex<char>);
	println!("parse_macro_input! returned");
	use std::fs::File;
	let mut f = File::create("regex.dot").unwrap();
	println!("file created");
	viz::render_graph(&r, &mut f);
	quote!().into()
}

