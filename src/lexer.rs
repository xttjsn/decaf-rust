use crate::regex::Regex;
use syn;
use syn::parse::{Parse, ParseStream};
use syn::{LitStr};

struct Lexer {
	patterns: Vec<Regex<char>>,
}

impl Parse for Lexer {
	fn parse(input: ParseStream) -> syn::Result<Lexer> {
		Ok(Lexer {
			patterns: parse_patterns(input)?
		})
	}
}

struct Pattern {
	reliteral: LitStr,
}

fn parse_patterns(input: ParseStream) -> syn::Result<Vec<Regex<char>>> {
	// let mut patterns = vec![];
//	while !input.is
    Ok(vec![])
}

pub fn lexer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let Lexer {
		patterns
	} = parse_macro_input!(input as Lexer);
	quote!(
		fn lexer_is_done() { print!("done"); }
	).into()
}
