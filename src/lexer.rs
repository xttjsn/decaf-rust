// use crate::regex::Regex;
// use syn;
// use syn::parse::{Parse, ParseStream};
// use syn::{LitStr};

// struct Lexer {
// 	patterns: Vec<Regex<char>>,
// }

// impl Parse for Lexer {
// 	fn parse(input: ParseStream) -> syn::Result<Lexer> {
// 		Ok(Lexer {
// 			patterns: parse_patterns(input)?
// 		})
// 	}
// }

// struct Pattern {
// 	reliteral: LitStr,
// }

// fn parse_patterns(input: ParseStream) -> syn::Result<Vec<Regex<char>>> {
// 	// let mut patterns = vec![];
// //	while !input.is
//     Ok(vec![])
// }

// pub fn lexer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
// 	let Lexer {
// 		patterns
// 	} = parse_macro_input!(input as Lexer);
// 	quote!(
// 		fn lexer_is_done() { print!("done"); }
// 	).into()
// }

use std::char;
use std::collections::{BTreeSet, VecDeque};

use crate::regex::Regex;
use crate::dfa::Dfa;

use proc_macro2::{Span, TokenStream};
use syn;
use syn::parse::{Parse, ParseStream};
use syn::{token, Expr, Ident, Lifetime, LitStr, Type, Visibility};

fn dfa_fn<T>(
	dfa: &Dfa<char, T>,
	state_enum: Ident,
	state_paths: &[TokenStream],
	fn_name: Ident,
) -> TokenStream {
	let mut arms = vec![];
}

fn first_nullable<T>(vec: &[Regex<T>]) -> Option<usize> {
	vec.iter().position(Regex::nullable)
}

fn dfa_make_names<V>(dfa: &Dfa<char, V>) -> Vec<String> {

}

struct Rule {
	pattern: LitStr,
	expr: Expr,
}

fn parse_rules(input: ParseStream) -> syn::Result<Vec<Rule>> {
	let mut rules = vec![];
    while !input.is_empty() {
        // FIXME: Make some nicer error messages.
        let pattern = input.parse()?;
        input.parse::<Token![=>]>()?;
        // Like in a `match` expression, braced block doesn't require a comma before the next rule.
        let optional_comma = input.peek(token::Brace);
        let expr = input.parse()?;
        rules.push(Rule { pattern, expr });
        match input.parse::<Token![,]>() {
            Ok(_) => {}
            Err(e) => {
                if !input.is_empty() && !optional_comma {
                    return Err(e);
                }
            }
        }
    }
    Ok(rules)
}

struct Lexer {
	vis: Visibility,
	name: Ident,
	input: Ident,
	lifetime: Option<Lifetime>,
	return_type: Type,
	rules: Vec<Rule>,
}

impl Parse for Lexer {
	fn parse(input: ParseStream) -> syn::Result<Lexer> {
        let lifetime;
        Ok(Lexer {
            vis: input.parse()?,
            name: {
                input.parse::<Token![fn]>()?;
                input.parse()?
            },
            input: {
                let inner;
                parenthesized!(inner in input);
                let lexer_input = inner.parse()?;
                if !inner.is_empty() {
                    inner.parse::<Token![:]>()?;
                    lifetime = Some(inner.parse()?);
                    if !inner.is_empty() {
                        return Err(inner.error("unexpected token after input lifetime"));
                    }
                } else {
                    lifetime = None;
                }
                lexer_input
            },
            lifetime,
            return_type: {
                input.parse::<Token![->]>()?;
                let t = input.parse()?;
                input.parse::<Token![;]>()?;
                t
            },
            rules: parse_rules(input)?,
        })
    }
}

pub fn lexer(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Lexer {
        vis,
        name,
        input,
        lifetime,
        return_type,
        rules,
    } = parse_macro_input!(input as Lexer);
	let (re_vec, actions): (Vec<Regex<_>>, Vec<Expr>) = rules
        .into_iter()
        .map(|Rule { pattern, expr }| {
            let re = match pattern.value().parse() {
                Ok(r) => r,
                Err(e) => {
                    pattern
                        .span()
                        .unstable()
                        .error(format!("invalid regular expression: {}", e))
                        .emit();
                    Regex::Null // dummy
                }
            };
            if re.nullable() {
                pattern
                    .span()
                    .unstable()
                    .error("token must not match the empty string")
                    .emit();
            }
            (re, expr)
        })
        .unzip();

	let (dfa, _) = Dfa::from_derivatives(vec![re_vec]);
	let dfa = dfa.map(|vec| first_nullable(&vec)).minimize().map(|&x| x);
	let error_state_ix = dfa.states.iter().enumerate().position(|(ix, state)| {
		state.value.is_none() && state.by_char.is_empty() && state.default as usize = ix
	});
	if error_state_ix.is_none() {
		Span::call_site()
			.unstable()
			.warning("this DFA has no error state; it will always scan the entire input")
			.emit();
	}

	let mut names: Vec<Ident> = dfa_make_names(&dfa)
		.into_iter()
		.map(|n| Ident::new(&n, Span::call_site()))
		.collect();

	if let Some(ix) == error_state_ix {
		names[ix] = Ident::new("Error", Span::call_site());
	}

	let state_paths: Vec<TokenStream> = names.iter().map(|name| quote!(State::#name)).collect();

	let initial_state = state_paths[0].clone();
	let error_state = error_state_ix.map(|ix| state_paths[ix].clone());

	let transition_fn = dfa_fn(
		&dfa,
		Ident::new("State", Span::call_site()),
		&state_paths,
		Ident::new("transition", Span::call_site())
	);

	let accepting_fn = {
		let arms = dfa
			.states
			.iter()
			.map(|state| state.value)
			.zip(&state_paths)
			.filter_map(|(maybe_act, state)| {
				maybe_act.map(|act| {
					let act = act as u32;
					quote!(#state => Some(#act))
				})
			});
		quote!(fn accepting(state: State) -> Option<u32> {
			match state {
				#(#arms,)*
				_ => None
			}
		})
	}

}
