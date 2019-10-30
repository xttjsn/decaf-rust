#![feature(proc_macro_hygiene)]
mod lnp;
use std::io::Read;

fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    let lexer = lnp::lexer::Lexer::new(&s).inspect(|tok| eprintln!("tok: {:?}", tok));
    println!("lexer finished!");
    let program = lnp::parser::parse(lexer).unwrap();
}
