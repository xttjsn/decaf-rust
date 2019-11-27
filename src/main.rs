pub mod lnp;
use std::io::Read;

fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
	lnp::parse_decaf(&s);
}
