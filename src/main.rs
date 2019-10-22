use decaf::regex_parse;

regex_parse! {
	"prefix[a-c0-9]*(ab|c)|q+"
}

fn main() {
}
