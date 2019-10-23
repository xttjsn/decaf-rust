use decaf::regex_parse;

regex_parse! {
	"(prefix1)(prefix2)[a-c0-9]*(ab|c)|q+"
}

fn main() {
}
