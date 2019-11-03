// Put then in the symbol table before compilation

/// The built-in class Object
struct Object {

}

/// Built-in Object implementation
impl Object {
	fn ctor() {}
}

/// The built-in class String
struct String {
	sup: Weak<Object>,
}

impl String {
	fn ctor() {}
}

/// The built-in class IO
struct IO {
	sup: Weak<Object>,
}

impl IO {
	fn ctor() {}
	fn impl_putchar() {}
	fn impl_putint() {}
	fn impl_putstring() {}
	fn peek() {}
	fn getchar() {}
	fn getint() {}
	fn getline() {}
}
