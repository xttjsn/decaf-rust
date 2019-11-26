#[cfg(test)]
mod tests {
	use crate::treebuild::DecafTreeBuilder;
	use crate::treebuild::Visitor;
	use crate::decaf::TypeBase::*;
	use crate::lnp;

	#[test]
	fn test_visit_program() {
	}

	#[test]
	fn test_visit_field() {
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_visit_class() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Main {
	public int x;
	private char y;
	public int foo() {}
	protected int bar() {}
	private int foobar() {}
	public static int sfoo() {}
	protected static int sbar() {}
	private static int sfoobar() {}
}"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 1);
		builder.visit_program(&program);

		match builder.class_map.get(&"Main".to_string()) {
			None => assert_eq!(1, 0),
			Some(c) => {
				assert_eq!(c.name, "Main");
				assert_eq!(c.sup.borrow().as_ref().unwrap().name, "Object");
				assert_eq!(c.pub_fields.borrow().len(), 1);
				assert_eq!(c.pub_fields.borrow().last().unwrap().name, "x");
				match c.pub_fields.borrow().last().unwrap().ty.borrow().base {
					IntTy => {},
					_ => assert_eq!(1, 0),
				};

				assert_eq!(c.priv_fields.borrow().len(), 1);
				assert_eq!(c.priv_fields.borrow().last().unwrap().name, "y");

				assert_eq!(c.pub_methods.borrow().len(), 1);
				assert_eq!(c.pub_methods.borrow().last().unwrap().name, "foo");
				assert_eq!(c.prot_methods.borrow().len(), 1);
				assert_eq!(c.prot_methods.borrow().last().unwrap().name, "bar");
				assert_eq!(c.priv_methods.borrow().len(), 1);
				assert_eq!(c.priv_methods.borrow().last().unwrap().name, "foobar");
				assert_eq!(c.pub_static_methods.borrow().len(), 1);
				assert_eq!(c.pub_static_methods.borrow().last().unwrap().name, "sfoo");
				assert_eq!(c.prot_static_methods.borrow().len(), 1);
				assert_eq!(c.prot_static_methods.borrow().last().unwrap().name, "sbar");
				assert_eq!(c.priv_static_methods.borrow().len(), 1);
				assert_eq!(c.priv_static_methods.borrow().last().unwrap().name, "sfoobar");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_normal_declaration() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Foo {
	public int x;
}
class Bar extends Foo {
}"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 2);
		builder.visit_program(&program);

		match builder.class_map.get(&"Bar".to_string()) {
			None => assert_eq!(1, 0),
			Some(bar) => {
				assert_eq!(bar.name, "Bar");
				assert_eq!(bar.sup.borrow().as_ref().unwrap().name, "Foo");
				assert_eq!(bar.pub_fields.borrow().len(), 1);
				assert_eq!(bar.pub_fields.borrow().last().unwrap().name, "x");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_forward_declaration() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Bar extends Foo {
}
class Foo {
	public int x;
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 2);
		builder.visit_program(&program);

		match builder.class_map.get(&"Bar".to_string()) {
			None => assert_eq!(1, 0),
			Some(bar) => {
				assert_eq!(bar.name, "Bar");
				assert_eq!(bar.sup.borrow().as_ref().unwrap().name, "Foo");
				assert_eq!(bar.pub_fields.borrow().len(), 1);
				assert_eq!(bar.pub_fields.borrow().last().unwrap().name, "x");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_three_layer_declaration() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Woo extends Bar {
	public int z;
}
class Bar extends Foo {
	public int y;
}
class Foo {
	public int x;
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 3);
		builder.visit_program(&program);

		match builder.class_map.get(&"Woo".to_string()) {
			None => assert_eq!(1, 0),
			Some(woo) => {
				assert_eq!(woo.name, "Woo");
				assert_eq!(woo.sup.borrow().as_ref().unwrap().name, "Bar");
				println!("fileds: {}, {}", woo.pub_fields.borrow()[0].name, woo.pub_fields.borrow()[1].name);
				assert_eq!(woo.pub_fields.borrow().len(), 3);
				assert_eq!(woo.pub_fields.borrow()[0].name, "x");
				assert_eq!(woo.pub_fields.borrow()[1].name, "y");
				assert_eq!(woo.pub_fields.borrow()[2].name, "z");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_mixed_order_declaration() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Bar extends Foo {
	public int y;
}
class Woo extends Bar {
	public int z;
}
class Foo {
	public int x;
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 3);
		builder.visit_program(&program);

		match builder.class_map.get(&"Woo".to_string()) {
			None => assert_eq!(1, 0),
			Some(woo) => {
				assert_eq!(woo.name, "Woo");
				assert_eq!(woo.sup.borrow().as_ref().unwrap().name, "Bar");
				assert_eq!(woo.pub_fields.borrow().len(), 3);
				assert_eq!(woo.pub_fields.borrow()[0].name, "x");
				assert_eq!(woo.pub_fields.borrow()[1].name, "y");
				assert_eq!(woo.pub_fields.borrow()[2].name, "z");
			}
		}
	}


	#[test]
	#[allow(unused_must_use)]
	fn test_methods_simple_inheritance() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Bar extends Foo {
	public int methodB() {}
}
class Foo {
	public int methodA() {}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 2);
		builder.visit_program(&program);

		match builder.class_map.get(&"Bar".to_string()) {
			None => assert_eq!(1, 0),
			Some(woo) => {
				assert_eq!(woo.name, "Bar");
				assert_eq!(woo.sup.borrow().as_ref().unwrap().name, "Foo");
				assert_eq!(woo.pub_methods.borrow().len(), 2);
				assert_eq!(woo.pub_methods.borrow()[0].name, "methodA");
				assert_eq!(woo.pub_methods.borrow()[1].name, "methodB");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_methods_overwrite() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Bar extends Foo {
	public int methodA() {}
}
class Foo {
	public int methodA() {}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 2);
		builder.visit_program(&program);

		match builder.class_map.get(&"Bar".to_string()) {
			None => assert_eq!(1, 0),
			Some(woo) => {
				assert_eq!(woo.name, "Bar");
				assert_eq!(woo.sup.borrow().as_ref().unwrap().name, "Foo");
				assert_eq!(woo.pub_methods.borrow().len(), 2);
				assert_eq!(woo.pub_methods.borrow()[0].name, "methodA");
				assert_eq!(woo.pub_methods.borrow()[1].name, "methodA");
			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_methods_visibility() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Bar extends Foo {
	public int methodA() {}
	protected int methodB() {}
	private int methodC() {}
}
class Foo {
	public int methodA() {}
	protected int methodB() {}
	private int methodC() {}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 2);
		builder.visit_program(&program);

		match builder.class_map.get(&"Bar".to_string()) {
			None => assert_eq!(1, 0),
			Some(woo) => {
				assert_eq!(woo.name, "Bar");
				assert_eq!(woo.sup.borrow().as_ref().unwrap().name, "Foo");
				assert_eq!(woo.pub_methods.borrow().len(), 2);
				assert_eq!(woo.pub_methods.borrow()[0].name, "methodA");
				assert_eq!(woo.pub_methods.borrow()[1].name, "methodA");
				assert_eq!(woo.prot_methods.borrow().len(), 2);
				assert_eq!(woo.prot_methods.borrow()[0].name, "methodB");
				assert_eq!(woo.prot_methods.borrow()[1].name, "methodB");
				assert_eq!(woo.priv_methods.borrow().len(), 1);
				assert_eq!(woo.priv_methods.borrow()[0].name, "methodC");
			}
		}
	}


	// TODO:
	// test consecutive vardeclarator
}
