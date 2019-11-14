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
	fn test_visit_class_single_field() {
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Main {
	public int x;
	public int foo() {}
	protected int bar() {}
	private int foobar() {}
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

				assert_eq!(c.pub_methods.borrow().len(), 1);
				assert_eq!(c.prot_methods.borrow().len(), 1);
				assert_eq!(c.priv_methods.borrow().len(), 1);
			}
		}
	}
}