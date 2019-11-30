#[cfg(test)]
mod tests {
	use crate::decaf::Expr::{Literal, Variable};
	use crate::decaf::TypeBase::*;
	use crate::lnp;
	use crate::treebuild::DecafTreeBuilder;
	use crate::treebuild::Visitor;

	#[test]
	fn test_visit_program() {}

	#[test]
	fn test_visit_field() {}

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

	// Arithmetic Op
	#[test]
	#[allow(unused_must_use)]
	fn test_simple_arith_ops() {
		use crate::treebuild::BuilderResult::*;
		use crate::decaf::Stmt;
		use crate::decaf::Expr;
		use crate::decaf::LiteralExpr::*;
		use crate::lnp::past::BinOp::*;
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Foo {
	public int methodA() {
	    int x;
	    x = 0;
	    x = x + 1;
	    x = x * x;
	    x = x / x;
	    x = x - x;
	    return 0;
	}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 1);
		assert_eq!(builder.visit_program(&program).unwrap(), Normal);

		match builder.class_map.get(&"Foo".to_string()) {
			None => assert_eq!(1, 0),
			Some(foo) => {
				assert_eq!(foo.name, "Foo");
				assert_eq!(foo.sup.borrow().as_ref().unwrap().name, "Object");
				assert_eq!(foo.pub_methods.borrow().len(), 1);

				// Method-wise check
				let method_a = &foo.pub_methods.borrow()[0];
				assert_eq!(method_a.name, "methodA");
				assert_eq!(method_a.return_ty.borrow().base, IntTy);
				assert_eq!(method_a.args.borrow().len(), 0);

				// Block-wise check
				let body = method_a.body.borrow();
				let block = body.as_ref().unwrap();
				assert_eq!(block.vartbl.borrow().len(), 1);
				assert_eq!(block.stmts.borrow().len(), 7);
				assert_eq!(block.vartbl.borrow()[0].name, "x");
				assert_eq!(block.vartbl.borrow()[0].ty.base, IntTy);
				assert_eq!(block.vartbl.borrow()[0].ty.array_lvl, 0);

				// Statement-by-statement check
				let stmt_int_x = &block.stmts.borrow()[0];
				{
					assert!(matches!(stmt_int_x, Stmt::Declare(_)));
					match stmt_int_x {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "x");
							assert_eq!(stmt.ty.base, IntTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							assert_eq!(stmt.init_expr, None);
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_x_eq_0 = &block.stmts.borrow()[1];
				{
					assert!(matches!(stmt_x_eq_0, Stmt::Expr(_)));
					match stmt_x_eq_0 {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Literal(lit_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											match lit_expr {
												IntLiteral(i) => {
													assert_eq!(*i, 0);
												},
												_ => panic!("lit expected to be a LiteralExpr"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and Literal")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_x_eq_x_plus_1 = &block.stmts.borrow()[2];
				{

					assert!(matches!(stmt_x_eq_x_plus_1, Stmt::Expr(_)));
					match stmt_x_eq_x_plus_1 {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::BinArith(ba_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(ba_expr.op, PlusOp);
											match (&*ba_expr.lhs, &*ba_expr.rhs) {
												(Variable(var_expr), Literal(IntLiteral(i))) => {
													assert_eq!(var_expr.var.name, "x");
													assert_eq!(var_expr.var.ty.base, IntTy);
													assert_eq!(var_expr.var.ty.array_lvl, 0);
													assert_eq!(*i, 1);
												},
												_ => panic!("lhs, rhs expected to be Variable and Literal"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and BinArith")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_x_eq_x_times_x = &block.stmts.borrow()[3];
				{
					assert!(matches!(stmt_x_eq_x_times_x, Stmt::Expr(_)));
					match stmt_x_eq_x_times_x {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::BinArith(ba_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(ba_expr.op, TimesOp);
											match (&*ba_expr.lhs, &*ba_expr.rhs) {
												(Variable(var_expr), Variable(var_expr2)) => {
													assert_eq!(var_expr.var.name, "x");
													assert_eq!(var_expr.var.ty.base, IntTy);
													assert_eq!(var_expr.var.ty.array_lvl, 0);
													assert_eq!(var_expr2.var.name, "x");
													assert_eq!(var_expr2.var.ty.base, IntTy);
													assert_eq!(var_expr2.var.ty.array_lvl, 0);
												},
												_ => panic!("lhs, rhs expected to be Variable and Variable"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and BinArith")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_x_eq_x_div_x = &block.stmts.borrow()[4];
				{
					assert!(matches!(stmt_x_eq_x_div_x, Stmt::Expr(_)));
					match stmt_x_eq_x_div_x {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::BinArith(ba_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(ba_expr.op, DivideOp);
											match (&*ba_expr.lhs, &*ba_expr.rhs) {
												(Variable(var_expr), Variable(var_expr2)) => {
													assert_eq!(var_expr.var.name, "x");
													assert_eq!(var_expr.var.ty.base, IntTy);
													assert_eq!(var_expr.var.ty.array_lvl, 0);
													assert_eq!(var_expr2.var.name, "x");
													assert_eq!(var_expr2.var.ty.base, IntTy);
													assert_eq!(var_expr2.var.ty.array_lvl, 0);
												},
												_ => panic!("lhs, rhs expected to be Variable and Variable"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and BinArith")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_x_eq_x_minus_x = &block.stmts.borrow()[5];
				{
					assert!(matches!(stmt_x_eq_x_minus_x, Stmt::Expr(_)));
					match stmt_x_eq_x_minus_x {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::BinArith(ba_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(ba_expr.op, MinusOp);
											match (&*ba_expr.lhs, &*ba_expr.rhs) {
												(Variable(var_expr), Variable(var_expr2)) => {
													assert_eq!(var_expr.var.name, "x");
													assert_eq!(var_expr.var.ty.base, IntTy);
													assert_eq!(var_expr.var.ty.array_lvl, 0);
													assert_eq!(var_expr2.var.name, "x");
													assert_eq!(var_expr2.var.ty.base, IntTy);
													assert_eq!(var_expr2.var.ty.array_lvl, 0);
												},
												_ => panic!("lhs, rhs expected to be Variable and Variable"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and BinArith")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}
			}
		}
	}

	// Cmp Op
	#[test]
	#[allow(unused_must_use)]
	fn test_simple_cmp_ops() {
		use crate::treebuild::BuilderResult::*;
		use crate::decaf::Stmt;
		use crate::decaf::Expr;
		use crate::decaf::LiteralExpr::*;
		use crate::lnp::past::BinOp::*;
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Foo {
	public int methodA() {
	    int x = 19;
	    x == 0;
	    x != 10;

	    boolean y = false;
	    boolean z = true;
	    z == y;
	    z != y;
	    z = y;

	    char c = 'q';
	    char d = 'e';
	    c == d;
	    c != d;
	    return 0;
	}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 1);
		assert_eq!(builder.visit_program(&program).unwrap(), Normal);

		match builder.class_map.get(&"Foo".to_string()) {
			None => assert_eq!(1, 0),
			Some(foo) => {
				assert_eq!(foo.name, "Foo");
				assert_eq!(foo.sup.borrow().as_ref().unwrap().name, "Object");
				assert_eq!(foo.pub_methods.borrow().len(), 1);

				// Method-wise check
				let method_a = &foo.pub_methods.borrow()[0];
				assert_eq!(method_a.name, "methodA");
				assert_eq!(method_a.return_ty.borrow().base, IntTy);
				assert_eq!(method_a.args.borrow().len(), 0);

				// Block-wise check
				let body = method_a.body.borrow();
				let block = body.as_ref().unwrap();
				assert_eq!(block.vartbl.borrow().len(), 5);
				assert_eq!(block.stmts.borrow().len(), 13);
				assert_eq!(block.vartbl.borrow()[0].name, "x");
				assert_eq!(block.vartbl.borrow()[0].ty.base, IntTy);
				assert_eq!(block.vartbl.borrow()[0].ty.array_lvl, 0);

				assert_eq!(block.vartbl.borrow()[1].name, "y");
				assert_eq!(block.vartbl.borrow()[1].ty.base, BoolTy);
				assert_eq!(block.vartbl.borrow()[1].ty.array_lvl, 0);

				assert_eq!(block.vartbl.borrow()[2].name, "z");
				assert_eq!(block.vartbl.borrow()[2].ty.base, BoolTy);
				assert_eq!(block.vartbl.borrow()[2].ty.array_lvl, 0);

				assert_eq!(block.vartbl.borrow()[3].name, "c");
				assert_eq!(block.vartbl.borrow()[3].ty.base, CharTy);
				assert_eq!(block.vartbl.borrow()[3].ty.array_lvl, 0);

				assert_eq!(block.vartbl.borrow()[4].name, "d");
				assert_eq!(block.vartbl.borrow()[4].ty.base, CharTy);
				assert_eq!(block.vartbl.borrow()[4].ty.array_lvl, 0);

				// Statement-by-statement check
				let stmt_int_x = &block.stmts.borrow()[0];
				{
					assert!(matches!(stmt_int_x, Stmt::Declare(_)));
					match &stmt_int_x {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "x");
							assert_eq!(stmt.ty.base, IntTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(IntLiteral(i)) => {
											assert_eq!(*i, 19);
										}
										_ => panic!("expr expected to be an IntLiteral of 19"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_x_eqs_0 = &block.stmts.borrow()[1];
				{
					assert!(matches!(stmt_x_eqs_0, Stmt::Expr(_)));
					match stmt_x_eqs_0 {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, EqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Literal(lit_expr)) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											match lit_expr {
												IntLiteral(i) => {
													assert_eq!(*i, 0);
												},
												_ => panic!("lit expected to be a LiteralExpr"),
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and Literal")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_x_neqs_10 = &block.stmts.borrow()[2];
				{

					assert!(matches!(stmt_x_neqs_10, Stmt::Expr(_)));
					match stmt_x_neqs_10 {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, NotEqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Literal(IntLiteral(i))) => {
											assert_eq!(var_expr.var.name, "x");
											assert_eq!(var_expr.var.ty.base, IntTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(*i, 10);
										}
										_ => panic!("lhs, rhs expected to be Variable and IntLiteral")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_boolean_y = &block.stmts.borrow()[3];
				{
					assert!(matches!(stmt_boolean_y, Stmt::Declare(_)));
					match &stmt_boolean_y {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "y");
							assert_eq!(stmt.ty.base, BoolTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(BoolLiteral(i)) => {
											assert_eq!(*i, false);
										}
										_ => panic!("expr expected to be an BoolLiteral of false"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_boolean_z = &block.stmts.borrow()[4];
				{
					assert!(matches!(stmt_boolean_z, Stmt::Declare(_)));
					match &stmt_boolean_z {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "z");
							assert_eq!(stmt.ty.base, BoolTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(BoolLiteral(i)) => {
											assert_eq!(*i, true);
										}
										_ => panic!("expr expected to be an BoolLiteral of true"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_z_eqs_y = &block.stmts.borrow()[5];
				{
					assert!(matches!(stmt_z_eqs_y, Stmt::Expr(_)));
					match stmt_z_eqs_y {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, EqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Variable(var_expr2)) => {
											assert_eq!(var_expr.var.name, "z");
											assert_eq!(var_expr.var.ty.base, BoolTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(var_expr2.var.name, "y");
											assert_eq!(var_expr2.var.ty.base, BoolTy);
											assert_eq!(var_expr2.var.ty.array_lvl, 0);
										}
										_ => panic!("lhs, rhs expected to be Variable and Variable")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_z_neq_y = &block.stmts.borrow()[6];
				{
					assert!(matches!(stmt_z_neq_y, Stmt::Expr(_)));
					match stmt_z_neq_y {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, NotEqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Variable(var_expr2)) => {
											assert_eq!(var_expr.var.name, "z");
											assert_eq!(var_expr.var.ty.base, BoolTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(var_expr2.var.name, "y");
											assert_eq!(var_expr2.var.ty.base, BoolTy);
											assert_eq!(var_expr2.var.ty.array_lvl, 0);
										}
										_ => panic!("lhs, rhs expected to be Variable and Variable")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_z_eq_y = &block.stmts.borrow()[7];
				{
					assert!(matches!(stmt_z_eq_y, Stmt::Expr(_)));
					match stmt_z_eq_y {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::Assign(expr) => {
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Variable(var_expr2)) => {
											assert_eq!(var_expr.var.name, "z");
											assert_eq!(var_expr.var.ty.base, BoolTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(var_expr2.var.name, "y");
											assert_eq!(var_expr2.var.ty.base, BoolTy);
											assert_eq!(var_expr2.var.ty.array_lvl, 0);
										}
										_ => panic!("lhs, rhs expected to be Variable and Variable")
									}
								}
								_ => panic!("expr expected to be Assign expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_char_c = &block.stmts.borrow()[8];
				{
					assert!(matches!(stmt_char_c, Stmt::Declare(_)));
					match stmt_char_c {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "c");
							assert_eq!(stmt.ty.base, CharTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(CharLiteral(i)) => {
											assert_eq!(*i, 'q');
										}
										_ => panic!("expr expected to be an CharLiteral of 'q'"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_char_d = &block.stmts.borrow()[9];
				{
					assert!(matches!(stmt_char_d, Stmt::Declare(_)));
					match stmt_char_d {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "d");
							assert_eq!(stmt.ty.base, CharTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(CharLiteral(i)) => {
											assert_eq!(*i, 'e');
										}
										_ => panic!("expr expected to be an CharLiteral of 'e'"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_c_eqs_d = &block.stmts.borrow()[10];
				{
					assert!(matches!(stmt_c_eqs_d, Stmt::Expr(_)));
					match stmt_c_eqs_d {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, EqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Variable(var_expr2)) => {
											assert_eq!(var_expr.var.name, "c");
											assert_eq!(var_expr.var.ty.base, CharTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(var_expr2.var.name, "d");
											assert_eq!(var_expr2.var.ty.base, CharTy);
											assert_eq!(var_expr2.var.ty.array_lvl, 0);
										}
										_ => panic!("lhs, rhs expected to be Variable and Variable")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}

				let stmt_c_neq_d = &block.stmts.borrow()[11];
				{
					assert!(matches!(stmt_c_neq_d, Stmt::Expr(_)));
					match stmt_c_neq_d {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinCmp(expr) => {
									assert_eq!(expr.op, NotEqualsOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::Variable(var_expr), Expr::Variable(var_expr2)) => {
											assert_eq!(var_expr.var.name, "c");
											assert_eq!(var_expr.var.ty.base, CharTy);
											assert_eq!(var_expr.var.ty.array_lvl, 0);
											assert_eq!(var_expr2.var.name, "d");
											assert_eq!(var_expr2.var.ty.base, CharTy);
											assert_eq!(var_expr2.var.ty.array_lvl, 0);
										}
										_ => panic!("lhs, rhs expected to be Variable and Variable")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}


			}
		}
	}

	#[test]
	#[allow(unused_must_use)]
	fn test_simple_logical_ops() {
		use crate::treebuild::BuilderResult::*;
		use crate::decaf::Stmt;
		use crate::decaf::Expr;
		use crate::decaf::LiteralExpr::*;
		use crate::lnp::past::BinOp::*;
		let mut builder = DecafTreeBuilder::new();

		let program_str = r#"
class Foo {
	public int methodA() {
	    int x = 19;
	    x == 0 && x != 10;
	    x >= 0;

	    boolean y = false;
	    boolean z = true;
	    z == y && z != y;
	    z != y;
	    z = y;

	    char c = 'q';
	    char d = 'e';
	    c == d;
	    c != d;

   	    x == 0 || x != 10;
	    x > 0 || x < 10;
	    x >= 0 || x <= 10;
	    return 0;
	}
}
"#;
		let program = lnp::parse_decaf(program_str);
		assert_eq!(program.classes.len(), 1);
		assert_eq!(builder.visit_program(&program).unwrap(), Normal);

		match builder.class_map.get(&"Foo".to_string()) {
			None => assert_eq!(1, 0),
			Some(foo) => {
				assert_eq!(foo.name, "Foo");
				assert_eq!(foo.sup.borrow().as_ref().unwrap().name, "Object");
				assert_eq!(foo.pub_methods.borrow().len(), 1);

				// Method-wise check
				let method_a = &foo.pub_methods.borrow()[0];
				assert_eq!(method_a.name, "methodA");
				assert_eq!(method_a.return_ty.borrow().base, IntTy);
				assert_eq!(method_a.args.borrow().len(), 0);

				// Block-wise check
				let body = method_a.body.borrow();
				let block = body.as_ref().unwrap();
				{
					assert_eq!(block.vartbl.borrow().len(), 5);
					assert_eq!(block.stmts.borrow().len(), 16);
					assert_eq!(block.vartbl.borrow()[0].name, "x");
					assert_eq!(block.vartbl.borrow()[0].ty.base, IntTy);
					assert_eq!(block.vartbl.borrow()[0].ty.array_lvl, 0);

					assert_eq!(block.vartbl.borrow()[1].name, "y");
					assert_eq!(block.vartbl.borrow()[1].ty.base, BoolTy);
					assert_eq!(block.vartbl.borrow()[1].ty.array_lvl, 0);

					assert_eq!(block.vartbl.borrow()[2].name, "z");
					assert_eq!(block.vartbl.borrow()[2].ty.base, BoolTy);
					assert_eq!(block.vartbl.borrow()[2].ty.array_lvl, 0);

					assert_eq!(block.vartbl.borrow()[3].name, "c");
					assert_eq!(block.vartbl.borrow()[3].ty.base, CharTy);
					assert_eq!(block.vartbl.borrow()[3].ty.array_lvl, 0);

					assert_eq!(block.vartbl.borrow()[4].name, "d");
					assert_eq!(block.vartbl.borrow()[4].ty.base, CharTy);
					assert_eq!(block.vartbl.borrow()[4].ty.array_lvl, 0);
				}

				// Statement-by-statement check
				let stmt_int_x = &block.stmts.borrow()[0];
				{
					assert!(matches!(stmt_int_x, Stmt::Declare(_)));
					match &stmt_int_x {
						Stmt::Declare(stmt) => {
							assert_eq!(stmt.name, "x");
							assert_eq!(stmt.ty.base, IntTy);
							assert_eq!(stmt.ty.array_lvl, 0);
							match &stmt.init_expr {
								Some(expr) => {
									match &expr {
										Literal(IntLiteral(i)) => {
											assert_eq!(*i, 19);
										}
										_ => panic!("expr expected to be an IntLiteral of 19"),
									}
								}
								_ => panic!("stmt expected to have init_expr"),
							}
						}
						_ => panic!("stmt_int_x expected to be Declare"),
					}
				}

				let stmt_x_eqs_0 = &block.stmts.borrow()[1];
				{
					assert!(matches!(stmt_x_eqs_0, Stmt::Expr(_)));
					match stmt_x_eqs_0 {
						Stmt::Expr(stmt) => {
							let expr = &stmt.expr;
							match expr {
								Expr::BinLogical(expr) => {
									assert_eq!(expr.op, LogicalAndOp);
									match (&*expr.lhs, &*expr.rhs) {
										(Expr::BinCmp(expr_x_eqs_0), Expr::BinCmp(expr_x_neqs_10)) => {
											assert_eq!(expr_x_eqs_0.op, EqualsOp);
											assert_eq!(expr_x_neqs_10.op, NotEqualsOp);
											match (&*expr_x_eqs_0.lhs, &*expr_x_eqs_0.rhs, &*expr_x_neqs_10.lhs, &*expr_x_neqs_10.rhs) {
												(Expr::Variable(var_expr),
													Expr::Literal(lit_0),
													Expr::Variable(var_expr2),
													Expr::Literal(lit_10)) => {

													assert_eq!(var_expr.var.name, "x");
													assert_eq!(var_expr.var.ty.base, IntTy);
													assert_eq!(var_expr.var.ty.array_lvl, 0);
													match lit_0 {
														IntLiteral(i) => {
															assert_eq!(*i, 0);
														},
														_ => panic!("lit expected to be a LiteralExpr of 0"),
													}

													assert_eq!(var_expr2.var.name, "x");
													assert_eq!(var_expr2.var.ty.base, IntTy);
													assert_eq!(var_expr2.var.ty.array_lvl, 0);
													match lit_10 {
														IntLiteral(i) => {
															assert_eq!(*i, 10);
														},
														_ => panic!("lit expected to be a LiteralExpr of 10"),
													}
												}
												_ => panic!("x == 0 && x != 0")
											}
										}
										_ => panic!("lhs, rhs expected to be Variable and Literal")
									}
								}
								_ => panic!("expr expected to be BinCmp expr")
							}
						}
						_ => panic!("stmt expected to be Expr statement"),
					}
				}
			}
		}
	}


	// While Loop
}
