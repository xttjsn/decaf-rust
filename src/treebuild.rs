use crate::lnp;
use crate::decaf;
use std::rc::Rc;

trait Visitor {
	type Result;

	fn visit_program(&mut self, prog: &lnp::past::Program) -> Self::Result;
	fn visit_class(&mut self, cls: &lnp::past::ClassNode) -> Self::Result;
	fn visit_field(&mut self, fld: &lnp::past::Field) -> Self::Result;
	fn visit_method(&mut self, mthd: &lnp::past::Method) -> Self::Result;
	fn visit_ctor(&mut self, ctor: &lnp::past::Ctor) -> Self::Result;
	fn visit_formalarg(&mut self, farg: &lnp::past::FormalArg) -> Self::Result;
	fn visit_type(&mut self, ty: &lnp::past::Type) -> Self::Result;
	fn visit_vardecl(&mut self, vardecl: &lnp::past::VarDeclarator) -> Self::Result;
	fn visit_expr(&mut self, expr: &lnp::past::Expression) -> Self::Result;
	fn visit_block(&mut self, block: &lnp::past::Block) -> Self::Result;
	fn visit_stmt(&mut self, stmt: &lnp::past::Statement) -> Self::Result;
	fn visit_declare_stmt(&mut self, ty: &lnp::past::Type,
						  vardecls: &Vec<lnp::past::VarDeclarator>) -> Self::Result;
	fn visit_if_stmt(&self, condexpr: &lnp::past::Expression,
					 thenstmt: &Box<lnp::past::Statement>) -> Self::Result;
	fn visit_if_else_stmt(&self, condexpr: &lnp::past::Expression,
						  thenstmt: &Box<lnp::past::Statement>,
						  elsestmt: &Box<lnp::past::Statement>) -> Self::Result;
	fn visit_while_stmt(&self, condexpr: &lnp::past::Expression,
						loopstmt: &Box<lnp::past::Statement>) -> Self::Result;
	fn visit_super_stmt(&self, stmt: &lnp::past::Statement) -> Self::Result;
	fn visit_primary(&self, prim: &lnp::past::Primary) -> Self::Result;
	fn visit_naexpr(&self, expr: &lnp::past::NewArrayExpr) -> Self::Result;
	fn visit_nnaexpr(&self, expr: &lnp::past::NNAExpr) -> Self::Result;
	fn visit_new_obj(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result;
	fn visit_call_self_method(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result;
	fn visit_call_method(&self, prim: &lnp::past::Primary, name: &String, args: &lnp::past::ActualArgs) -> Self::Result;
	fn visit_call_super(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result;
	fn visit_array_expr(&self, expr: &lnp::past::ArrayExpr) -> Self::Result;
	fn visit_field_expr(&self, expr: &lnp::past::FieldExpr) -> Self::Result;
	fn visit_actural_args(&self, args: &lnp::past::ActualArgs) -> Self::Result;
}

trait Visitable {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result;
}

impl Visitable for lnp::past::Program {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_program(self)
	}
}

impl Visitable for lnp::past::ClassNode {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_class(self)
	}
}

impl Visitable for lnp::past::Field {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_field(self)
	}
}

impl Visitable for lnp::past::Method {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_method(self)
	}
}

impl Visitable for lnp::past::Ctor {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_ctor(self)
	}
}

trait Normalize {
	type Result;
	fn normalize(&self) -> Self::Result;
}

impl Normalize for lnp::past::Field {
	type Result = Vec<(decaf::TypeBase, u32, String, Option<lnp::past::Expression>)>;

	fn normalize(&self) -> Self::Result {
		let result = vec![];

		fn go(ty: &lnp::past::Type, lvl: u32) -> (u32, decaf::Type) {
			// Count array nesting level from type
			match ty {
				lnp::past::Type::Primitive(prim) => {
					match prim {
						lnp::past::PrimitiveType::BoolType => (lvl, decaf::TypeBase::BoolTy),
						lnp::past::PrimitiveType::CharType => (lvl, decaf::TypeBase::CharTy),
						lnp::past::PrimitiveType::IntType => (lvl, decaf::TypeBase::IntTy),
						lnp::past::PrimitiveType::VoidType => (lvl, decaf::TypeBase::VoidTy),
					}
				}
				lnp::past::Type::Custom(cls_name) => (lvl, decaf::TypeBase::UnknownTy(cls_name)),
				lnp::past::Type::Array(ty) => go(*ty, lvl + 1),
			}
		}

		let (array_lvl, decaf_type) = go(&self.ty, 0);

		for vard in self.var_decl.iter() {
			let (cnt, name, expr) = vard.normalize();
			result.push((decaf_type, array_lvl + cnt, name, expr));
		}
		result
	}
}

impl Normalize for lnp::past::VarDeclarator {
	type Result = (u32, String, Option<lnp::past::Expression>);	// array_cnt, name, init_expr
	fn normalize(&self) -> Self::Result {
		fn go(vid: lnp::past::VarDeclaratorId, lvl: u32) -> (u32, String) {
			match vid.id {
				lnp::VarDeclaratorIdInner::Single(name) => (lvl, name),
				lnp::VarDeclaratorIdInner::Array(vid_next) => {
					go(*vid_next, lvl + 1)
				}
			}
		}
		let (lvl, name) = go(self.vardeclid, 0);
		(lvl, name, self.expr.clone())
	}
}

pub enum BuilderState {
	First, // Symbol resolution
	Second,
}

pub enum BuilderResult {
	Normal,
	Node(decaf::Node),
}

// trait Scope {
// 	type K;
// 	type V: Clone;
// 	type F: Clone;

// 	fn lookup(&self, &Self::K) -> Option<Self::V>;
// 	fn add(&mut self, &Self::K, &Self::V);
// 	fn get_type_resolution_hooks(&mut self, &Self::K) -> Option<Vec<Box<dyn FnMut(Self::V)>>>;
// 	fn add_type_resolution_hook(&mut self, &Self::K, Box<dyn FnMut(Self::V)>);
// 	fn get_func_resolution_hooks(&mut self, &Self::K) -> Option<Vec<Box<dyn FnMut(Self::F)>>>;
// 	fn add_func_resolution_hook(&mut self, &Self::K, Box<dyn FnMut(Self::F)>);
// }
pub enum Scope {
	ClassScope(Rc<decaf::Class>),
	FunctionScope(Rc<decaf::Function>),
	MethodScope(Rc<decaf::Method>),
	BlockScope(Rc<decaf::Block>),
}

impl Scope {
	fn symbol_lookup(&self, name: &str, ) -> Option<decaf::Type> {
		use Scope::*;
		match *self {
			ClassScope(cls) => {
				// Field lookup
				for field in cls.inh_fields.iter() {
					if field.name == name {
						return Some(field.ty.clone())
					}
				}

				// Method lookup
				for field

				//
			},
			FunctionScope(func) => {
				panic!("not implemented")
			},
			MethodScope(func) => {
				panic!("not implemented")
			},
			BlockScope(func) => {
				panic!("not implemented")
			}
		}
	}
}

struct DecafTreeBuilder {
	state: BuilderState,
}

impl DecafTreeBuilder {
	fn type_lookup(&mut self, name: &String) -> Option<decaf::Type> {
		for scope in self.scopes.iter() {
			match scope.lookup(name) {
				Some(ty) => Some(ty),
				None => continue,
			}
		}
		None
	}

	fn type_add(&mut self, name: &String, ty: decaf::Type) {
		// Add type to the current scope
		match self.scopes.last_mut() {
			Some(curr_scope) => {
				curr_scope.add(name, ty.clone());
			},
			None => panic!("missing scope")
		}

		// If there are unresolved symbols within current scope
		// Check if name matches, if so invoke the resolution hook
		for hook in self.scopes
			.last_mut()
			.unwarp()
			.get_type_resolution_hooks(name).
			unwarp()
			.into_iter() {
			hook(ty.clone());
		}
	}

	fn get_curr_scope_as_class(&self) -> Rc<decaf::Class>{
		use decaf::Scope::*;
		match self.scopes.last() {
			None => panic!("no scope"),
			Some(s) => match s {
				ClassScope(cls) => cls.clone(),
				_ => panic!("current scope is not class"),
			}
		}
	}

	fn load_builtin(&mut self) {
		// TODO: load built-in classes: Object, String and IO
		// TODO: reset scopes
	}

	fn save_classes(&mut self) {
		// Save
	}
}

impl Visitor for DecafTreeBuilder {
	use decaf::SemanticError;
	type Result = Result<BuilderResult, SemanticError>;

	fn visit_program(&mut self, prog: &Program) -> Self::Result {
		self.load_builtin();

		// Two pass to resolve all symbols
		self.state = BuilderState::First;
		for cls in prog.classes.iter() {
			cls.accept(self)?;
		}

		Ok(Normal)
	}

	fn visit_class(&mut self, cls: &ClassNode) -> Self::Result {
		use decaf::Type::*;
		use decaf::SemanticError::*;
		match self.state {
			BuilderState::First => {
				// Check for class name conflicts
				if let Some(t) = self.type_lookup(cls.name) {
					match t {
						ClassTy(c) => Err(ClassConflict(c)),
						_ => Err(BadImplmentation("bad conflict in class name"))
					}
				} else {
					// Create new class type
					let newclass = Rc::new(decaf::Class::new(cls.name));

					// Try to get the super type
					match cls.sup {
						None => {
							// Inherits from Object class
							newclass.set_super(self.type_lookup("Object")?);
						},
						Some(supnode) => {
							match self.class_lookup(supnode.name) {
								None => {
									// Unresolved name, add a resolution hook
									self.add_class_resolution_hook(supnode.name,
																   move |cls| {
																	   c.clone().set_super(cls);
																   });
								},
								Some(supcls) => {
									newclass.set_super(supcls.clone());
								}
							}
						}
					}

					// Add the type to the current class_list
					self.class_list.push(newclass.clone());

					self.scopes.push(ClassScope(newclass.clone()));

					for member in cls.member_list {
						match member {
							FieldMember(field) => field.accept(self)?,
							MethodMember(method) => method.accept(self)?,
							CtorMember(ctor) => ctor.accept(self)?,
						}
					}

					self.scopes.pop();

					Ok(Normal)
				}
			}
			BuilderState::Second => {
				panic!("not implemented")
			}
		}
	}

	fn visit_field(&mut self, field: &Field) -> Self::Result {
		use decaf::SemanticError::*;
		use decaf::TypeBase::*;

		match self.state {
			BuilderState::First => {
				// Normalize field
				for norm_field in field.normalize() {
					let (ty, array_lvl, field_name, init_expr) = norm_field;

					// Only allow 1 modifier and if it's pub/prot/priv
					match (field.modies.len(), field.modies.last()) {
						(1, Some(modifier)) => {
							let vis = match modifier {
								ModPublic => decaf::Visibility::Pub,
								ModPrivate => decaf::Visibility::Priv,
								ModProtected => decaf::Visibility::Prot,
								_ => panic!("static field not allowed"),
							};

							// Get current class
							let curr_cls = self.get_curr_scope_as_class();

							if curr_cls.fields.iter().any(|f| f.name == field_name) {
								panic!("field name conflict");
							}

							let field_node = Rc::new(decaf::Field::new(field_name));

							// Construct correct base type
							let field_type = match ty {
								UnknownTy(cls_name) => {
									match self.class_lookup(cls_name) {
										None => {
											// Unresolved name, add a resolution hook
											self.add_class_resolution_hook(cls_name,
																		   move |cls| {
																			   field_node.clone().set_base_type(Type {
																				   base: ClassTy(cls),
																				   array_lvl: array_lvl,
																			   });
																		   });
										},
										Some(cls) => {
											field_node.set_base_type(Type {
												base: ClassTy(cls),
												array_lvl: array_lvl,
											});
										}
									}
								},
								BoolTy => {
									field_node.set_base_type(Type {
										base: BoolTy,
										array_lvl: array_lvl,
									});
								},
								IntTy => {
									field_node.set_base_type(Type {
										base: IntTy,
										array_lvl: array_lvl,
									});
								},
								CharTy => {
									field_node.set_base_type(Type {
										base: CharTy,
										array_lvl: array_lvl,
									});
								},
								VoidTy => {
									if array_lvl != 0 {
										panic!("void type does not allow array");
									}
									field_node.set_base_type(Type {
										base: VoidTy,
										array_lvl: 0,
									});
								},
								ClassTy(cls) => {
									field_node.set_base_type(Type {
										base: ClassTy(cls),
										array_lvl: array_lvl,
									});
								}
							};

						},
						_ => {
							Err(InvalidModifier(field.span))
						}
					}
				}
			}
		}
	}
}
