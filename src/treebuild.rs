trait Visitor {
	use crate::lnp::past::*;

	type Result;

	fn visit_program(&mut self, prog: &Program) -> Self::Result;
	fn visit_class(&mut self, cls: &ClassNode) -> Self::Result;
	fn visit_field(&mut self, fld: &Field) -> Self::Result;
	fn visit_method(&mut self, mthd: &Method) -> Self::Result;
	fn visit_ctor(&mut self, ctor: &Ctor) -> Self::Result;
	fn visit_formalarg(&mut self, farg: &FormalArg) -> Self::Result;
	fn visit_type(&mut self, ty: &Type) -> Self::Result;
	fn visit_vardecl(&mut self, vardecl: &VarDeclarator) -> Self::Result;
	fn visit_expr(&mut self, expr: &Expression) -> Self::Result;
	fn visit_block(&mut self, block: &Block) -> Self::Result;
	fn visit_stmt(&mut self, stmt: &Statement) -> Self::Result;
	fn visit_declare_stmt(&mut self, ty: &Type,
						  vardecls: &Vec<VarDeclarator>) -> Self::Result;
	fn visit_if_stmt(&self, condexpr: &Expression,
					 thenstmt: &Box<Statement>) -> Self::Result;
	fn visit_if_else_stmt(&self, condexpr: &Expression,
						  thenstmt: &Box<Statement>,
						  elsestmt: &Box<Statement>) -> Self::Result;
	fn visit_while_stmt(&self, condexpr: &Expression,
					   loopstmt: &Box<Statement>) -> Self::Result;
	fn visit_super_stmt(&self, stmt: &Statement) -> Self::Result;
	fn visit_primary(&self, prim; &Primary) -> Self::Result;
	fn visit_naexpr(&self, expr: &NewArrayExpr) -> Self::Result;
	fn visit_nnaexpr(&self, expr: &NNAExpr) -> Self::Result;
	fn visit_new_obj(&self, name: &String, args: &ActualArgs) -> Self::Result;
	fn visit_call_self_method(&self, name: &String, args: &ActualArgs) -> Self::Result;
	fn visit_call_method(&self, prim: &Primary, name: &String, args: &ActualArgs) -> Self::Result;
	fn visit_call_super(&self, name: &String, args: &ActualArgs) -> Self::Result;
	fn visit_array_expr(&self, expr: &ArrayExpr) -> Self::Result;
	fn visit_field_expr(&self, expr: &FieldExpr) -> Self::Result;
	fn visit_actural_args(&self, args: &ActualArgs) -> Self::Result;
}

trait Visitable {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result;
}

impl Visitable for Program {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_program(self)
	}
}

impl Visitable for ClassNode {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_class(self)
	}
}

impl Visitable for Field {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_field(self)
	}
}

impl Visitable for Method {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_method(self)
	}
}

impl Visitable for Ctor {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_ctor(self)
	}
}

trait Scope {
	type K;
	type V: Clone;
	type F: Clone;

	fn lookup(&self, &Self::K) -> Option<Self::V>;
	fn add(&mut self, &Self::K, &Self::V);
	fn get_type_resolution_hooks(&mut self, &Self::K) -> Option<Vec<Box<dyn FnMut(Self::V)>>>;
	fn add_type_resolution_hook(&mut self, &Self::K, Box<dyn FnMut(Self::V)>);
	fn get_func_resolution_hooks(&mut self, &Self::K) -> Option<Vec<Box<dyn FnMut(Self::F)>>>;
	fn add_func_resolution_hook(&mut self, &Self::K, Box<dyn FnMut(Self::F)>);
}

enum BuilderState {
	First, // Symbol resolution
	Second,
}

enum BuilderResult {
	Normal,
	Node(decaf::Node),
}

struct DecafTreeBuilder {
	state: BuilderState,
	scopes: Vec<Rf<Scope>>,
	ctbl: BTreeMap<String, Rc<decaf::Class>>,
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

	fn load_builtin(&mut self) {
		// TODO: load built-in classes: Object, String and IO
		// TODO: reset scopes
	}

	fn save_classes(&mut self) {
		// Save
	}
}

impl Visitor for DecafTreeBuilder {
	type Result = Result<BuilderResult, SemanticError>;

	fn visit_program(&mut self, prog: &Program) -> Self::Result {
		self.load_builtin();

		// Two pass to resolve all symbols
		self.state = BuilderState::First;
		for cls in prog.classes.iter() {
			cls.accept(self)?;
		}

		// Second pass
		self.state = BuilderState::Second;
		for cls in prog.classes.iter() {
			cls.accept(self)?;
		}

		Ok(Normal)
	}

	fn visit_class(&mut self, cls: &ClassNode) -> Self::Result {
		match self.state {
			BuilderState::First => {
				// Check for class name conflicts
				if let Some(t) = self.type_lookup(cls.name) {
					match t {
						decaf::Class(c) => Err(ClassConflict(c)),
						_ => Err(BadImplmentation("bad conflict in class name"))
					}
				} else {
					// Create new class type
					let newclass = Rc::new(decaf::Class::new(cls.name));

					// Try to get the super type
					let newclass = match cls.sup {
						None => {
							// Inherits from Object class
							Rc::new(decaf::Class::new(
								cls.name,
								self.type_lookup("Object")?,
							))
						},
						Some(supnode) => {
							let c = Rc::new(decaf::Class::new(
								cls.name,
								decaf::Type::Unknown(supnode.name), // Unknown class
							));
							match self.type_lookup(supnode.name) {
								None => {
									// Unresolved name, add a resolution hook
									self.scopes.last_mut()
										.add_type_resolution_hook(supnode.name,
																  move |ty| {
																	  c.clone().set_super(ty);
																  });
								}
							}
						}
					}

					// Add the type to the current scope
					self.type_add(cls.name, decaf::Type::Class(newclass.clone()));

					// Enter new scope
					self.scopes.push(newclass.clone());

					for member in cls.member_list {
						match member {
							FieldMember(field) => field.accept(self)?,
							MethodMember(method) => method.accept(self)?,
							CtorMember(ctor) => ctor.accept(self)?,
						}
					}

					// Save class
					self.ctbl.insert(cls.name, newclass.clone());

					// Pop scope
					self.scopes.pop();

					Ok(Normal)
				}
			}
			BuilderState::Second => {

			}
		}
	}

	fn visit_field(&mut self, field: &Field) -> Self::Result {
		match self.state {
			BuilderState::First => {
				// Only allow 1 modifier and if it's pub/prot/priv
				match (field.modies.len(), field.modies.last()) {
					(1, Some(modifier)) => {
						let vis = match modifier => {
							ModPublic => Visibility::Pub,
							ModPrivate => Visibility::Priv,
							ModProtected => Visibility::Prot,
							_ => panic!("static field not allowed"),
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
