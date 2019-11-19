use crate::lnp;
use crate::decaf;
use crate::decaf::{Value};
use std::rc::Rc;
use std::collections::BTreeMap;

pub trait Visitor {
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

impl Visitable for lnp::past::Block {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_block(self)
	}
}

impl Visitable for lnp::past::Expression {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
		visitor.visit_expr(self)
	}
}

trait Normalize {
	type Result;
	fn normalize(&self) -> Self::Result;
}

fn ty2ty(ty: &lnp::past::Type, lvl: u32) -> (u32, decaf::TypeBase) {
	// Count array nesting level from type
	match ty {
		lnp::past::Type::Primitive(prim) => {
			match prim {
				lnp::past::PrimitiveType::BoolType(_) => (lvl, decaf::TypeBase::BoolTy),
				lnp::past::PrimitiveType::CharType(_) => (lvl, decaf::TypeBase::CharTy),
				lnp::past::PrimitiveType::IntType(_) => (lvl, decaf::TypeBase::IntTy),
				lnp::past::PrimitiveType::VoidType(_) => (lvl, decaf::TypeBase::VoidTy),
			}
		}
		lnp::past::Type::Custom(cls_name) => (lvl, decaf::TypeBase::UnknownTy(cls_name.to_string())),
		lnp::past::Type::Array(ty) => ty2ty(&*ty, lvl + 1),
	}
}

impl Normalize for lnp::past::Method {
	type Result = (decaf::TypeBase, u32, String, Vec<(String, decaf::TypeBase, u32)>, lnp::past::Block);

	fn normalize(&self) -> Self::Result {
		use crate::lnp::past::VarDeclaratorIdInner::*;
		// Return type
		let (array_lvl, return_type) = ty2ty(&self.ty, 0);

		// Resolve argument type
		// We do not allow Array of VarDeclaratorIdInner
		let mut args = vec![];
		for arg in self.fargs.farg_list.iter() {
			let (arg_array_lvl, arg_type) = ty2ty(&arg.ty, 0);
			match &arg.var_decl_id.id {
				Single(arg_name) => {
					args.push((arg_name.clone(), arg_type, arg_array_lvl));
				},
				Array(_) => panic!("array of ids are not allowed in method argument")
			}
		}

		(return_type, array_lvl, self.name.clone(), args, self.block.clone())
	}
}

impl Normalize for lnp::past::Field {
	type Result = Vec<(decaf::TypeBase, u32, String, Option<lnp::past::Expression>)>;

	fn normalize(&self) -> Self::Result {
		let mut result = vec![];

		let (array_lvl, decaf_type) = ty2ty(&self.ty, 0);

		for vard in self.var_decl.iter() {
			let (cnt, name, expr) = vard.normalize();
			result.push((decaf_type.clone(), array_lvl + cnt, name, expr));
		}
		result
	}
}

impl Normalize for lnp::past::VarDeclarator {
	type Result = (u32, String, Option<lnp::past::Expression>);	// array_cnt, name, init_expr
	fn normalize(&self) -> Self::Result {
		fn go(vid: &lnp::past::VarDeclaratorId, lvl: u32) -> (u32, String) {
			match &vid.id {
				lnp::past::VarDeclaratorIdInner::Single(name) => (lvl, name.clone()),
				lnp::past::VarDeclaratorIdInner::Array(vid_next) => {
					go(&vid_next, lvl + 1)
				}
			}
		}
		let (lvl, name) = go(&self.vardeclid, 0);
		(lvl, name, self.expr.clone())
	}
}

pub enum BuilderState {
	First, // Symbol resolution
	Second,
}

pub enum BuilderResult {
	Normal,
	ExprNode(decaf::Expr)
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
	MethodScope(Rc<decaf::Method>),
	BlockScope(Rc<decaf::Block>),
}

impl Scope {
	fn symbol_lookup(&self, name: &str) -> Option<Rc<decaf::Type>> {
		use Scope::*;
		match self {
			ClassScope(cls) => {
				// Field lookup
				for field in cls.pub_fields.borrow().iter() {
					if field.name == name {
						return Some(field.ty.borrow().clone());
					}
				}

				// Method lookup
				for method in cls.pub_methods.borrow().iter() {
					if method.name == name {
						return Some(Rc::new(decaf::Type {
							base: decaf::TypeBase::MethodTy(method.clone()),
							array_lvl: 0,
						}));
					}
				}

				for method in cls.prot_methods.borrow().iter() {
					if method.name == name {
						return Some(Rc::new(decaf::Type {
							base: decaf::TypeBase::MethodTy(method.clone()),
							array_lvl: 0,
						}));
					}
				}

				for method in cls.priv_methods.borrow().iter() {
					if method.name == name {
						return Some(Rc::new(decaf::Type {
							base: decaf::TypeBase::MethodTy(method.clone()),
							array_lvl: 0,
						}));
					}
				}

				for func in cls.pub_static_methods.borrow().iter() {
					if func.name == name {
						return Some(Rc::new(decaf::Type {
							base: decaf::TypeBase::MethodTy(func.clone()),
							array_lvl: 0,
						}));
					}
				}
				None
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

pub struct DecafTreeBuilder {
	pub state: BuilderState,
	pub scopes: Vec<decaf::Scope>,
	pub class_hooks: BTreeMap<String, Vec<ClassHook>>,
	pub class_map: BTreeMap<String, Rc<decaf::Class>>,
}

type ClassHook = Box<dyn FnMut(Rc<decaf::Class>) -> Option<Rc<decaf::Class>>>;

impl DecafTreeBuilder {
	pub fn new() -> Self {
		DecafTreeBuilder {
			state: BuilderState::First,
			scopes: vec![],
			class_hooks: BTreeMap::new(),
			class_map: BTreeMap::new(),
		}
	}

	fn type_add(&mut self,
				base_ty: decaf::TypeBase,
				array_lvl: u32,
				hook: Option<ClassHook>) -> Option<decaf::Type> {
		use self::BuilderState::*;
		use crate::decaf::Type;
		use crate::decaf::TypeBase::*;
		match self.state {
			First => {
				match base_ty {
					UnknownTy(cls_name) => {
						match self.class_lookup(cls_name.clone()) {
							None => {
								if let Some(hook) = hook {
									self.add_class_resolution_hook(cls_name.clone(), hook);
								}
								None
							}
							Some(cls_base_ty) => {
								Some(Type{
									base: ClassTy(cls_base_ty.clone()),
									array_lvl: array_lvl,
								})
							}
						}
					},
					BoolTy | IntTy | CharTy  => Some(Type {
						base: base_ty,
						array_lvl: array_lvl,
					}),
					VoidTy => {
						if array_lvl != 0 {
							panic!("void type does not allow array");
						}
						Some(Type {
							base: VoidTy,
							array_lvl: 0,
						})
					},
					ClassTy(cls) => Some(Type {
						base: ClassTy(cls.clone()),
						array_lvl: array_lvl,
					}),
					MethodTy(_) => {
						panic!("bad field type: method");
					}
				}
			},
			Second => {
				panic!("not implemented")
			}
		}
	}

	fn class_lookup(&self, cls_name: String) -> Option<Rc<decaf::Class>> {
		match self.class_map.get(&cls_name) {
			None => None,
			Some(cls) => Some(cls.clone())
		}
	}

	fn class_add(&mut self, cls_name: String, cls: Rc<decaf::Class>) {
		self.class_map.insert(cls_name, cls);
	}

	fn add_class_resolution_hook(&mut self, cls_name: String, f: ClassHook) {
		self.class_hooks.entry(cls_name).or_insert(vec![]).push(f);
	}

	fn invoke_class_resolution_hook(&mut self, cls_name: String, cls: Rc<decaf::Class>) {
		fn go(class_hooks: &mut BTreeMap<String, Vec<ClassHook>>,
			  cls_name: String,
			  cls: Rc<decaf::Class>) {

			let mut tmp = vec![];
			if let Some(hooks) = class_hooks.remove(&cls_name) {
				for mut hook in hooks.into_iter() {
					if let Some(resolved_cls) = hook(cls.clone()) {
						tmp.push(resolved_cls);
					}
				}
			}

			for resolved_cls in tmp.into_iter() {
				go(class_hooks, resolved_cls.name.clone(), resolved_cls.clone());
			}
		}

		go(&mut self.class_hooks, cls_name, cls);
	}

	fn get_curr_scope_as_class(&self) -> Rc<decaf::Class>{
		match self.scopes.last() {
			None => panic!("no scope"),
			Some(s) => match s {
				crate::decaf::Scope::ClassScope(cls) => cls.clone(),
				_ => panic!("current scope is not class"),
			}
		}
	}

	fn has_class_in_scope_stack(&self, cls: &Rc<decaf::Class>) -> bool {
		for scope in self.scopes.iter() {
			match scope {
				ClassScope(ref scls) => {
					if scls == cls {
						return true;
					}
				},
				_ => continue
			}
		}
		false
	}

	fn load_builtin(&mut self) {
		// TODO: load built-in classes: Object, String and IO
		self.class_map.insert("Object".to_string(), Rc::new(decaf::Class::new("Object".to_string())));
		// TODO: reset scopes
	}

	fn save_classes(&mut self) {
		// Save
	}
}

impl Visitor for DecafTreeBuilder {
	type Result = Result<BuilderResult, decaf::SemanticError>;

	fn visit_program(&mut self, prog: &lnp::past::Program) -> Self::Result {
		self.load_builtin();

		// First pass to collect class list
		self.state = BuilderState::First;
		for cls in prog.classes.iter() {
			cls.accept(self)?;
		}

		// Second pass to start parsing
		// self.state = BuilderState::Second;
		// for cls in prog.classes.iter() {
		// 	cls.accept(self)?;
		// }

		Ok(BuilderResult::Normal)
	}

	fn visit_class(&mut self, cls_node: &lnp::past::ClassNode) -> Self::Result {
		use crate::decaf::SemanticError::*;
		match self.state {
			BuilderState::First => {
				// Check for class name conflicts
				if let Some(_) = self.class_lookup(cls_node.name.clone()) {
					return Err(ClassRedefinition(cls_node.name.clone()));
				} else {
					// Create new class type
					let new_class = Rc::new(decaf::Class::new(cls_node.name.clone()));

					// Try to get the super type
					match &cls_node.sup {
						None => {
							// Inherits from Object class
							new_class.clone().set_super(self.class_lookup("Object".to_string()).unwrap());
						},
						Some(supnode) => {
							match self.class_lookup(supnode.name.clone()) {
								None => {
									// Unresolved name, add a resolution hook
									let tmp = new_class.clone();
									self.add_class_resolution_hook(supnode.name.clone(),
																   Box::new(
																	   move |cls| {
																		   tmp.clone().set_super(cls);
																		   // Resolve
																		   Some(tmp.clone())
																	   }));
								},
								Some(supcls_node) => {
									// Set super if super class is resolved
									match *supcls_node.sup.borrow() {
										Some(_) => {
											new_class.clone().set_super(supcls_node.clone());
										},
										None => {
											// Otherwise register hook
											let tmp = new_class.clone();
											self.add_class_resolution_hook(supnode.name.clone(),
																		   Box::new(
																			   move |cls| {
																				   tmp.clone().set_super(cls);
																				   Some(tmp.clone())
																			   }
																		   ));
										}
									}

								}
							}
						}
					}

					// Add the type to the current class_list
					self.class_add(cls_node.name.clone(), new_class.clone());

					self.scopes.push(decaf::Scope::ClassScope(new_class.clone()));

					println!("trying to visit class member");
					for member in cls_node.member_list.iter() {
						match member {
							lnp::past::Member::FieldMember(field) => field.accept(self)?,
							lnp::past::Member::MethodMember(method) => method.accept(self)?,
							lnp::past::Member::CtorMember(ctor) => ctor.accept(self)?,
						};
					}

					self.scopes.pop();

					// If the class is fully resolved invoke class resolution hook for it
					// A class is fully resolved if it has a known super
					if let Some(_) = *new_class.sup.borrow() {
						self.invoke_class_resolution_hook(new_class.name.clone(), new_class.clone());
					}

					Ok(BuilderResult::Normal)
				}
			}
			BuilderState::Second => {
				use crate::decaf::Scope::*;
				use BuilderResult::*;
				// get decaf class node
				if let Some(cls) = self.class_lookup(cls_node.name.clone()) {
					self.scopes.push(ClassScope(cls.clone()));

					for member in cls_node.member_list.iter() {
						match member {
							lnp::past::Member::FieldMember(field) => field.accept(self)?,
							lnp::past::Member::MethodMember(method) => method.accept(self)?,
							lnp::past::Member::CtorMember(ctor) => ctor.accept(self)?,
						};
					}

					self.scopes.pop();

					Ok(Normal)
				} else {
					panic!("class not defined in the first pass")
				}
				// visit each method
			}
		}
	}

	fn visit_field(&mut self, field_node: &lnp::past::Field) -> Self::Result {
		use crate::decaf::SemanticError::*;
		use crate::decaf::Type;
		use self::BuilderState::*;
		use self::BuilderResult::*;
		use crate::decaf::TypeBase::*;
		use crate::lnp::past::Modifier::*;

		match &self.state {
			First => {
				// Normalize field
				for norm_field in field_node.normalize() {
					let (ty, array_lvl, field_name, init_expr) = norm_field;

					// Get current class
					let curr_cls = self.get_curr_scope_as_class();

					// Check name conflict
					if curr_cls.pub_fields.borrow().iter().any(|f| f.name == field_name) ||
						curr_cls.prot_fields.borrow().iter().any(|f| f.name == field_name) ||
						curr_cls.priv_fields.borrow().iter().any(|f| f.name == field_name) {
							panic!("field name conflict");
						}

					// Create field node
					let field = Rc::new(crate::decaf::Field::new(field_name));
					let tmp = field.clone();

					// First check if we have change to reuse type
					if let Some(field_type) = self.type_add(ty,
												   array_lvl,
												   Some(Box::new(
													   move |cls_ty| {
														   tmp.clone().set_base_type(Rc::new(Type{
															   base: ClassTy(cls_ty),
															   array_lvl: array_lvl,
														   }));
														   None
													   }
												   ))) {
						field.set_base_type(Rc::new(field_type));
					}

					// Only allow 0/1 modifier and if it's pub/prot/priv
					match (field_node.modies.len(), field_node.modies.last()) {
						(1, Some(modifier)) => {
							match modifier {
								ModPublic(_) => {
									curr_cls.pub_fields.borrow_mut().push(field);
								}
								ModPrivate(_) => {
									curr_cls.priv_fields.borrow_mut().push(field);
								}
								ModProtected(_) => {
									curr_cls.prot_fields.borrow_mut().push(field);
								}
								ModStatic(_) => panic!("static field not allowed"),
							};
						},
						(0, None) => {
							// Default to public fields
							curr_cls.pub_fields.borrow_mut().push(field);
						}
						_ => {
							return Err(InvalidModifier(field_node.span));
						}
					};
				}
				Ok(BuilderResult::Normal)
			},
			Second => {
				// Get current class
				let cls = self.get_curr_scope_as_class();

				// Visit the expression if there's any
				for norm_field in field_node.normalize() {
					let (ty, array_lvl, field_name, init_expr) = norm_field;
					let field = cls.get_field_by_name(&field_name).unwrap();
					if let Some(init_expr) = init_expr {
						init_expr.accept(self)?;
					}
				}
				Ok(Normal)
			}
		}
	}

	fn visit_method(&mut self, mthd: &lnp::past::Method) -> Self::Result {
		use crate::decaf::SemanticError::*;
		use self::BuilderState::*;
		use self::BuilderResult::*;
		use crate::decaf::Type;
		use crate::decaf::TypeBase::*;
		use crate::lnp::past::Modifier::*;
		use crate::decaf::Visibility::*;
		use crate::decaf::Scope::*;
		match self.state {
			First => {
				// Normalize method
				let (return_ty, array_lvl, name, args, block) = mthd.normalize();

				// Get current class
				let curr_cls = self.get_curr_scope_as_class();

				// Create method node
				let method_node = Rc::new(crate::decaf::Method::new(name, curr_cls.clone()));
				let tmp = method_node.clone();

				// Get return type
				if let Some(return_ty) = self.type_add(return_ty,
													   array_lvl,
													   Some(Box::new(
														   move |cls_ty| {
															   *tmp.clone().return_ty.borrow_mut() = Rc::new(Type {
																   base: ClassTy(cls_ty),
																   array_lvl: array_lvl,
															   });
															   None
														   }
													   ))) {
					*method_node.return_ty.borrow_mut() = Rc::new(return_ty);
				}

				// Get arg types
				for (arg_name, arg_ty, arg_array_lvl) in args.into_iter() {
					let tmp = method_node.clone();
					let tmp_name = arg_name.clone();
					if let Some(arg_ty) = self.type_add(arg_ty,
														arg_array_lvl,
														Some(Box::new(
															move |cls_ty| {
																tmp.set_arg_type(tmp_name.clone(), Rc::new(Type {
																	base: ClassTy(cls_ty),
																	array_lvl: array_lvl,
																}));
																None
															}
														))) {
						method_node.add_arg(arg_name, Rc::new(arg_ty));
					}
				}

				// TODO: Set block

				// Check name conflict
				if curr_cls.pub_methods.borrow().iter().any(|m| m.has_same_signature(method_node.as_ref())) {
					panic!("method redefinition")
				}

				match (mthd.modies.len(), mthd.modies.last()) {
					(1, Some(modifier)) => {
						match modifier {
							ModPublic(_) => {
								*method_node.vis.borrow_mut() = Pub;
								curr_cls.pub_methods.borrow_mut().push(method_node);
							}
							ModPrivate(_) => {
								*method_node.vis.borrow_mut() = Priv;
								curr_cls.priv_methods.borrow_mut().push(method_node);
							}
							ModProtected(_) => {
								*method_node.vis.borrow_mut() = Prot;
								curr_cls.prot_methods.borrow_mut().push(method_node);
							}
							ModStatic(_) => {
								*method_node.vis.borrow_mut() = Pub;
								*method_node.stat.borrow_mut() = true;
								curr_cls.pub_static_methods.borrow_mut().push(method_node);
							}
						};
					},
					(0, None) => {
						// Default to public fields
						*method_node.vis.borrow_mut() = Pub;
						curr_cls.pub_methods.borrow_mut().push(method_node);
					},
					(2, _) => {
						// public/private/protected + static
						match mthd.modies[0] {
							ModPublic(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Pub;
									*method_node.stat.borrow_mut() = true;
									curr_cls.pub_static_methods.borrow_mut().push(method_node);
								} else {
									panic!("second modifer must be static");
								}
							},
							ModPrivate(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Priv;
									*method_node.stat.borrow_mut() = true;
									curr_cls.priv_static_methods.borrow_mut().push(method_node);
								} else {
									panic!("second modifer must be static");
								}
							},
							ModProtected(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Prot;
									*method_node.stat.borrow_mut() = true;
									curr_cls.prot_static_methods.borrow_mut().push(method_node);
								} else {
									panic!("second modifer must be static");
								}
							},
							_ => {
								panic!("first modifier must not be static (when there are two modifiers)");
							}
						}
					}
					_ => {
						return Err(InvalidModifier(mthd.span));
					}
				};
			},
			Second => {
				// Get current class
				let cls = self.get_curr_scope_as_class();

				// Normalize method
				let (return_ty, array_lvl, name, args, block) = mthd.normalize();
				let is_static = mthd.modies.iter().any(|x| {
					if let ModStatic(_) = x {
						true
					} else {
						false
					}
				});

				// Get current method
				let method = cls.get_method_by_signature(&return_ty, array_lvl, &args, is_static).unwrap();

				// Push new scope
				self.scopes.push(MethodScope(method));

				// Visit block
				mthd.block.accept(self)?;
			}
		};
		Ok(Normal)
	}

	fn visit_expr(&mut self, expr: &lnp::past::Expression) -> Self::Result {
		use crate::decaf::SemanticError::*;
		use self::BuilderState::*;
		use self::BuilderResult::*;
		use crate::lnp::past::Expr::*;
		use crate::lnp::past::BinOp::*;
		use crate::lnp::past::UnOp::*;
		use crate::lnp::past::Prim::*;
		use crate::lnp::past::PrimitiveType::*;
		use crate::lnp::past::Litr::*;
		use crate::lnp::past::NAExpr::*;
		use crate::lnp::past::NNAExpr::*;
		use crate::decaf::TypeBase::*;
		use crate::decaf::Scope::*;
		use crate::decaf::{Expr, AssignExpr, BinArithExpr, BinLogicalExpr, BinCmpExpr, UnArithExpr,
						   UnNotExpr, CreateArrayExpr, LiteralExpr, CreateObjExpr, SelfMethodCallExpr,
						   MethodCallExpr, SuperCallExpr, ArrayAccessExpr, FieldAccessExpr,
						   VariableExpr};
		use crate::decaf::LiteralExpr::*;
		use crate::decaf::Expr::*;
		let scope = self.get_curr_scope();
		match expr.expr {
			BinaryExpr(left_expr, binop, right_expr) => {
				match (right_expr.accept(self), left_expr.accept(self)) {
					(Ok(rhs), Ok(lhs)) => {
						let (lhs, rhs) = {
							match (lhs, rhs) {
								(ExprNode(lhs), ExprNode(rhs)) => (lhs, rhs),
								_ => return Err(E_EXPRNODE_NOT_FOUND)
							}
						};
						// TODO : Ord trait for Type
						match lhs.get_type().is_compatible(rhs.get_type()) {
							true => {
								match binop {
									AssignOp => {
										// Check if lhs is addressable
										if lhs.addressable() {
											Ok(ExprNode(Assign(AssignExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
											})))
										} else {
											Err(E_LHS_NOT_ADDRESSABLE)
										}
									}
									PlusOp | MinusOp | TimesOp | DivideOp | ModOp => {
										if lhs.get_type().is_arith_type() {
											Ok(ExprNode(BinArith(BinArithExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop,
											})))
										} else {
											Err(E_EXPR_NOT_SUPPORT_ARITH_OP)
										}
									}
									LogicalOrOp | LogicalAndOp | EqualsOp | NotEqualsOp => {
										if lhs.get_type().is_logical_type() {
											Ok(ExprNode(BinLogical(BinLogicalExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop,
											})))
										} else {
											Err(E_EXPR_NOT_SUPPORT_LOGICAL_OP)
										}
									}
									_ => {
										if lhs.get_type().is_cmp_type() {
											Ok(ExprNode(BinCmp(BinCmpExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop,
											})))
										} else {
											Err(E_EXPR_NOT_SUPPORT_CMP_OP)
										}
									}
								}
							}
							false => Err(E_EXPR_NOT_COMPATIBLE_TYPE)
						}
					}
					(Err(e), _) | (_, Err(e))=> Err(e)
				}
			},
			UnaryExpr(unop, right_expr) => {
				match right_expr.accept(self) {
					Ok(rhs) => {
						let rhs = match rhs {
							ExprNode(rhs) => rhs,
							_ => Err(E_EXPRNODE_NOT_FOUND)
						};
						match unop {
							PlusUOp | MinusUOp => {
								if rhs.get_type().is_arith_type() {
									Ok(ExprNode(UnArith(UnArithExpr {
										rhs: Box::new(rhs),
										op: unop,
									})))
								} else {
									Err(E_EXPR_NOT_SUPPORT_ARITH_OP)
								}
							}
							NotUOp => {
								if rhs.get_type().is_logical_type() {
									Ok(ExprNode(UnNot(UnNotExpr {
										rhs: Box::new(rhs)
									})))
								} else {
									Err(E_EXPR_NOT_SUPPORT_LOGICAL_OP)
								}
							}
						}
					}
					Err(e) => Err(e)
				}
			},
			PrimaryExpr(prim) => {
				match prim.prim {
					NewArray(naexpr) => {
						match naexpr.expr {
							NewCustom(id, dims) => {
								match dims.accept(self) {
									Ok(dim_expr) => {
										let dim_expr = match dim_expr {
											ExprNode(dim_expr) => dim_expr,
											_ => Err(E_EXPRNODE_NOT_FOUND),
										};
										if dim_expr.get_type().is_arith_type() {
											// lookup class for id
											if let Some(cls) = self.class_lookup(id.clone()) {
												Ok(ExprNode(CreateArray(CreateArrayExpr {
													ty: ClassTy(cls),
													array_lvl_expr: Box::new(dim_expr),
												})))
											} else {
												Err(E_CLASS_NOT_DEFINED)
											}
										} else {
											Err(E_DIM_EXPR_NOT_ARITH_TYPE)
										}
									}
									Err(e) => Err(e)
								}
							}
							NewPrimitive(primtype, dims) => {
								match dims.accept(self) {
									Ok(dim_expr) => {
										let dim_expr = match dim_expr {
											ExprNode(dim_expr) => dim_expr,
											_ => Err(E_EXPRNODE_NOT_FOUND),
										};
										if dim_expr.get_type().is_arith_type() {
											match primtype {
												BoolType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
													ty: BoolTy,
													array_lvl_expr: Box::new(dim_expr),
												}))),
												IntType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
													ty: IntTy,
													array_lvl_expr: Box::new(dim_expr),
												}))),
												CharType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
													ty: CharTy,
													array_lvl_expr: Box::new(dim_expr),
												}))),
												VoidType(_) => Err(E_VOID_ARRAY)
											}
										} else {
											Err(E_DIM_EXPR_NOT_ARITH_TYPE)
										}
									}
									Err(e) => Err(e)
								}
							}
						}
					}
					NonNewArray(nnaexpr) => {
						match nnaexpr.expr {
							JustLit(litr) => {
								match litr.litr {
									Null => Ok(ExprNode(NULL)),
									Bool(b) => Ok(ExprNode(Literal(BoolLiteral(b)))),
									Char(c) => Ok(ExprNode(Literal(CharLiteral(c)))),
									Str(s) => Ok(ExprNode(Literal(StrLiteral(s)))),
								}
							}
							JustThis => Ok(ExprNode(This)),
							JustExpr(expr) => expr.accept(self),
							NewObj(id, args) => {
								if let Some(cls) = self.class_lookup(id.clone()) {
									let arg_exprs = vec![];
									for arg in args.iter() {
										match arg.accept(self) {
											Ok(arg_expr) => {
												let arg_expr = match arg_expr {
													ExprNode(arg_expr) => arg_expr,
													_ => return Err(E_EXPRNODE_NOT_FOUND)
												};
												arg_exprs.push(arg_expr);
											},
											Err(e) => return Err(e)
										}
									}
									// Check if signature matches with any of the constructors
									// TODO: create a get_visible_ctors() function to tidy things up
									match cls.get_compatible_pub_ctor(&arg_exprs) {
										Some(ctor) => {
											Ok(ExprNode(CreateObj(CreateObjExpr {
												cls: cls,
												ctor: ctor,
												args: arg_exprs,
											})))
										},
										None => {
											let in_class_scope = self.has_class_in_scope_stack(&cls);
											if in_class_scope {
												match cls.get_compatible_prot_ctor(&arg_exprs) {
													Some(ctor) => Ok(ExprNode(CreateObj(CreateObjExpr {
														cls: cls,
														ctor: ctor,
														args: arg_exprs,
													}))),
													None => match cls.get_compatible_priv_ctor(&arg_exprs) {
														Some(ctor) => Ok(ExprNode(CreateObj(CreateObjExpr {
															cls: cls,
															ctor: ctor,
															args: arg_exprs,
														}))),
														None => Err(E_NO_COMPATIBLE_CTOR)
													}
												}
											} else {
												Err(E_NO_COMPATIBLE_CTOR)
											}
										}
									}
								} else {
									Err(E_CLASS_NOT_DEFINED)
								}
							}
							CallSelfMethod(id, args) => {
								let arg_exprs = vec![];
								for arg in args.iter() {
									match arg.accept(self) {
										Ok(arg_expr) => {
											let arg_expr = match arg_expr {
												ExprNode(arg_expr) => arg_expr,
												_ => return Err(E_EXPRNODE_NOT_FOUND)
											};
											arg_exprs.push(arg_expr);
										},
										Err(e) => return Err(e)
									}
								}

								// Lookup the method
								let mlookup = |cls| {
									match cls.get_compatible_pub_method(&arg_exprs) {
										Some(method) => Ok(ExprNode(SelfMethodCall(SelfMethodCallExpr {
											cls: cls,
											method: method,
											args: arg_exprs,
										}))),
										None => match cls.get_compatible_prot_method(&arg_exprs) {
											Some(method) => Ok(ExprNode(SelfMethodCall(SelfMethodCallExpr {
												cls: cls,
												method: method,
												args: arg_exprs,
											}))),
											None => match cls.get_compatible_priv_method(&arg_exprs) {
												Some(method) => Ok(ExprNode(SelfMethodCall(SelfMethodCallExpr {
													cls: cls,
													method: method,
													args: arg_exprs,
												}))),
												None => Err(E_NO_COMPATIBLE_METHOD)
											}
										}
									}
								}
								for scope in self.scopes.iter().rev() {
									match scope {
										ClassScope(cls) => if let Ok(expr_node) = mlookup(cls.clone()) {
											return Ok(expr_node);
										} else {
											continue;
										}
									}
								}
								Err(E_NO_COMPATIBLE_METHOD)
							}
							CallMethod(nprim, id, args) => {

							}
							CallSuper(id, args) => {}
							EvalArray(arrexpr) => {}
							EvalField(fldexpr) => {}
						}
					}
					Identifier(id) => {

					}
				}
			}
		}
		Ok(BuilderResult::Normal)
	}

	fn visit_block(&mut self, block: &lnp::past::Block) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_ctor(&mut self, ctor: &lnp::past::Ctor) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_formalarg(&mut self, farg: &lnp::past::FormalArg) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_type(&mut self, ty: &lnp::past::Type) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_vardecl(&mut self, vardecl: &lnp::past::VarDeclarator) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_stmt(&mut self, stmt: &lnp::past::Statement) -> Self::Result {
		Ok(BuilderResult::Normal)
	}

	fn visit_declare_stmt(&mut self, ty: &lnp::past::Type,
						  vardecls: &Vec<lnp::past::VarDeclarator>) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_if_stmt(&self, condexpr: &lnp::past::Expression,
					 thenstmt: &Box<lnp::past::Statement>) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_if_else_stmt(&self, condexpr: &lnp::past::Expression,
						  thenstmt: &Box<lnp::past::Statement>,
						  elsestmt: &Box<lnp::past::Statement>) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_while_stmt(&self, condexpr: &lnp::past::Expression,
						loopstmt: &Box<lnp::past::Statement>) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_super_stmt(&self, stmt: &lnp::past::Statement) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_primary(&self, prim: &lnp::past::Primary) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_naexpr(&self, expr: &lnp::past::NewArrayExpr) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_nnaexpr(&self, expr: &lnp::past::NNAExpr) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_new_obj(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_call_self_method(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_call_method(&self, prim: &lnp::past::Primary, name: &String, args: &lnp::past::ActualArgs) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_call_super(&self, name: &String, args: &lnp::past::ActualArgs) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_array_expr(&self, expr: &lnp::past::ArrayExpr) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_field_expr(&self, expr: &lnp::past::FieldExpr) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
	fn visit_actural_args(&self, args: &lnp::past::ActualArgs) -> Self::Result {
		Ok(BuilderResult::Normal)
	}
}
