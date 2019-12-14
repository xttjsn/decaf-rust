use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::decaf;
use crate::decaf::{ControlFlow, Value, VTable, Scope::*, Expr::*, AssignExpr, BinArithExpr, UnArithExpr, BinLogicalExpr, UnNotExpr, BinCmpExpr, CreateArrayExpr, ThisExpr, CreateObjExpr, MethodCallExpr, SuperCallExpr, ArrayAccessExpr, FieldAccessExpr, VariableExpr, ClassIdExpr, Type, TypeBase::*, Visibility::*, LiteralExpr::*, Stmt::*, Ctor};

use SemanticError::*;
use BuilderState::*;
use BuilderResult::*;

use crate::lnp;
use lnp::past::{Modifier::*, Litr::*, NNAExpr::*, AExpr::*, FExpr::*, NAExpr::*, Prim::*,
				PrimitiveType::*, Stmt::*, Expr::*, BinOp::*, UnOp::*};

use crate::codegen::LLVMName;
use crate::lnp::past::Primary;

macro_rules! debug {
	() => (println!());
	($($arg:tt)*) => ({
		println!("DEBUG");
		println!($($arg)*);
		println!("END DEBUG")})
}

pub trait Visitor {
	type Result;

	fn visit_program(&mut self, prog: &lnp::past::Program) -> Self::Result;
	fn visit_class(&mut self, cls: &lnp::past::ClassNode) -> Self::Result;
	fn visit_field(&mut self, fld: &lnp::past::Field) -> Self::Result;
	fn visit_method(&mut self, mthd: &lnp::past::Method) -> Self::Result;
	fn visit_expr(&mut self, expr: &lnp::past::Expression) -> Self::Result;
	fn visit_ctor(&mut self, ctor: &lnp::past::Ctor) -> Self::Result;
	fn visit_stmt(&mut self, stmt: &lnp::past::Statement) -> Self::Result;
	fn visit_block(&mut self, block: &lnp::past::Block) -> Self::Result;
	fn visit_dims(&mut self, block: &Vec<lnp::past::Dimension>) -> Self::Result;
	fn visit_primary(&mut self, prim: &lnp::past::Primary) -> Self::Result;
	fn visit_nnaexpr(&mut self, expr: &lnp::past::NNAExpr) -> Self::Result;
	fn visit_actural_args(&mut self, args: &lnp::past::ActualArgs) -> Self::Result;
	fn visit_this(&mut self) -> Self::Result;
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

impl Visitable for lnp::past::Statement {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_stmt(self) }
}

impl Visitable for lnp::past::Expression {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_expr(self) }
}

impl Visitable for lnp::past::Block {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_block(self) }
}

impl Visitable for lnp::past::NNAExpr {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_nnaexpr(self) }
}

impl Visitable for lnp::past::ActualArgs {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_actural_args(self) }
}

impl Visitable for lnp::past::Primary {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_primary(self) }
}

impl Visitable for Vec<lnp::past::Dimension> {
	fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result { visitor.visit_dims(self) }
}

// Errors
#[derive(Debug)]
pub enum SemanticError {
	EExprNodeNotFound(String),
	ELhsNotAddressable,
	EExprNotSupportArithOp,
	EExprNotSupportLogicalOp,
	EExprNotSupportCmpOp,
	EExprNotCompatibleType,
	EVariableRedefinition,
	EBlockStmtNotFound(String),
	EStmtOrVecStmtNotFound,
	ECondNotLogicalType,
	EStmtNodeNotFound,
	EUnmatchedReturnType(String),
	EReturnInNonMethodScope,
	EContinueNotInLoop,
	EBreakNotInLoop,
	ECallingCtorOutsideClass,
	EVecExprNotFound(String),
	ECallingSuperCtorOutsideCtor,
	EClassNotDefined,
	EDimExprNotArithType,
	EVoidArray,
	ENoCompatibleCtor,
	ENoCompatibleMethod(String),
	EArrayTypeNotSupportMethodCall,
	ENonClassTypeNotSupportMethodCall,
	ESuperCallWithoutClass,
	EArrayAccessOnNonArrayType,
	ENoCompatibleField,
	EArrayTypeNoField,
	EPrimitiveTypeNoField,
	EFieldAccessWithoutClass,
	EClassRedefinition,
	EInvalidModifier,
	EThisInStaticMethod,
	EThisWithoutClass,
	EThisWithoutMethod,
	EThisNotFound,
	EMethodCallWithoutClass,
	EUnknownIdentifier(String),
	EMissingReturn,
	EUninitializedArray,
	EArrayNegativeLevel,
	EMethodCallWithoutMethod,
	ENotInClassScope,
}

trait Normalize {
	type Result;
	fn normalize(&self) -> Self::Result;
}

trait ASTBuilder {
	type Type;
	type TypeBase;
	type ClassHook;
	fn type_add(&mut self, base_ty: Self::TypeBase, array_lvl: u32, hook: Option<Self::ClassHook>) -> Option<Self::Type>;
	fn class_lookup(&self, cls_name: String) -> Option<Rc<decaf::Class>>;
	fn class_add(&mut self, cls_name: String, cls: Rc<decaf::Class>);
	fn add_class_resolution_hook(&mut self, cls_name: String, f: Self::ClassHook);
	fn invoke_class_resolution_hook(&mut self, cls_name: String, cls: Rc<decaf::Class>);
	fn get_curr_scope_as_class(&self) -> Rc<decaf::Class>;
	fn has_class_in_scope_stack(&self, cls: &Rc<decaf::Class>) -> bool;
	fn variable_lookup(&self, name: String) -> Option<Rc<decaf::Variable>>;
	fn variable_add(&self, name: String, ty: decaf::Type);
	fn get_top_most_method_scope(&self) -> Option<decaf::Scope>;
	fn get_top_most_while_scope(&self) -> Option<decaf::Scope>;
	fn get_top_most_class_scope(&self) -> Option<decaf::Scope>;
	fn get_top_most_ctor_scope(&self) -> Option<decaf::Scope>;
	fn load_builtin(&mut self);
}

impl Normalize for lnp::past::Method {
	type Result = (decaf::TypeBase, u32, String, Vec<(String, decaf::TypeBase, u32)>, lnp::past::Block);

	fn normalize(&self) -> Self::Result {
		use lnp::past::VarDeclaratorIdInner::*;
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
				Array(id) => {
					// Resolve the array_lvl
					let mut curr_lvl = 1;
					let mut curr_id = id;
					loop {
						match &curr_id.id {
							Single(id_s) => {
								args.push((id_s.clone(), arg_type, curr_lvl));
								break;
							}
							Array(id_a) => {
								curr_id = id_a;
							}
						}
						curr_lvl += 1;
					}
				}
			}
		}

		(return_type, array_lvl, self.name.clone(), args, self.block.clone())
	}
}

impl Normalize for lnp::past::Ctor {
	type Result = (Vec<(String, decaf::TypeBase, u32)>, lnp::past::Block);

	fn normalize(&self) -> Self::Result {
		use lnp::past::VarDeclaratorIdInner::*;
		let mut args = vec![];
		for arg in self.fargs.farg_list.iter() {
			let (arg_array_lvl, arg_type) = ty2ty(&arg.ty, 0);
			match &arg.var_decl_id.id {
				Single(arg_name) => {
					args.push((arg_name.clone(), arg_type, arg_array_lvl));
				},
				Array(id) => {
					// Resolve the array_lvl
					let mut curr_lvl = 1;
					let mut curr_id = id;
					loop {
						match &curr_id.id {
							Single(id_s) => {
								args.push((id_s.clone(), arg_type, curr_lvl));
								break;
							}
							Array(id_a) => {
								curr_id = id_a;
							}
						}
						curr_lvl += 1;
					}
				}
			}
		}

		(args, self.block.clone())
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

fn ty2ty2<B: ASTBuilder>(ty: &lnp::past::Type, lvl: u32, builder: &B) -> decaf::Type {
	use decaf::TypeBase::*;
	// Count array nesting level from type
	match ty {
		lnp::past::Type::Primitive(prim) => {
			match prim {
				lnp::past::PrimitiveType::BoolType(_) => decaf::Type {
					base: decaf::TypeBase::BoolTy,
					array_lvl: lvl,
				},
				lnp::past::PrimitiveType::CharType(_) => decaf::Type {
					base: decaf::TypeBase::CharTy,
					array_lvl: lvl,
				},
				lnp::past::PrimitiveType::IntType(_) => decaf::Type {
					base: decaf::TypeBase::IntTy,
					array_lvl: lvl,
				},
				lnp::past::PrimitiveType::VoidType(_) => match lvl {
					0 => decaf::Type {
						base: decaf::TypeBase::VoidTy,
						array_lvl: 0,
					},
					_ => panic!("void type cannot be array")
				}
			}
		}
		lnp::past::Type::Custom(cls_name) => {
			// Lookup class
			match builder.class_lookup(cls_name.clone()) {
				Some(cls) => decaf::Type {
					base: ClassTy(cls),
					array_lvl: lvl,
				},
				None => panic!("class {} not found", cls_name)
			}
		}
		lnp::past::Type::Array(ty) => ty2ty2(&*ty, lvl + 1, builder),
	}
}

pub enum BuilderState {
	First, // Symbol resolution
	Second,
}

#[derive(PartialEq, Debug)]
pub enum BuilderResult {
	Normal,
	ExprNode(decaf::Expr),
	VecExpr(Vec<decaf::Expr>),
    StmtNode(decaf::Stmt),
    VecStmt(Vec<decaf::Stmt>),
}

type DecafClassHook = Box<dyn FnMut(Rc<decaf::Class>) -> Option<Rc<decaf::Class>>>;

pub struct Program {
	pub classes: Vec<Rc<decaf::Class>>,
}

pub struct DecafTreeBuilder {
	pub state: BuilderState,
	pub scopes: Vec<decaf::Scope>,
	pub class_hooks: BTreeMap<String, Vec<DecafClassHook>>,
	pub class_map: BTreeMap<String, Rc<decaf::Class>>,
}

impl DecafTreeBuilder {
	#[allow(dead_code)]
	pub fn new() -> Self {
		DecafTreeBuilder {
			state: BuilderState::First,
			scopes: vec![],
			class_hooks: BTreeMap::new(),
			class_map: BTreeMap::new(),
		}
	}
}

impl ASTBuilder for DecafTreeBuilder {
	type Type = decaf::Type;
	type TypeBase = decaf::TypeBase;
	type ClassHook = DecafClassHook;

	fn type_add(&mut self,
				base_ty: decaf::TypeBase,
				array_lvl: u32,
				hook: Option<Self::ClassHook>) -> Option<decaf::Type> {
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
									array_lvl,
								})
							}
						}
					},
					BoolTy | IntTy | CharTy  => Some(Type {
						base: base_ty,
						array_lvl,
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
						array_lvl,
					}),
					NULLTy => Some(Type {
						base: NULLTy,
						array_lvl,
					}),
					StrTy => Some(Type {
						base: StrTy,
						array_lvl,
					})
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

	fn add_class_resolution_hook(&mut self, cls_name: String, f: Self::ClassHook) {
		self.class_hooks.entry(cls_name).or_insert(vec![]).push(f);
	}

	fn invoke_class_resolution_hook(&mut self, cls_name: String, cls: Rc<decaf::Class>) {
		fn go(class_hooks: &mut BTreeMap<String, Vec<DecafClassHook>>,
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
		use crate::decaf::Scope::*;
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

	fn variable_lookup(&self, name: String) -> Option<Rc<decaf::Variable>> {
		for scope in self.scopes.iter().rev() {
			match scope.variable_lookup(&name) {
				Some(v) => {
					return Some(v);
				}
				None => continue,
			}
		}
		None
	}

	fn variable_add(&self, name: String, ty: decaf::Type) {
		use crate::decaf::Scope::*;
		// The top most scope must be a block scope
		match self.scopes.last() {
			Some(BlockScope(_)) | Some(CtorScope(_)) | Some(MethodScope(_)) => {
				self.scopes.last().unwrap().variable_add(name, ty);
			}
			_ => {
				panic!("top most scope must be a block/ctor/method scope when adding variable");
			}
		}
	}

	fn get_top_most_method_scope(&self) -> Option<decaf::Scope> {
		use crate::decaf::Scope::*;
		for scope in self.scopes.iter().rev() {
			match scope {
				MethodScope(_) => {
					return Some(scope.clone());
				},
				_ => continue,
			}
		}
		None
	}

	fn get_top_most_while_scope(&self) -> Option<decaf::Scope> {
		use crate::decaf::Scope::*;
		for scope in self.scopes.iter().rev() {
			match scope {
				WhileScope(_) => {
					return Some(scope.clone());
				},
				_ => continue,
			}
		}
		None
	}

	fn get_top_most_class_scope(&self) -> Option<decaf::Scope> {
		use crate::decaf::Scope::*;
		for scope in self.scopes.iter().rev() {
			match scope {
				ClassScope(_) => {
					return Some(scope.clone());
				},
				_ => continue,
			}
		}
		None
	}

	fn get_top_most_ctor_scope(&self) -> Option<decaf::Scope> {
		use crate::decaf::Scope::*;
		for scope in self.scopes.iter().rev() {
			match scope {
				CtorScope(_) => {
					return Some(scope.clone());
				},
				_ => continue,
			}
		}

		None
	}

	fn load_builtin(&mut self) {
		use decaf::{Class, Method, TypeBase::*};
		let objcls = Rc::new(Class::new("Object".to_string()));
		let strcls = Rc::new(Class::new("String".to_string()));
		let iocls = Rc::new(Class::new("IO".to_string()));

		// No default methods for String
		*strcls.sup.borrow_mut() = Some(objcls.clone());
//		{
//			let str_inner_fld = Field::new("str".to_string());
//			let str_len_fld = Field::new("len".to_string());
//			strcls.priv_fields.borrow_mut().push(Rc::new(str_inner_fld));
//			// TODO: add LLVM string valueo
//		}

		// putChar, putString, putInt, getChar, getInt, getLine, peek()
		*iocls.sup.borrow_mut() = Some(objcls.clone());
		{
			let put_char = Method::new("putChar".to_string(), iocls.clone());
			*put_char.stat.borrow_mut() = true;
			*put_char.return_ty.borrow_mut() = Rc::new(Type {
				base: VoidTy,
				array_lvl: 0,
			});
			*put_char.args.borrow_mut() = vec![("char".to_string(), Rc::new(Type {
				base: CharTy,
				array_lvl: 0,
			}))];

			let put_string = Method::new("putString".to_string(), iocls.clone());
			*put_string.stat.borrow_mut() = true;
			*put_string.return_ty.borrow_mut() = Rc::new(Type {
				base: VoidTy,
				array_lvl: 0,
			});
			*put_string.args.borrow_mut() = vec![("str".to_string(), Rc::new(Type {
				base: ClassTy(strcls.clone()),
				array_lvl: 0,
			}))];

			let put_int = Method::new("putInt".to_string(), iocls.clone());
			*put_int.stat.borrow_mut() = true;
			*put_int.return_ty.borrow_mut() = Rc::new(Type {
				base: VoidTy,
				array_lvl: 0,
			});
			*put_int.args.borrow_mut() = vec![("int".to_string(), Rc::new(Type {
				base: IntTy,
				array_lvl: 0,
			}))];

			let get_char = Method::new("getChar".to_string(), iocls.clone());
			*get_char.stat.borrow_mut() = true;
			*get_char.return_ty.borrow_mut() = Rc::new(Type {
				base: CharTy,
				array_lvl: 0,
			});

			let get_line = Method::new("getLine".to_string(), iocls.clone());
			*get_line.stat.borrow_mut() = true;
			*get_line.return_ty.borrow_mut() = Rc::new(Type {
				base: ClassTy(strcls.clone()),
				array_lvl: 0,
			});

			let get_int = Method::new("getInt".to_string(), iocls.clone());
			*get_int.stat.borrow_mut() = true;
			*get_int.return_ty.borrow_mut() = Rc::new(Type {
				base: IntTy,
				array_lvl: 0,
			});

			let peek = Method::new("peek".to_string(), iocls.clone());
			*peek.stat.borrow_mut() = true;
			*peek.return_ty.borrow_mut() = Rc::new(Type {
				base: CharTy,
				array_lvl: 0,
			});

			(*iocls.pub_static_methods.borrow_mut()).extend(vec![Rc::new(put_char), Rc::new(put_string),
																 Rc::new(put_int), Rc::new(get_char),
																 Rc::new(get_line), Rc::new(get_int),
																 Rc::new(peek)]);
		}

		self.class_map.insert("Object".to_string(), objcls);
		self.class_map.insert("String".to_string(), strcls);
		self.class_map.insert("IO".to_string(), iocls);

		// TODO: reset scopes
	}
}

impl Visitor for DecafTreeBuilder {
	type Result = Result<BuilderResult, SemanticError>;

	fn visit_program(&mut self, prog: &lnp::past::Program) -> Self::Result {
		self.load_builtin();

		// First pass to collect class list
		self.state = BuilderState::First;
		for cls in prog.classes.iter() {
			cls.accept(self)?;
		}

		// Second pass to start parsing
		 self.state = BuilderState::Second;
		 for cls in prog.classes.iter() {
		 	cls.accept(self)?;
		 }

		Ok(BuilderResult::Normal)
	}

	fn visit_class(&mut self, cls_node: &lnp::past::ClassNode) -> Self::Result {
		match self.state {
			BuilderState::First => {
				// Check for class name conflicts
				if let Some(_) = self.class_lookup(cls_node.name.clone()) {
					return Err(EClassRedefinition);
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

					// Define default constructor
					let has_default_ctor = {
						let mut b = false;
						for member in cls_node.member_list.iter() {
							match member {
								lnp::past::Member::FieldMember(_) => continue,
								lnp::past::Member::MethodMember(_) => continue,
								lnp::past::Member::CtorMember(ctor) => {
									if ctor.fargs.farg_list.len() == 0 {
										b = true;
										break;
									}
								}
							};
						}
						b
					};
					if !has_default_ctor {
						let default_ctor = Rc::new(decaf::Ctor::new(new_class.clone()));
						self.scopes.push(CtorScope(default_ctor.clone()));
						self.variable_add("this".into(), Type {
							base: ClassTy(new_class.clone()),
							array_lvl: 0,
						});
						*default_ctor.body.borrow_mut() = Some(
							Rc::new(decaf::BlockStmt {
								vartbl: RefCell::new(vec![]),
								stmts: RefCell::new(vec![Return(decaf::ReturnStmt{
									expr: Some(This(decaf::ThisExpr {
										cls: new_class.clone(),
									}))
								})])
							})
						);
						self.scopes.pop();
						new_class.pub_ctors.borrow_mut().push(default_ctor);
					}

					println!("trying to visit class member");
					for member in cls_node.member_list.iter() {
						match member {
							lnp::past::Member::FieldMember(field) => field.accept(self)?,
							lnp::past::Member::MethodMember(method) => method.accept(self)?,
							lnp::past::Member::CtorMember(ctor) => ctor.accept(self)?,
						};
					}

					self.scopes.pop();

					// Report vtable
					for (ix, method) in new_class.vtable.borrow().iter().enumerate() {
						println!("The {}-th method in {}'s vtable is {}", ix, new_class.name, method.name);
					}

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

					// Add all fields as variable
//					for field in cls.pub_fields.borrow().iter() {
//						self.variable_add(field.name.clone(), decaf::Type {
//							base: field.ty.borrow().base.clone(),
//							array_lvl: field.ty.borrow().array_lvl
//						});
//					}
//					for field in cls.prot_fields.borrow().iter() {
//						self.variable_add(field.name.clone(), decaf::Type {
//							base: field.ty.borrow().base.clone(),
//							array_lvl: field.ty.borrow().array_lvl
//						});
//					}
//					for field in cls.priv_fields.borrow().iter() {
//						self.variable_add(field.name.clone(), decaf::Type {
//							base: field.ty.borrow().base.clone(),
//							array_lvl: field.ty.borrow().array_lvl
//						});
//					}

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
		match &self.state {
			First => {
				// Normalize field
				for norm_field in field_node.normalize() {
					let (ty, array_lvl, field_name, _init_expr) = norm_field;

					// Get current class
					let curr_cls = self.get_curr_scope_as_class();

					// Check name conflict
					if curr_cls.pub_fields.borrow().iter().any(|f| f.name == field_name) ||
						curr_cls.prot_fields.borrow().iter().any(|f| f.name == field_name) ||
						curr_cls.priv_fields.borrow().iter().any(|f| f.name == field_name) {
							panic!("field name conflict");
						}

					// Create field node
					let field = Rc::new(crate::decaf::Field::new(field_name, Some(curr_cls.clone())));
					let tmp = field.clone();

					// First check if we have change to reuse type
					if let Some(field_type) = self.type_add(ty,
												   array_lvl,
												   Some(Box::new(
													   move |cls_ty| {
														   tmp.clone().set_base_type(Rc::new(Type{
															   base: ClassTy(cls_ty),
															   array_lvl,
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
							return Err(EInvalidModifier)
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
					let (_ty, _array_lvl, field_name, init_expr) = norm_field;
					let field = cls.get_field_by_name(&field_name).unwrap();
					if let Some(init_expr) = init_expr {
						match init_expr.accept(self) {
							Ok(ExprNode(init_expr)) => {
								field.set_init_expr(init_expr);
							},
							_ => return Err(EExprNodeNotFound(format!("init_expr of {}", field.name))),
						}
					}
				}
				Ok(Normal)
			}
		}
	}

	fn visit_method(&mut self, mthd: &lnp::past::Method) -> Self::Result {
		match self.state {
			First => {
				// Normalize method
				let (return_ty, array_lvl, name, args, _) = mthd.normalize();

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
																   array_lvl,
															   });
															   None
														   }
													   ))) {
					*method_node.return_ty.borrow_mut() = Rc::new(return_ty);
				}


				// Get arg types
				for (arg_name, arg_ty, arg_array_lvl) in args.into_iter() {
					println!("{} has arg {}", method_node.llvm_name(), arg_name);
					let tmp = method_node.clone();
					let tmp_name = arg_name.clone();
					if let Some(arg_ty) = self.type_add(arg_ty.clone(),
														arg_array_lvl,
														Some(Box::new(
															move |cls_ty| {
																tmp.set_arg_type(tmp_name.clone(), Rc::new(Type {
																	base: ClassTy(cls_ty),
																	array_lvl,
																}));
																None
															}
														))) {
						method_node.add_arg(arg_name, Rc::new(arg_ty.clone()));
					} else {
						// Type is unknown at this point of time
						method_node.add_arg(arg_name, Rc::new(Type {
							base: arg_ty.clone(),
							array_lvl: arg_array_lvl,
						}))
					}
				}

				// Check name conflict
				if curr_cls.local_methods.borrow().iter().any(|m| m.has_same_signature(method_node.as_ref())) {
					panic!("method redefinition")
				}

				curr_cls.local_methods.borrow_mut().push(method_node.clone());

				match (mthd.modies.len(), mthd.modies.last()) {
					(1, Some(modifier)) => {
						match modifier {
							ModPublic(_) => {
								*method_node.vis.borrow_mut() = Pub;
								curr_cls.pub_methods.borrow_mut().push(method_node.clone());
								curr_cls.vtable.borrow_mut().add_vmethod(&method_node);
							}
							ModPrivate(_) => {
								*method_node.vis.borrow_mut() = Priv;
								curr_cls.priv_methods.borrow_mut().push(method_node);
							}
							ModProtected(_) => {
								*method_node.vis.borrow_mut() = Prot;
								curr_cls.prot_methods.borrow_mut().push(method_node.clone());
								curr_cls.vtable.borrow_mut().add_vmethod(&method_node);
							}
							ModStatic(_) => {
								*method_node.vis.borrow_mut() = Pub;
								*method_node.stat.borrow_mut() = true;
								curr_cls.pub_static_methods.borrow_mut().push(method_node);
							}
						};
					},
					(0, None) => {
						// Default to public methods
						*method_node.vis.borrow_mut() = Pub;
						curr_cls.pub_methods.borrow_mut().push(method_node.clone());
						curr_cls.vtable.borrow_mut().add_vmethod(&method_node);
					},
					(2, _) => {
						// public/private/protected + static
						match mthd.modies[0] {
							ModPublic(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Pub;
									*method_node.stat.borrow_mut() = true;
									curr_cls.pub_static_methods.borrow_mut().push(method_node.clone());
								} else {
									panic!("second modifer must be static");
								}
							},
							ModPrivate(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Priv;
									*method_node.stat.borrow_mut() = true;
									curr_cls.priv_static_methods.borrow_mut().push(method_node.clone());
								} else {
									panic!("second modifer must be static");
								}
							},
							ModProtected(_) => {
								if let ModStatic(_) = mthd.modies[1] {
									*method_node.vis.borrow_mut() = Prot;
									*method_node.stat.borrow_mut() = true;
									curr_cls.prot_static_methods.borrow_mut().push(method_node.clone());
								} else {
									panic!("second modifer must be static");
								}
							},
							ModStatic(_) => {
								match mthd.modies[1] {
									ModPublic(_) => {
										*method_node.vis.borrow_mut() = Pub;
										*method_node.stat.borrow_mut() = true;
										curr_cls.pub_static_methods.borrow_mut().push(method_node.clone());
									}
									ModPrivate(_) => {
										*method_node.vis.borrow_mut() = Priv;
										*method_node.stat.borrow_mut() = true;
										curr_cls.priv_static_methods.borrow_mut().push(method_node.clone());
									}
									ModProtected(_) => {
										*method_node.vis.borrow_mut() = Prot;
										*method_node.stat.borrow_mut() = true;
										curr_cls.prot_static_methods.borrow_mut().push(method_node.clone());
									}
									_ => panic!("cannot have more than one static modifiers"),
								}
							}
						}
					}
					_ => {
						return Err(EInvalidModifier);
					}
				};
			},
			Second => {
				use decaf::Stmt::*;

				// Get current class
				let cls = self.get_curr_scope_as_class();

				// Normalize method
				let (return_ty, array_lvl, name, mut args, _block) = mthd.normalize();
				let is_static = mthd.modies.iter().any(|x| {
					if let ModStatic(_) = x {
						true
					} else {
						false
					}
				});

				// Resolve args
				args = args.into_iter().map(|(name, ty_base, array_lvl)| (name, match ty_base {
					UnknownTy(cls_name) => ClassTy(self.class_lookup(cls_name).unwrap()),
					_ => ty_base,
				}, array_lvl)).collect();
				let return_ty = match return_ty {
					UnknownTy(cls_name) => ClassTy(self.class_lookup(cls_name).unwrap()),
					_ => return_ty
				};

				// Get current method
				let method = match cls.get_method_by_signature(&return_ty, array_lvl, &args, is_static, &name) {
					Some(m) => m,
					None => panic!("fail to get method by signature: {:?}_{}_{:?}_{}_{}", return_ty, array_lvl, args, is_static, name),
				};

				// Push new scope
				self.scopes.push(MethodScope(method.clone()));

				// Add argument as variable
				self.variable_add("this".into(), Type {
					base: ClassTy(cls.clone()),
					array_lvl: 0
				});
				for (arg_name, arg_ty_base, arg_array_lvl) in args.iter() {
					let arg_ty = decaf::Type {
						base: arg_ty_base.clone(),
						array_lvl: *arg_array_lvl
					};
					self.variable_add(arg_name.clone(), arg_ty);
				}

				// Visit block
				let ret = match mthd.block.accept(self) {
					Ok(StmtNode(Block(blk))) => {
						match return_ty {
							VoidTy => {
								*method.body.borrow_mut() = Some(blk);
								Ok(Normal)
							}
							_ => {
								// check if the last statement is a return
								if let Some(rty) = Block(blk.clone()).returnable() {
									if rty.base == return_ty && rty.array_lvl == array_lvl {
										*method.body.borrow_mut() = Some(blk);
										Ok(Normal)
									}
									else {
										Err(EMissingReturn)
									}
								} else {
									Err(EMissingReturn)
								}
							}
						}
					}
					Ok(_) => {
						Err(EBlockStmtNotFound(format!("{}", method)))
					}
					Err(e) => Err(e)
				};

				self.scopes.pop();

				return ret;
			}
		};
		Ok(Normal)
	}

	fn visit_expr(&mut self, expr: &lnp::past::Expression) -> Self::Result {
		match &expr.expr {
			BinaryExpr(left_expr, binop, right_expr) => {
				match (right_expr.accept(self), left_expr.accept(self)) {
					(Ok(rhs), Ok(lhs)) => {
						let (lhs, rhs) = {
							match (lhs, rhs) {
								(ExprNode(lhs), ExprNode(rhs)) => (lhs, rhs),
								_ => return Err(EExprNodeNotFound(format!("lhs {:?} rhs", binop)))
							}
						};
						// TODO : Ord trait for Type
						match lhs.get_type().is_compatible_with(&rhs.get_type(), false) {
							true => {
								match binop.op {
									AssignOp => {
										// Check if lhs is addressapble
										if lhs.addressable() {
											Ok(ExprNode(Assign(AssignExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
											})))
										} else {
											Err(ELhsNotAddressable)
										}
									}
									PlusOp | MinusOp | TimesOp | DivideOp | ModOp => {
										if lhs.get_type().supports_arith_ops() {
											Ok(ExprNode(BinArith(BinArithExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop.op.clone(),
											})))
										} else {
											Err(EExprNotSupportArithOp)
										}
									}
									LogicalOrOp | LogicalAndOp => {
										if lhs.get_type().supports_logical_ops() {
											Ok(ExprNode(BinLogical(BinLogicalExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop.op.clone(),
											})))
										} else {
											Err(EExprNotSupportLogicalOp)
										}
									}
									GreaterThanOp | GreaterOrEqOp | LessThanOp | LessOrEqOp => {
										if lhs.get_type().supports_cmp_ops() {
											Ok(ExprNode(BinCmp(BinCmpExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop.op.clone(),
											})))
										} else {
											Err(EExprNotSupportCmpOp)
										}
									}
									EqualsOp | NotEqualsOp => {
										if lhs.get_type().supports_eq_ops() {
											Ok(ExprNode(BinCmp(BinCmpExpr {
												lhs: Box::new(lhs),
												rhs: Box::new(rhs),
												op: binop.op.clone(),
											})))
										} else {
											Err(EExprNotSupportCmpOp)
										}
									}
								}
							}
							false => Err(EExprNotCompatibleType)
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
							_ => return Err(EExprNodeNotFound(format!("UnaryExpr rhs")))
						};
						match unop.op {
							PlusUOp | MinusUOp => {
								if rhs.get_type().supports_arith_ops() {
									Ok(ExprNode(UnArith(UnArithExpr {
										rhs: Box::new(rhs),
										op: unop.op.clone(),
									})))
								} else {
									Err(EExprNotSupportArithOp)
								}
							}
							NotUOp => {
								if rhs.get_type().supports_logical_ops() {
									Ok(ExprNode(UnNot(UnNotExpr {
										rhs: Box::new(rhs)
									})))
								} else {
									Err(EExprNotSupportLogicalOp)
								}
							}
						}
					}
					Err(e) => Err(e)
				}
			},
			PrimaryExpr(ref prim) => prim.accept(self),
		}
	}

	fn visit_ctor(&mut self, ctor: &lnp::past::Ctor) -> Self::Result {
		match self.state {
			First => {
				// Normalize Ctor
				let (args, _) = ctor.normalize();
				println!("normalize ctor with args: {}", args.iter().map(|(name, ty, lvl)|
					format!("{}_{}", name, Type{base:ty.clone(), array_lvl:lvl.clone()})).collect::<Vec<String>>().join("_"));

				// Get current class
				let curr_cls = self.get_curr_scope_as_class();

				// Create ctor node
				let ctor_node = Rc::new(crate::decaf::Ctor::new(curr_cls.clone()));

				// Get arg type
				for (arg_name, arg_ty, arg_array_lvl) in args.into_iter() {
					let tmp = ctor_node.clone();
					let tmp_name = arg_name.clone();
					if let Some(arg_ty) = self.type_add(arg_ty.clone(),
														arg_array_lvl,
														Some(Box::new(
															move |cls_ty| {
																tmp.set_arg_type(tmp_name.clone(), Rc::new(Type {
																	base: ClassTy(cls_ty),
																	array_lvl: arg_array_lvl,
																}));
																None
															}
														))) {
						ctor_node.add_arg(arg_name, Rc::new(arg_ty));
					} else {
						// Type is unknown at this time
						ctor_node.add_arg(arg_name, Rc::new(Type {
							base: arg_ty,
							array_lvl: arg_array_lvl,
						}))
					}
				}

				// Check signature conflict (except for overriding default ctor)
				if curr_cls.pub_ctors.borrow().iter().any(|m| m.has_same_signature(ctor_node.as_ref())) ||
					curr_cls.prot_ctors.borrow().iter().any(|m| m.has_same_signature(ctor_node.as_ref())) ||
					curr_cls.priv_ctors.borrow().iter().any(|m| m.has_same_signature(ctor_node.as_ref())) {
					panic!("ctor duplicate {}", ctor_node);
				}

				// Store ctor according to modifiers
				match (ctor.modies.len(), ctor.modies.last()) {
					(1, Some(modifier)) => {
						match modifier {
							ModPublic(_) => {
								*ctor_node.vis.borrow_mut() = Pub;
								curr_cls.pub_ctors.borrow_mut().push(ctor_node);
							}
							ModPrivate(_) => {
								*ctor_node.vis.borrow_mut() = Priv;
								curr_cls.priv_ctors.borrow_mut().push(ctor_node);
							}
							ModProtected(_) => {
								*ctor_node.vis.borrow_mut() = Prot;
								curr_cls.prot_ctors.borrow_mut().push(ctor_node);
							}
							ModStatic(_) => {
								panic!("invalid modifier static for constructors");
							}
						};
					},
					(0, None) => {
						// Default to public ctors
						*ctor_node.vis.borrow_mut() = Pub;
						curr_cls.pub_ctors.borrow_mut().push(ctor_node);
					},
					_ => {
						return Err(EInvalidModifier);
					}
				};
			}
			Second => {
				use decaf::Stmt::*;

				// Get current class
				let cls = self.get_curr_scope_as_class();

				// Normalize ctor
				let (mut args, _block) = ctor.normalize();
				// Resolve args
				args = args.into_iter().map(|(name, ty_base, array_lvl)| (name, match ty_base {
					UnknownTy(cls_name) => ClassTy(self.class_lookup(cls_name).unwrap()),
					_ => ty_base,
				}, array_lvl)).collect();

				// Get current ctor
				let ctor_node = match cls.get_ctor_by_signature(&args) {
					Some(ctor) => ctor,
					None => panic!("cannot find ctor for args: {}", args.iter().map(|(name, ty, lvl)|
					format!("{}_{}", name, Type{base:ty.clone(), array_lvl:lvl.clone()})).collect::<Vec<String>>().join("_")),
				};

				// Push new scope
				self.scopes.push(CtorScope(ctor_node.clone()));

				// Add argument as variable
				self.variable_add("this".into(), Type {
					base: ClassTy(cls.clone()),
					array_lvl: 0
				});
				for (arg_name, arg_ty_base, arg_array_lvl) in args.iter() {
					let arg_ty = decaf::Type {
						base: arg_ty_base.clone(),
						array_lvl: *arg_array_lvl
					};
					self.variable_add(arg_name.clone(), arg_ty);
				}

				// Visit super ctor (except for class whose super is Object)
				*ctor_node.body.borrow_mut() = Some(Rc::new(decaf::BlockStmt{
					vartbl: RefCell::new(vec![]),
					stmts: RefCell::new(vec![]),
				}));
				let mut stmts = vec![];
				let sup_cls = cls.sup.borrow().clone().unwrap();
				if sup_cls.name != "Object" {
					stmts.push(Super(decaf::SuperStmt {
						sup: sup_cls.clone(),
						sup_ctor: sup_cls.get_compatible_level0_ctor(&vec![]).unwrap(), // Look for default constructor
						args: vec![],
					}));
				}

				// Visit block
				let ret =  match ctor.block.accept(self) {
					Ok(StmtNode(Block(blk))) => {
						stmts.push(Block(blk));
						*ctor_node.body.borrow_mut() = Some(Rc::new(decaf::BlockStmt{
							vartbl: RefCell::new(vec![]),
							stmts: RefCell::new(stmts),
						}));
						Ok(Normal)
					}
					Ok(_) => {
						Err(EBlockStmtNotFound(format!("{}", ctor_node)))
					}
					Err(e) => Err(e)
				};

				self.scopes.pop();

				return ret;
			}
		}
		Ok(Normal)
	}

	fn visit_stmt(&mut self, stmt: &lnp::past::Statement) -> Self::Result {
		match self.state {
			First => panic!("stmt should not be visited in the first pass"),
			Second => {
				match &stmt.stmt {
					EmptyStmt => Ok(StmtNode(NOP)),
					DeclareStmt(ty, var_decls) => {
							let ty = ty2ty2(ty, 0, self);
							let mut res = vec![];
							for var_decl in var_decls.iter() {
								// Normalize vardecl
								let (array_cnt, name, init_expr) = var_decl.normalize();

								// Check for name conflict and add variable
								match self.variable_lookup(name.clone()) {
									Some(_) => {
										return Err(EVariableRedefinition);
									}
									None => {
										let real_ty = decaf::Type {
											base: ty.base.clone(),
											array_lvl: ty.array_lvl + array_cnt,
										};

										// Check if init_expr matches with type
										match init_expr {
											Some(init_expr) => {
												match init_expr.accept(self) {
													Ok(init_expr_v) => match init_expr_v {
														ExprNode(init_expr_v) => {
															let init_expr_ty = init_expr_v.get_type();
															if init_expr_ty.is_compatible_with(&real_ty, false) {
																self.variable_add(name.clone(), real_ty.clone());
																res.push(Declare(decaf::DeclareStmt {
																	name: name.clone(),
																	ty: real_ty,
																	init_expr: Some(init_expr_v),
																}));
															} else {
																return Err(EExprNotCompatibleType);
															}
														},
														_ => return Err(EExprNodeNotFound(format!("decalre of {}", name)))
													}
													Err(e) => return Err(e)
												}
											}
											None => {
												if array_cnt > 0 {
													return Err(EUninitializedArray);
												} else {
													self.variable_add(name.clone(), real_ty.clone());
													res.push(Declare(decaf::DeclareStmt {
														name: name.clone(),
														ty: real_ty,
														init_expr: None,
													}));
												}

											}
										};
									}
								}
							}
							Ok(VecStmt(res))
						}
					IfStmt(condexpr, thenstmt) =>
						match condexpr.accept(self) {
							Ok(condexpr) => {
								match condexpr {
									ExprNode(condexpr) => {
										if condexpr.get_type().supports_logical_ops() {
											match thenstmt.accept(self) {
												Ok(thenstmt) => {
													match thenstmt {
														StmtNode(thenstmt) => {
															Ok(StmtNode(If(decaf::IfStmt{
																cond: condexpr,
																thenstmt: Box::new(thenstmt),
															})))
														}
														_ => Err(EStmtOrVecStmtNotFound)
													}
												},
												Err(e) => Err(e)
											}
										} else {
											Err(ECondNotLogicalType)
										}
									}
									_ => Err(EExprNodeNotFound(format!("IfStmt")))
								}
							},
							Err(e) => Err(e)
						},
					IfElseStmt(condexpr, thenstmt, elsestmt) =>
						match condexpr.accept(self) {
							Ok(condexpr) => match condexpr {
								ExprNode(condexpr) => {
									if condexpr.get_type().supports_logical_ops() {
										match (thenstmt.accept(self), elsestmt.accept(self)) {
											(Ok(thenstmt), Ok(elsestmt)) => match (thenstmt, elsestmt) {
												(StmtNode(thenstmt), StmtNode(elsestmt)) => {
													Ok(StmtNode(IfElse(decaf::IfElseStmt {
														cond: condexpr,
														thenstmt: Box::new(thenstmt),
														elsestmt: Box::new(elsestmt),
													})))
												}
												_ => Err(EStmtNodeNotFound)
											},
											(Err(e), _) | (_, Err(e)) => Err(e)
										}
									} else {
										Err(ECondNotLogicalType)
									}
								}
								_ => Err(EExprNodeNotFound(format!("If Else Stmt")))
							},
							Err(e) => Err(e)
						},
					ExprStmt(expr) => {
						match expr.accept(self) {
							Ok(expr) =>
								match expr {
									ExprNode(expr) => Ok(StmtNode(Expr(decaf::ExprStmt {
										expr,
									}))),
									_ => Err(EExprNodeNotFound(format!("expr stmt")))
								},

							Err(e) => Err(e)
						}
					}
					WhileStmt(condexpr, stmt) => {
						match condexpr.accept(self) {
							Ok(condexpr) => match condexpr {
								ExprNode(condexpr) => {
									let whilestmt = Rc::new(decaf::WhileStmt {
										condexpr,
										bodyblock: RefCell::new(None),
										condbb: RefCell::new(None),
										bodybb: RefCell::new(None),
										nextbb: RefCell::new(None)
									});
									self.scopes.push(WhileScope(whilestmt.clone()));
									match stmt.accept(self) {
										Ok(bodystmt) => match bodystmt {
											StmtNode(bodystmt) => match bodystmt {
												Block(bodyblock) => {
													*whilestmt.bodyblock.borrow_mut() = Some(bodyblock);
													self.scopes.pop();
													Ok(StmtNode(While(whilestmt)))
												},
												_ => Err(EBlockStmtNotFound(format!("{}", bodystmt)))
											},
											_ => Err(EStmtNodeNotFound)
										}
										Err(e) => Err(e)
									}
								}
								_ => Err(EExprNodeNotFound(format!("While Stmt")))
							}
							Err(e) => Err(e)
						}
					}
					ReturnStmt(retexpr) => {
						use decaf::Scope::*;
						match retexpr {
							None => {
								use decaf::TypeBase::*;
								// Check if return type is void
								match self.get_top_most_method_scope() {
									Some(MethodScope(method)) => {
										if let VoidTy = &(*method.return_ty.borrow()).base {
											Ok(StmtNode(Return(decaf::ReturnStmt {
												expr: None,
											})))
										} else {
											Err(EUnmatchedReturnType("VoidTy".into()))
										}
									}
									_ => Err(EReturnInNonMethodScope)
								}
							}
							Some(retexpr) => match retexpr.accept(self) {
								Ok(retexpr) => match retexpr {
									ExprNode(retexpr) => {
										let ret_ty = retexpr.get_type();
										match self.get_top_most_method_scope() {
											Some(MethodScope(method)) => {
												if ret_ty.is_compatible_with(&method.return_ty.borrow(), false) {
													Ok(StmtNode(Return(decaf::ReturnStmt {
														expr: Some(retexpr),
													})))
												} else {
													Err(EUnmatchedReturnType(format!("{}", retexpr)))
												}
											}
											_ => Err(EReturnInNonMethodScope)
										}
									},
									_ => Err(EExprNodeNotFound(format!("Return stmt")))
								},
								Err(e) => Err(e)
							}
						}
					}
					ContinueStmt => {
						use decaf::Scope::*;
						// Check if we are in a loop
						match self.get_top_most_while_scope() {
							Some(WhileScope(whilestmt)) => {
								Ok(StmtNode(Continue(decaf::ContinueStmt {
									lup: whilestmt.clone(),
								})))
							}
							_ => Err(EContinueNotInLoop)
						}
					}
					BreakStmt => {
						use decaf::Scope::*;
						// Check if we are in a loop
						match self.get_top_most_while_scope() {
							Some(WhileScope(whilestmt)) => {
								Ok(StmtNode(Break(decaf::BreakStmt {
									lup: whilestmt.clone(),
								})))
							}
							_ => Err(EBreakNotInLoop)
						}
					}
					SuperStmt(args) => {
						use decaf::Scope::*;
						// Super constructor call
						match self.get_top_most_ctor_scope() {
							Some(_) => {
								// Check the closest ancestor that has the signature
								match args.accept(self) {
									Ok(VecExpr(args_expr)) => {
										match self.get_top_most_class_scope() {
											Some(ClassScope(cls)) => {
												match cls.get_compatible_super_ctor(&args_expr) {
													Some(sup_ctor) => Ok(StmtNode(Super(decaf::SuperStmt {
														sup: cls.sup.borrow().as_ref().unwrap().clone(),
														sup_ctor,
														args: args_expr,
													}))),
													None => Err(ENoCompatibleCtor)
												}
											},
											Some(_) | None => Err(ECallingCtorOutsideClass)
										}
									}
									_ => Err(EVecExprNotFound(format!("SuperStmt")))
								}
							}
							None => Err(ECallingSuperCtorOutsideCtor)
						}
					}
					BlockStmt(block) => block.accept(self),
				}
			}
		}
	}

	fn visit_block(&mut self, block: &lnp::past::Block) -> Self::Result {
		match self.state {
			First => panic!("block should not be visited in the first pass"),
			Second => {
				// Create a new scope
				let blockstmt = Rc::new(decaf::BlockStmt {
					vartbl: RefCell::new(Vec::new()),
					stmts: RefCell::new(Vec::new()),
				});

				self.scopes.push(BlockScope(blockstmt.clone()));

				for stmt in block.stmts.iter() {
					match stmt.accept(self) {
						Ok(StmtNode(stmt)) => {
							blockstmt.stmts.borrow_mut().push(stmt);
						}
						Ok(VecStmt(stmts)) => {
							blockstmt.stmts.borrow_mut().extend(stmts);
						}
						Ok(_) => {
							return Err(EStmtOrVecStmtNotFound);
						},
						Err(e) => {
							debug!("visit_block: Get an error: {:?}", e);
							return Err(e);
						}
					}
				}

				self.scopes.pop();

				Ok(StmtNode(Block(blockstmt)))
			}
		}
	}

	fn visit_dims(&mut self, dims: &Vec<lnp::past::Dimension>) -> Self::Result {
		match self.state {
			First => panic!("dims should not be visited in the first pass"),
			Second => {
				let mut res = vec![];
				for dim in dims.iter() {
					match dim.expr.accept(self) {
						Ok(ExprNode(expr)) => {
							res.push(expr);
						},
						Ok(_) => {
							return Err(EExprNodeNotFound(format!("dim")));
						},
						Err(e) => {
							return Err(e);
						}
					}
				}
				Ok(VecExpr(res))
			}
		}
	}

	fn visit_primary(&mut self, prim: &lnp::past::Primary) -> Self::Result {
		match self.state {
			First => panic!("primary should not be visited in the first pass"),
			Second => {
				match &prim.prim {
					NewArray(naexpr) => {
						match &naexpr.expr {
							NewCustom(id, dims) => {
								match dims.accept(self) {
									Ok(dims_expr) => {
										match dims_expr {
											VecExpr(dims_expr) => {
												if dims_expr.iter().all(|dim| dim.get_type().supports_arith_ops()) {
													// lookup class for id
													if let Some(cls) = self.class_lookup(id.clone()) {
														Ok(ExprNode(CreateArray(CreateArrayExpr {
															ty: ClassTy(cls),
															dims: dims_expr,
														})))
													} else {
														Err(EClassNotDefined)
													}
												} else {
													Err(EDimExprNotArithType)
												}
											},
											_ => Err(EVecExprNotFound(format!("NewCustom"))),
										}
									}
									Err(e) => Err(e)
								}
							}
							NewPrimitive(primtype, dims) => {
								match dims.accept(self) {
									Ok(dims_expr) => {
										match dims_expr {
											VecExpr(dims_expr) => {
												if dims_expr.iter().all(|dim| dim.get_type().supports_arith_ops()) {
													match primtype {
														BoolType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
															ty: BoolTy,
															dims: dims_expr,
														}))),
														IntType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
															ty: IntTy,
															dims: dims_expr,
														}))),
														CharType(_) => Ok(ExprNode(CreateArray(CreateArrayExpr {
															ty: CharTy,
															dims: dims_expr,
														}))),
														VoidType(_) => Err(EVoidArray)
													}
												} else {
													Err(EDimExprNotArithType)
												}
											},
											_ => Err(EExprNodeNotFound(format!("New Primitive"))),
										}
									}
									Err(e) => Err(e)
								}
							}
						}
					}
					NonNewArray(nnaexpr) => nnaexpr.expr.accept(self),
					Identifier(id) => {
						match self.variable_lookup(id.clone()) { // look for variable
							Some(var) => Ok(ExprNode(Variable(VariableExpr {
								var
							}))),
							None => { // Look for fields
								match self.get_top_most_class_scope() {
									Some(ClassScope(cls)) => {
										match cls.get_field_by_name(&id) {
											Some(field) => {
												Ok(ExprNode(FieldAccess(FieldAccessExpr {
													var: Box::new(This(ThisExpr {cls: cls.clone()})),
													cls,
													fld: field,
												})))
											}
											None => match self.class_lookup(id.clone()) {
												Some(cls) => Ok(ExprNode(ClassId(ClassIdExpr {
													cls,
												}))),
												None => Err(EUnknownIdentifier(id.clone()))
											}
										}
									}
									_ => Err(ENotInClassScope)
								}
							}
						}
					}
				}
			}
		}
	}

	fn visit_nnaexpr(&mut self, nnaexpr: &lnp::past::NNAExpr) -> Self::Result {
		match self.state {
			First => panic!("block should not be visited in the first pass"),
			Second => {
				match &nnaexpr {
					JustLit(litr) => {
						match &litr.litr {
							Null => Ok(ExprNode(NULL)),
							Bool(ref b) => Ok(ExprNode(Literal(BoolLiteral(b.clone())))),
							Char(ref c) => Ok(ExprNode(Literal(CharLiteral(c.clone())))),
							Int(ref i) => Ok(ExprNode(Literal(IntLiteral(i.clone())))),
							Str(ref s) => {
								match self.class_lookup("String".to_string()) {
									Some(cls) => {
										let lit = Literal(StrLiteral(s.clone()));
										let arg_expr = vec![lit];
										Ok(ExprNode(CreateObj(CreateObjExpr {
											cls,
											ctor: None,
											args: arg_expr,
										})))
									}
									None => panic!("String class not found"),
									}
								}
							}
						}
					JustThis => self.visit_this(),
					JustExpr(expr) => expr.accept(self),
					NewObj(id, args) => {
						if let Some(cls) = self.class_lookup(id.clone()) {
							match args.accept(self) {
								Ok(VecExpr(arg_exprs)) => {
									// Check if signature matches with any of the constructors
									// TODO: create a get_visible_ctors() function to tide things up
									match if self.has_class_in_scope_stack(&cls) {
										cls.get_compatible_level0_ctor(&arg_exprs)
									} else {
										cls.get_compatible_level1_ctor(&arg_exprs)
									} {
										Some(ctor) => Ok(ExprNode(CreateObj(CreateObjExpr {
											cls,
											ctor: Some(ctor),
											args: arg_exprs,
										}))),
										None => Err(ENoCompatibleCtor),
									}
								}
								_ => Err(EVecExprNotFound(format!("NewObj")))
							}
						} else {
							Err(EClassNotDefined)
						}
					}
					CallSelfMethod(id, args) => {
						match args.accept(self) {
							Ok(VecExpr(arg_exprs)) => {
								// Lookup the method
								match self.get_top_most_class_scope() {
									Some(ClassScope(cls)) => {
										match self.get_top_most_method_scope() {
											Some(MethodScope(method)) => {
												match *method.stat.borrow() {
													false => {
														println!("========trying to find method \"{}\"======", id);
														match cls.get_compatible_level0_method(&arg_exprs, id) {
															Some(method) => {
																Ok(ExprNode(MethodCall(MethodCallExpr {
																	var: Box::new({
																		match self.visit_this() {
																			Ok(ExprNode(this_expr)) => match this_expr {
																				This(_) => this_expr,
																				_ => return Err(EThisNotFound),
																			},
																			_ => return Err(EExprNodeNotFound(format!("CallSelfMethod {}", id)))
																		}
																	}),
																	method,
																	args: arg_exprs,
																})))

															},
															// Look for static method
															None => match cls.get_compatible_level0_static_method(&arg_exprs, id) {
																Some(method) => {

																	Ok(ExprNode(MethodCall(MethodCallExpr {
																		var: Box::new(ClassId(ClassIdExpr {
																			cls: cls.clone()
																		})),
																		method,
																		args: arg_exprs,
																	})))

																},
																None => Err(ENoCompatibleMethod(format!("point A: {}", id.clone())))
															}
														}
													}
													true => {
														println!("========trying to find static method \"{}\"======", id);
														match cls.get_compatible_level0_static_method(&arg_exprs, id) {
															Some(method) => {
																Ok(ExprNode(MethodCall(MethodCallExpr {
																	var: Box::new(ClassId(ClassIdExpr {
																		cls: cls.clone()
																	})),
																	method,
																	args: arg_exprs,
																})))
															},
															None => Err(ENoCompatibleMethod(format!("point B: {}", id.clone())))
														}
													}
												}
											}
											_ => match self.get_top_most_ctor_scope() {
												Some(CtorScope(ctor)) => {
													match cls.get_compatible_level0_method(&arg_exprs, id) {
														Some(method) => {
															Ok(ExprNode(MethodCall(MethodCallExpr {
																var: Box::new({
																	match self.visit_this() {
																		Ok(ExprNode(this_expr)) => match this_expr {
																			This(_) => this_expr,
																			_ => return Err(EThisNotFound),
																		},
																		_ => return Err(EExprNodeNotFound(format!("CallSelfMethod in ctor {}", id)))
																	}
																}),
																method,
																args: arg_exprs,
															})))

														},
														None => match cls.get_compatible_level0_static_method(&arg_exprs, id) {
															Some(method) => {
																Ok(ExprNode(MethodCall(MethodCallExpr {
																	var: Box::new(ClassId(ClassIdExpr {
																		cls: cls.clone()
																	})),
																	method,
																	args: arg_exprs,
																})))
															},
															None => Err(ENoCompatibleMethod(format!("point C: {}", id.clone())))
														}
													}
												}
												_ => Err(EMethodCallWithoutMethod)
											}
										}

									},
									_ => Err(EMethodCallWithoutClass),
								}
							}
							_ => Err(EVecExprNotFound(format!("CallSelfMethod {}", id)))
						}
					}
					CallMethod(nprim, id, args) => {
						match args.accept(self) {
							Ok(VecExpr(arg_exprs)) => {
								match nprim.accept(self) {
									Ok(ExprNode(prim_expr)) => {
//										println!("arg_exprs.len() : {}", arg_exprs.len());
//										if arg_exprs.len() > 0 {
//											println!("arg_exprs[0] type: {:?}", arg_exprs[0].get_type());
//										}

										match prim_expr {
											ClassId(cls_id_expr) => {
												match if self.has_class_in_scope_stack(&cls_id_expr.cls) {
													cls_id_expr.cls.get_compatible_level0_static_method(&arg_exprs, id)
												} else {
													cls_id_expr.cls.get_compatible_level1_static_method(&arg_exprs, id)
												} {
													Some(method) => {
//														println!("Found method");
//														println!("method.name : {:?}", method.name);
//														println!("id : {:?}", *id);
														if method.name == *id {
															Ok(ExprNode(MethodCall(MethodCallExpr {
																var: Box::new(ClassId(cls_id_expr)),
																method,
																args: arg_exprs
															})))
														} else {
															Err(ENoCompatibleMethod(id.clone()))
														}
													}
													None => Err(ENoCompatibleMethod(id.clone()))
												}
											},
											_ => {
												let prim_ty = prim_expr.get_type();
												if let ClassTy(cls) = prim_ty.base {
													if prim_ty.array_lvl == 0 {
														// Lookup method
														match if self.has_class_in_scope_stack(&cls) {
															cls.get_compatible_level0_method(&arg_exprs, id)
														} else {
															cls.get_compatible_level1_method(&arg_exprs, id)
														} {
															Some(method) => {
																if method.name == *id {
																	Ok(ExprNode(MethodCall(MethodCallExpr {
																		var: Box::new(prim_expr),
																		method,
																		args: arg_exprs
																	})))
																} else {
																	Err(ENoCompatibleMethod(id.clone()))
																}
															},
															None => Err(ENoCompatibleMethod(id.clone()))
														}
													} else {
														Err(EArrayTypeNotSupportMethodCall)
													}
												} else {
													Err(ENonClassTypeNotSupportMethodCall)
												}
											}
										}
									},
									_ => Err(EExprNodeNotFound(format!("{}", id)))
								}
							}
							Ok(_) => Err(EVecExprNotFound(format!("CallMethod {}", id))),
							Err(e) => Err(e),
						}
					}
					CallSuper(id, args) => {
						match args.accept(self) {
							Ok(VecExpr(arg_exprs)) => {
								// NOTE: make it recursive if we support nested class
								match self.get_top_most_class_scope() {
									Some(ClassScope(cls)) => {
										match cls.get_compatible_level0_super_method(&arg_exprs, id) {
											Some(method) => {
												Ok(ExprNode(SuperCall(SuperCallExpr {
													cls: cls.clone(),
													method,
													args: arg_exprs,
												})))
											},
											None => match cls.get_compatible_level0_super_static_method(&arg_exprs, id) {
												Some(method) => {
													Ok(ExprNode(MethodCall(MethodCallExpr {
														var: Box::new(ClassId(ClassIdExpr {
															cls: cls.clone()
														})),
														method,
														args: arg_exprs,
													})))
												},
												None => Err(ENoCompatibleMethod(id.clone()))
											}
										}
									}
									Some(_) | None => Err(ESuperCallWithoutClass)
								}
							},
							Ok(_) => Err(EVecExprNotFound(format!("CallSuper {}", id))),
							Err(e) => Err(e)
						}
					}
					EvalArray(arrexpr) => {
						match &arrexpr.expr {
							SimpleArraryExpr(id, dim) => {
								// Lookup id, must have array_lvl
								match self.variable_lookup(id.clone()) {
									Some(inst) => {
										match dim.expr.accept(self) {
											Ok(idx_expr) => match idx_expr {
												ExprNode(idx_expr) => {
													let ty = inst.get_type();
													if ty.array_lvl > 0 {
														Ok(ExprNode(ArrayAccess(ArrayAccessExpr {
															var: Box::new(Variable(VariableExpr {
																var: inst
															})),
															idx: Box::new(idx_expr),
														})))
													} else {
														Err(EArrayAccessOnNonArrayType)
													}
												}
												_ => Err(EExprNodeNotFound(format!("EvalArray")))
											}
											Err(e) => Err(e)
										}
									}
									None => {
										// This could be a field
										match self.get_top_most_class_scope() {
											Some(ClassScope(cls)) => {
												match cls.get_field_by_name(&id) {
													Some(field) => {
														match dim.expr.accept(self)? {
															ExprNode(idx_expr) => {
																Ok(ExprNode(ArrayAccess(ArrayAccessExpr {
																	var: Box::new(FieldAccess(FieldAccessExpr {
																		var: Box::new(This(ThisExpr { cls: cls.clone() })),
																		cls,
																		fld: field,
																	})),
																	idx: Box::new(idx_expr),
																})))
															}
															_ => Err(EExprNodeNotFound(format!("Field lookup")))
														}
													}
													None => Err(EUnknownIdentifier(id.clone()))
												}
											}
											_ => Err(EUnknownIdentifier(id.clone()))
										}

									}
								}
							}
							ComplexArrayExpr(nna, dim) => {
								match nna.expr.accept(self) {
									Ok(prim_expr) => match prim_expr {
										ExprNode(prim_expr) => {
											let ty = prim_expr.get_type();
											if ty.array_lvl > 0 {
												match dim.expr.accept(self) {
													Ok(dim_expr) => match dim_expr {
														ExprNode(dim_expr) => {
															Ok(ExprNode(ArrayAccess(ArrayAccessExpr {
																var: Box::new(prim_expr),
																idx: Box::new(dim_expr),
															})))
														}
														_ => Err(EExprNodeNotFound(format!("dim expr")))
													}
													Err(e) => Err(e)
												}
											} else {
												Err(EArrayAccessOnNonArrayType)
											}
										}
										_ => Err(EExprNodeNotFound(format!("ComplexArrayExpr PrimExpr")))
									}
									Err(e) => Err(e)
								}
							}
						}
					}
					EvalField(fldexpr) => {
						match &fldexpr.expr {
							PrimaryFieldExpr(prim, id) => {
								match prim.accept(self) {
									Ok(prim_expr) => match prim_expr {
										ExprNode(prim_expr) => {
											// Lookup field
											let ty = prim_expr.get_type();
											match ty.base {
												ClassTy(cls) => {
													match ty.array_lvl {
														0 => {
															match if self.has_class_in_scope_stack(&cls) {
																cls.get_level0_field_by_name(id)
															} else {
																cls.get_level1_field_by_name(id)
															} {
																Some(fld) => Ok(ExprNode(FieldAccess(FieldAccessExpr {
																	var: Box::new(prim_expr),
																	cls,
																	fld,
																}))),
																None => Err(ENoCompatibleField)
															}
														}
														_ => {
															if ty.array_lvl > 0 {
																if id == "length" {
																	// Only field "length"
																	let fld = Rc::new(decaf::Field::new("length".into(), None));
																	fld.set_base_type(Rc::new(decaf::Type {
																		base: IntTy,
																		array_lvl: 0
																	}));
																	Ok(ExprNode(FieldAccess(FieldAccessExpr {
																		var: Box::new(prim_expr),
																		cls,
																		fld,
																	})))
																} else {
																	Err(ENoCompatibleField)
																}
															} else {
																Err(EArrayNegativeLevel)
															}
														}
													}
												}
												_ => Err(EPrimitiveTypeNoField)
											}
										}
										_ => Err(EExprNodeNotFound(format!("Primary FieldExpr")))
									}
									Err(e) => Err(e)
								}
							}
							SuperFieldExpr(id) => {
								// Since class inherited all fields, this is a simple field lookup
								// NOTE: this implies that hidden fields will never be accessed even in this scenario
								match self.get_top_most_class_scope() {
									Some(ClassScope(cls)) => {
										match cls.get_level0_super_field_by_name(id) {
											Some(fld) => Ok(ExprNode(FieldAccess(FieldAccessExpr {
												var: Box::new({
													match self.visit_this() {
														Ok(ExprNode(this_expr)) => match this_expr {
															This(_) => this_expr,
															_ => return Err(EThisNotFound),
														},
														_ => return Err(EExprNodeNotFound(format!("Super field Expr")))
													}
												}),
												cls,
												fld,
											}))),
											None => Err(ENoCompatibleField)
										}
									}
									_ => Err(EFieldAccessWithoutClass)
								}
							}
						}
					}
				}
			}
		}
	}

	fn visit_actural_args(&mut self, args: &lnp::past::ActualArgs) -> Self::Result {
		use self::BuilderResult::*;
		use SemanticError::*;
		let mut res = vec![];
		for arg in args.exprs.iter() {
			match arg.accept(self) {
				Ok(arg_expr) => match arg_expr {
					ExprNode(arg_expr) => {
						res.push(arg_expr);
					}
					_ => {
						return Err(EExprNodeNotFound(format!("arg")));
					}
				}
				Err(e) => { return Err(e); }
			}
		}
		Ok(VecExpr(res))
	}

	fn visit_this(&mut self) -> Self::Result {
		match self.get_top_most_class_scope() {
			Some(ClassScope(cls)) => {
				match self.get_top_most_method_scope() {
					Some(MethodScope(mthd)) => {
						if *mthd.stat.borrow() {
							Err(EThisInStaticMethod)
						} else {
							Ok(ExprNode(This(ThisExpr {cls})))
						}
					}
					_ => match self.get_top_most_ctor_scope() {
						Some(CtorScope(ctor)) => {
							Ok(ExprNode(This(ThisExpr {cls})))
						}
						_ => Err(EThisWithoutMethod)
					}
				}
			}
			_ => Err(EThisWithoutClass)
		}
	}
}

pub fn parse_decaf<'a>(src: &str) -> Result<Program, String> {
	let syntactic_program = lnp::parse_decaf(src);
	let mut builder = DecafTreeBuilder::new();
	match builder.visit_program(&syntactic_program) {
		Ok(Normal) => {
			Ok(Program {
				classes: builder.class_map.iter()
					.map(|(_, cls)| cls.clone()).collect::<Vec<Rc<decaf::Class>>>()
			})
		}
		Ok(_) => Err("expect Normal result".to_string()),
		Err(e) => Err(format!("{:?}", e))
	}
}

// TODO: disallow calling private method in this case:
// class Foo {
//     public void f(Foo foo) {
//          foo.h()
//     }
//     private void h() {
//     }
// }

// TODO: check argument type compatibility
