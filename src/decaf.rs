use std::rc::Rc;
use std::fmt;
use crate::lnp;

pub enum SemanticError{
	ClassRedefinition(Rc<Class>),
	BadImplementation(String),
	InvalidModifier(lnp::lexer::Span)
}

impl fmt::Display for SemanticError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::SemanticError::*;
		match *self {
			ClassRedefinition(c) => write!(f, "redefinition of class {}", c.name),
			BadImplementation(why) => write!(f, "bad implementation: {}", why),
			InvalidModifier(loc) => write!(f, "invalid modifier at {},{}", loc.lo, loc.hi),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
	pub base: TypeBase,
	pub array_lvl: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeBase {
	UnknownTy(String),
	BoolTy,
	IntTy,
	CharTy,
	VoidTy,
	ClassTy(Rc<Class>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
	pub return_ty: Type,
	pub args: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
	pub return_ty: Type,
	pub args: Vec<(String, Type)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ctor {
	pub cls: Rc<Class>,
	pub args: Vec<(String, Type)>
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {

}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {

}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
	ClassScope(Rc<Class>),
	FunctionScope(Rc<Function>),
	MethodScope(Rc<Method>),
	BlockScope(Rc<Block>),
	GlobalScope,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
	pub name: String,
	pub ty: Type,
	pub init_expr: Option<Expr>,
}

impl Field {
	fn new(name: String) -> Field {
		Self {
			name: name,
			ty: Type {
				base: TypeBase::UnknownTy("Unknown"),
				array_lvl: 0,
			},
			init_expr: None,
		}
	}

	fn set_base_type(&mut self, ty: Type) {
		self.ty = ty;
	}

	fn set_init_expr(&mut self, expr: Expr) {
		self.init_expr = Some(expr);
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
	pub name: String,
	pub sup: Option<Rc<Class>>,
	pub inh_fields: Vec<Field>,
	pub fields: Vec<Field>,
	pub ctors: Vec<Ctor>,
	pub vtable: Vec<Method>,
	pub priv_methods: Vec<Method>,
	pub static_methods: Vec<Function>,
}

impl Class {
	fn new(name: String) -> Self {
		Class {
			name: name,
			sup: None,
			inh_fields: vec![],
			fields: vec![],
			ctors: vec![],
			vtable: vec![],
			priv_methods: vec![],
			static_methods: vec![],
		}
	}

	fn has_definition_for(vtable: &Vec<Method>, method: &Method) -> bool {
		match Class::get_method_idx_from_vtable(vtable, method) {
			None => false,
			Some(_) => true,
		}
	}

	fn get_method_idx_from_vtable(vtable: &Vec<Method>, method: &Method) -> Option<usize> {
		for (ix, child_method) in vtable.iter().enumerate() {
			if child_method == method {
				return Some(ix)
			}
		}
		None
	}

	fn merge_vtable(child_vtable: Vec<Method>, parent_vtable: &Vec<Method>) -> Vec<Method> {
		// Merge two vtables and maintain orders
		let result = vec![];
		for method in parent_vtable.iter() {
			if Class::has_definition_for(&child_vtable, method) {
				result.push(*method.clone());
				child_vtable.remove(Class::get_method_idx_from_vtable(&child_vtable, method).unwrap());
			} else {
				result.push(*method.clone());
			}
		}
		for method in child_vtable.into_iter() {
			result.push(method);
		}
		result
	}

	fn merge_fields(inh_fields: &Vec<Field>, fields: &Vec<Field>) -> Vec<Field> {
		let result = vec![];
		result.extend(inh_fields.into_iter().map(|x| *x));
		result.extend(fields.into_iter().map(|x| *x));
		result
	}

	fn set_super(self: Rc<Self>, supr: Rc<Class>) {
		// supr must be a resolved class
		match supr.sup {
			None if supr.name != "Object" => panic!("Super class is not resolved"),
			_ => {}
		}

		// Set vtable and inherit fields
		self.vtable = Class::merge_vtable(self.vtable, &supr.vtable);
		self.inh_fields = Class::merge_fields(&supr.inh_fields, &supr.fields);
	}
}
