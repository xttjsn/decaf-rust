use std::rc::Rc;
use std::cmp::Ordering;
use std::fmt;
use std::collections::BTreeSet;
use std::cell::RefCell;
use crate::lnp;

pub enum SemanticError{
	ClassRedefinition(String),
	BadImplementation(String),
	InvalidModifier(lnp::lexer::Span)
}

impl fmt::Display for SemanticError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use self::SemanticError::*;
		match self {
			ClassRedefinition(cls_name) => write!(f, "redefinition of class {}", cls_name),
			BadImplementation(why) => write!(f, "bad implementation: {}", why),
			InvalidModifier(loc) => write!(f, "invalid modifier at {},{}", loc.lo, loc.hi),
		}
	}
}


#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
	Pub,
	Priv,
	Prot
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
	MethodTy(Rc<Method>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
	pub name: String,
	pub return_ty: RefCell<Rc<Type>>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>,
}

impl Method {

	pub fn new(name: String) -> Self {
		Method {
			name: name,
			return_ty: RefCell::new(Rc::new(Type {
				base: TypeBase::UnknownTy("Unknown".to_string()),
				array_lvl: 0,
			})),
			args: RefCell::new(vec![]),
		}
	}

	pub fn add_arg(&self, arg_name: String, arg_ty: Rc<Type>) {
		// Check name conflicts
		if self.args.borrow().iter().any(|(name, ty)| name == &arg_name) {
			panic!("argument name conflict in method")
		}
		self.args.borrow_mut().push((arg_name, arg_ty));
	}

	pub fn set_arg_type(&self, arg_name: String, arg_ty: Rc<Type>) {
		if !self.args.borrow().iter().any(|(name, ty)| name == &arg_name) {
			panic!("argument not found in method")
		}
		self.args.replace_with(|v| v.into_iter().map(|(name, ty)| {
			if name == &arg_name {
				(name.clone(), arg_ty.clone())
			} else {
				(name.clone(), ty.clone())
			}
		}).collect());
	}

	pub fn has_same_signature(&self, other: &Self) -> bool {
		if self.name != other.name {
			return false;
		}
		if self.return_ty != other.return_ty {
			return false;
		}
		if self.args.borrow().len() != other.args.borrow().len() {
			return false;
		}
		for (lhs, rhs) in self.args.borrow().iter().zip(other.args.borrow().iter()) {
			// Compare type
			let (lhs, rhs) = (lhs.1.as_ref(), rhs.1.as_ref());
			if lhs.array_lvl != rhs.array_lvl {
				return false;
			}
			if lhs.base != rhs.base {
				return false;
			}
		}
		return true;
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ctor {
	pub cls: Rc<Class>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>
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
	MethodScope(Rc<Method>),
	BlockScope(Rc<Block>),
	GlobalScope,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
	pub name: String,
	pub ty: RefCell<Rc<Type>>,
	pub init_expr: RefCell<Option<Rc<Expr>>>,
}

impl Field {
	pub fn new(name: String) -> Field {
		Self {
			name: name,
			ty: RefCell::new(Rc::new(Type {
				base: TypeBase::UnknownTy("Unknown".to_string()),
				array_lvl: 0,
			})),
			init_expr: RefCell::new(None),
		}
	}

	pub fn set_base_type(&self, ty: Rc<Type>) {
		*self.ty.borrow_mut() = ty;
	}

	fn set_init_expr(&self, expr: Rc<Expr>) {
		*self.init_expr.borrow_mut() = Some(expr);
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
	pub name: String,
	pub sup: RefCell<Option<Rc<Class>>>,
	pub pub_fields: RefCell<Vec<Rc<Field>>>,
	pub prot_fields: RefCell<Vec<Rc<Field>>>,
	pub priv_fields: RefCell<Vec<Rc<Field>>>,
	pub pub_ctors: RefCell<Vec<Rc<Ctor>>>,
	pub prot_ctors: RefCell<Vec<Rc<Ctor>>>,
	pub priv_ctors: RefCell<Vec<Rc<Ctor>>>,
	pub pub_methods: RefCell<Vec<Rc<Method>>>,
	pub prot_methods: RefCell<Vec<Rc<Method>>>,
	pub priv_methods: RefCell<Vec<Rc<Method>>>,
	pub pub_static_methods: RefCell<Vec<Rc<Method>>>,
	pub prot_static_methods: RefCell<Vec<Rc<Method>>>,
	pub priv_static_methods: RefCell<Vec<Rc<Method>>>,
}

fn prepend<T>(v: &[T], s: &[T]) -> Vec<T>
where
	T: Clone,
{
	let mut tmp: Vec<_> = s.to_owned();
	tmp.extend(v.to_owned());
	tmp
}

impl Class {
	pub fn new(name: String) -> Self {
		Class {
			name: name,
			sup: RefCell::new(None),
			pub_fields: RefCell::new(vec![]),
			prot_fields: RefCell::new(vec![]),
			priv_fields: RefCell::new(vec![]),
			pub_ctors: RefCell::new(vec![]),
			prot_ctors: RefCell::new(vec![]),
			priv_ctors: RefCell::new(vec![]),
			pub_methods: RefCell::new(vec![]),
			prot_methods: RefCell::new(vec![]),
			priv_methods: RefCell::new(vec![]),
			pub_static_methods: RefCell::new(vec![]),
			prot_static_methods: RefCell::new(vec![]),
			priv_static_methods: RefCell::new(vec![]),
		}
	}

	pub fn check_pub_method_defined_as_nonpub(&self, supr_pub_method: &Rc<Method>) -> Result<(), &'static str> {
		for prot_method in self.prot_methods.borrow().iter() {
			if prot_method.has_same_signature(supr_pub_method) {
				return Err("parent public method redefined as protected method");
			}
		}

		for priv_method in self.priv_methods.borrow().iter() {
			if priv_method.has_same_signature(supr_pub_method) {
				return Err("parent public method redefined as private method");
			}
		}

		Ok(())
	}

	pub fn check_prot_method_defined_as_nonprot(&self, supr_prot_method: &Rc<Method>) -> Result<(), &'static str> {
		for pub_method in self.pub_methods.borrow().iter() {
			if pub_method.has_same_signature(supr_prot_method) {
				return Err("parent protected method redefined as public method");
			}
		}

		for priv_method in self.priv_methods.borrow().iter() {
			if priv_method.has_same_signature(supr_prot_method) {
				return Err("parent protected method redefined as private method");
			}
		}

		Ok(())
	}

	pub fn set_super(self: Rc<Class>, supr: Rc<Class>) {
		// supr must be a resolved class
		match *supr.sup.borrow() {
			None if supr.name != "Object" => panic!("Super class is not resolved"),
			_ => {}
		}

		// Set super
		*self.sup.borrow_mut() = Some(supr.clone());

		// Inherit public fields
		let pub_fields = prepend(&self.pub_fields.borrow()[..], &supr.pub_fields.borrow()[..]);
		*self.pub_fields.borrow_mut() = pub_fields;

		let prot_fields = prepend(&self.prot_fields.borrow()[..], &supr.prot_fields.borrow()[..]);
		*self.prot_fields.borrow_mut() = prot_fields;

		// Inherit public methods
		for supr_pub_method in supr.pub_methods.borrow().iter() {
			self.check_pub_method_defined_as_nonpub(supr_pub_method).unwrap();
		}
		let pub_methods = prepend(&self.pub_methods.borrow()[..], &supr.pub_methods.borrow()[..]);
		*self.pub_methods.borrow_mut() = pub_methods;

		// Inherit protected methods
		for supr_prot_method in supr.pub_methods.borrow().iter() {
			self.check_prot_method_defined_as_nonprot(supr_prot_method).unwrap();
		}
		let prot_methods = prepend(&self.prot_methods.borrow()[..], &supr.prot_methods.borrow()[..]);
		*self.prot_methods.borrow_mut() = prot_methods;
	}
}
