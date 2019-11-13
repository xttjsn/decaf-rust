use std::rc::Rc;
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
	FuncTy(Rc<Function>),
	MethodTy(Rc<Method>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
	pub name: String,
	pub return_ty: RefCell<Rc<Type>>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
	pub name: String,
	pub return_ty: Rc<Type>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>,
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
	FunctionScope(Rc<Function>),
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

	pub fn set_base_type(self: Rc<Self>, ty: Rc<Type>) {
		*self.ty.borrow_mut() = ty;
	}

	fn set_init_expr(self: Rc<Self>, expr: Rc<Expr>) {
		*self.init_expr.borrow_mut() = Some(expr);
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
	pub name: String,
	pub sup: RefCell<Option<Rc<Class>>>,
	pub inh_fields: RefCell<Vec<Rc<Field>>>,
	pub fields: RefCell<Vec<Rc<Field>>>,
	pub ctors: RefCell<Vec<Rc<Ctor>>>,
	pub vtable: RefCell<Vec<Rc<Method>>>,
	pub priv_methods: RefCell<Vec<Rc<Method>>>,
	pub static_methods: RefCell<Vec<Rc<Function>>>,
}

impl Class {
	pub fn new(name: String) -> Self {
		Class {
			name: name,
			sup: RefCell::new(None),
			inh_fields: RefCell::new(vec![]),
			fields: RefCell::new(vec![]),
			ctors: RefCell::new(vec![]),
			vtable: RefCell::new(vec![]),
			priv_methods: RefCell::new(vec![]),
			static_methods: RefCell::new(vec![]),
		}
	}

	fn has_definition_for(vtable: &Vec<Rc<Method>>, method: &Rc<Method>) -> bool {
		match Class::get_method_idx_from_vtable(vtable, method) {
			None => false,
			Some(_) => true,
		}
	}

	fn get_method_idx_from_vtable(vtable: &Vec<Rc<Method>>, method: &Rc<Method>) -> Option<usize> {
		for (ix, child_method) in vtable.iter().enumerate() {
			if child_method == method {
				return Some(ix)
			}
		}
		None
	}

	fn merge_vtable(child_vtable: &Vec<Rc<Method>>, parent_vtable: &Vec<Rc<Method>>) -> Vec<Rc<Method>> {
		// Merge two vtables and maintain orders
		let mut result = vec![];
		let mut mask = BTreeSet::new();
		for method in parent_vtable.iter() {
			if Class::has_definition_for(&child_vtable, method) {
				result.push(method.clone());
				mask.insert(Class::get_method_idx_from_vtable(&child_vtable, method).unwrap());
			} else {
				result.push(method.clone());
			}
		}
		for (ix, method) in child_vtable.iter().enumerate() {
			if !mask.contains(&ix) {
				result.push(method.clone());
			}
		}
		result
	}

	fn merge_fields(inh_fields: &Vec<Rc<Field>>, fields: &Vec<Rc<Field>>) -> Vec<Rc<Field>> {
		let mut result = vec![];
		result.extend(inh_fields.iter().map(|x| x.clone()));
		result.extend(fields.iter().map(|x| x.clone()));
		result
	}

	pub fn set_super(self: Rc<Class>, supr: Rc<Class>) {
		// supr must be a resolved class
		match *supr.sup.borrow() {
			None if supr.name != "Object" => panic!("Super class is not resolved"),
			_ => {}
		}

		// Set super
		*self.sup.borrow_mut() = Some(supr.clone());

		// Set vtable and inherit fields
		let new_vtable = Self::merge_vtable(&*self.vtable.borrow(), &*supr.vtable.borrow());
		*self.vtable.borrow_mut() = new_vtable;
		let new_inh_fields = Self::merge_fields(&*supr.inh_fields.borrow(), &*supr.fields.borrow());
		*self.inh_fields.borrow_mut() = new_inh_fields;
	}
}
