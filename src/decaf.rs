use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;
use crate::lnp;

use llvm_sys::prelude::*;
use lnp::past::{BinOp, UnOp};

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
	Pub,
	Priv,
	Prot,
}

impl fmt::Display for Visibility {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use Visibility::*;
		match self {
			Pub => {
				write!(f, "pub");
			}
			Prot => {
				write!(f, "prot");
			}
			Priv => {
				write!(f, "priv");
			}
		}
		Ok(())
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
	pub base: TypeBase,
	pub array_lvl: u32,
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use TypeBase::*;
		match &self.base {
			UnknownTy(name) => {
				write!(f, "{}_{}", name, self.array_lvl);
			}
			BoolTy => {
				write!(f, "bool_{}", self.array_lvl);
			}
			IntTy => {
				write!(f, "int_{}", self.array_lvl);
			}
			CharTy => {
				write!(f, "char_{}", self.array_lvl);
			}
			StrTy => {
				write!(f, "str_{}", self.array_lvl);
			}
			VoidTy => {
				write!(f, "void_{}", self.array_lvl);
			}
			NULLTy => {
				write!(f, "null_{}", self.array_lvl);
			}
			ClassTy(cls) => {
				write!(f, "{}_{}", cls.name, self.array_lvl);
			}
		}
		Ok(())
	}
}

impl Type {
	pub fn is_compatible_with(&self, rhs: &Type) -> bool {
		use TypeBase::*;
		match self.array_lvl == rhs.array_lvl {
			false => false,
			true => {
				match (&self.base, &rhs.base) {
					(UnknownTy(_), _) | (_, UnknownTy(_)) => false,
					_ => {
						match &self.base == &rhs.base {
							true => true,
							false => {
								match (&self.base, &rhs.base) {
									(ClassTy(_), NULLTy) | (NULLTy, ClassTy(_)) => true,
									_ => false
								}
							}
						}
					}
				}
			}
		}
	}

	pub fn supports_arith_ops(&self) -> bool {
		use TypeBase::*;
		if self.array_lvl == 0 {
			match &self.base {
				UnknownTy(_) | BoolTy | CharTy | VoidTy | NULLTy | ClassTy(_) | StrTy => false,
				IntTy => true,
			}
		} else {
			false
		}
	}

	pub fn supports_logical_ops(&self) -> bool {
		use TypeBase::*;
		if self.array_lvl == 0 {
			match &self.base {
				UnknownTy(_) | IntTy | CharTy | VoidTy | NULLTy | ClassTy(_) | StrTy => false,
				BoolTy => true,
			}
		} else {
			false
		}
	}

	pub fn supports_cmp_ops(&self) -> bool {
		use TypeBase::*;
		if self.array_lvl == 0 {
			match &self.base {
				UnknownTy(_)  | VoidTy | NULLTy | ClassTy(_) | StrTy => false,
				IntTy | CharTy | BoolTy => true,
			}
		} else {
			false
		}
	}

	pub fn supports_eq_ops(&self) -> bool {
		use TypeBase::*;
		if self.array_lvl == 0 {
			match &self.base {
				UnknownTy(_)  | VoidTy => false,
				IntTy | CharTy | BoolTy | ClassTy(_) | NULLTy | StrTy => true,
			}
		} else {
			false
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeBase {
	UnknownTy(String),
	BoolTy,
	IntTy,
	CharTy,
	StrTy,
	VoidTy,
	NULLTy,
	ClassTy(Rc<Class>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
	pub name: String,
	pub cls: RefCell<Rc<Class>>,
	pub vis: RefCell<Visibility>,
	pub stat: RefCell<bool>,
	pub return_ty: RefCell<Rc<Type>>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>,
	pub body: RefCell<Option<Rc<BlockStmt>>>,
}

impl Method {

	pub fn new(name: String, cls: Rc<Class>) -> Self {
		let unknowncls = Rc::new(Type {
			base: TypeBase::UnknownTy("Unknown".to_string()),
			array_lvl: 0,
		});
		Method {
			name,
			cls: RefCell::new(cls),
			vis: RefCell::new(Visibility::Pub),
			stat: RefCell::new(false),
			return_ty: RefCell::new(unknowncls.clone()),
			args: RefCell::new(vec![]),
			body: RefCell::new(None),
		}
	}

	pub fn add_arg(&self, arg_name: String, arg_ty: Rc<Type>) {
		// Check name conflicts
		if self.args.borrow().iter().any(|(name, _ty)| name == &arg_name) {
			panic!("argument name conflict in method")
		}
		self.args.borrow_mut().push((arg_name, arg_ty));
	}

	pub fn set_arg_type(&self, arg_name: String, arg_ty: Rc<Type>) {
		if !self.args.borrow().iter().any(|(name, _ty)| name == &arg_name) {
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
	pub vis: RefCell<Visibility>,
	pub args: RefCell<Vec<(String, Rc<Type>)>>,
	pub body: RefCell<Option<Rc<BlockStmt>>>,
}

impl Ctor {
	pub fn new(cls: Rc<Class>) -> Self {
		Ctor {
			cls,
			vis: RefCell::new(Visibility::Pub),
			args: RefCell::new(vec![]),
			body: RefCell::new(None),
		}
	}

	pub fn add_arg(&self, arg_name: String, arg_ty: Rc<Type>) {
		// Check name conflicts
		if self.args.borrow().iter().any(|(name, _ty)| name == &arg_name) {
			panic!("argument name conflict in method")
		}
		self.args.borrow_mut().push((arg_name, arg_ty));
	}

	pub fn set_arg_type(&self, arg_name: String, arg_ty: Rc<Type>) {
		if !self.args.borrow().iter().any(|(name, _ty)| name == &arg_name) {
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
pub struct Block {
}

pub trait Value {
	fn addressable(&self) -> bool;
	fn get_type(&self) -> Type;
}

pub trait Returnable {
	fn returnable(&self) -> Option<Type>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
	pub name: String,
	pub ty: Type,
	pub addr: Option<LLVMValueRef>,
}

impl Value for Variable {
	fn addressable(&self) -> bool {
		true
	}

	fn get_type(&self) -> Type {
		self.ty.clone()
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
	Declare(DeclareStmt),
	If(IfStmt),
	IfElse(IfElseStmt),
	Expr(ExprStmt),
	While(Rc<WhileStmt>),
	Return(ReturnStmt),
	Continue(ContinueStmt),
	Break(BreakStmt),
	Super(SuperStmt),
	Block(Rc<BlockStmt>),
	NOP,
}

impl Returnable for Stmt {
	fn returnable(&self) -> Option<Type> {
		use Stmt::*;
		match self {
			NOP => None,
			Declare(_) => None,
			If(_) => None,
			IfElse(stmt) => {
				match Block(stmt.thenblock.clone()).returnable() {
					None => None,
					Some(thenty) => match Block(stmt.elseblock.clone()).returnable() {
						None => None,
						Some(elsety) => {
							if thenty != elsety {
								panic!("return type differs in thenblock and elseblock");
							}
							Some(thenty)
						}
					}
				}
			}
			Expr(_) => None,
			While(_) => None, // We don't know at compile time if while body will be executed at all
			Return(stmt)=> {
				match &stmt.expr {
					None => None,
					Some(expr) => Some(expr.get_type())
				}
			}
			Continue(_) => None,
			Break(_) => None,
			Super(_) => None,
			Block(blkstmt) => {
				blkstmt.stmts.borrow().iter().rev().find_map(|stmt| stmt.returnable())
			}
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclareStmt {
	pub name: String,
	pub ty: Type,
	pub init_expr: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStmt {
	pub cond: Expr,
	pub thenblock: Rc<BlockStmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfElseStmt {
	pub cond: Expr,
	pub thenblock: Rc<BlockStmt>,
	pub elseblock: Rc<BlockStmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprStmt {
	pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStmt {
	pub condexpr: Expr,
	pub bodyblock: RefCell<Option<Rc<BlockStmt>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStmt {
	pub expr: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueStmt {
	pub lup: Rc<WhileStmt> // loop
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakStmt {
	pub lup: Rc<WhileStmt> // loop
}

#[derive(Debug, PartialEq, Clone)]
pub struct SuperStmt {
	pub sup: Rc<Class>,
	pub sup_ctor: Rc<Ctor>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStmt {
	pub vartbl: RefCell<Vec<Rc<Variable>>>,
	pub stmts: RefCell<Vec<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
	Assign(AssignExpr),
	BinArith(BinArithExpr),
	UnArith(UnArithExpr),
	BinLogical(BinLogicalExpr),
	UnNot(UnNotExpr),
	BinCmp(BinCmpExpr),
	CreateArray(CreateArrayExpr),
	Literal(LiteralExpr),
	This(ThisExpr),
	NULL,
	CreateObj(CreateObjExpr),
	MethodCall(MethodCallExpr),
	SuperCall(SuperCallExpr),
	ArrayAccess(ArrayAccessExpr),
	FieldAccess(FieldAccessExpr),
	Variable(VariableExpr),
	ClassId(ClassIdExpr),
}

impl Value for Expr {
	fn addressable(&self) -> bool {
		use Expr::*;
		// only a tentative implementation
		match self {
			Assign(_) => false,
			BinArith(_) => false,
			UnArith(_) => false,
			BinLogical(_) => false,
			UnNot(_) => false,
			BinCmp(_) => false,
			CreateArray(_) => false,
			Literal(_) => false,
			This(_) => true,
			NULL => false,
			CreateObj(_) => false,
			MethodCall(_) => false,
			SuperCall(_) => false,
			ArrayAccess(_) => true,
			FieldAccess(_)=> true,
			Variable(_) => true,
			ClassId(_) => false,
		}
	}

	fn get_type(&self) -> Type {
		use Expr::*;
		use TypeBase::*;
		use LiteralExpr::*;
		// Perform no checks
		match self {
			Assign(_) => Type {base: VoidTy, array_lvl: 0},
			BinArith(_) => Type {base: IntTy, array_lvl: 0},
			UnArith(_) => Type {base: IntTy, array_lvl: 0},
			BinLogical(_) => Type {base: BoolTy, array_lvl: 0},
			UnNot(_) => Type {base: BoolTy, array_lvl: 0},
			BinCmp(_) => Type {base: BoolTy, array_lvl: 0},
			CreateArray(expr) => Type {base: expr.ty.clone(), array_lvl: expr.dims.len() as u32},
			Literal(expr) => match expr {
				IntLiteral(_) => Type {base: IntTy, array_lvl: 0},
				BoolLiteral(_) => Type {base: BoolTy, array_lvl: 0},
				CharLiteral(_) => Type {base: CharTy, array_lvl: 0},
				StrLiteral(_) => panic!("not implemented"),
			},
			This(expr) => Type {base: ClassTy(expr.cls.clone()), array_lvl: 0},
			NULL => Type {base: NULLTy, array_lvl: 0},
			CreateObj(expr) => Type {base: ClassTy(expr.cls.clone()), array_lvl: 0},
			MethodCall(expr) => Type {
				base: expr.method.return_ty.borrow().base.clone(),
				array_lvl: expr.method.return_ty.borrow().array_lvl,
			},
			SuperCall(expr) => Type {
				base: expr.method.return_ty.borrow().base.clone(),
				array_lvl: expr.method.return_ty.borrow().array_lvl,
			},
			ArrayAccess(expr) => {
				let ty = expr.var.get_type();
				if ty.array_lvl < 1 {
					panic!("ArrayAccess with bad array_lvl: {}", ty.array_lvl);
				}
				Type {base: ty.base, array_lvl: ty.array_lvl - 1}
			}
			FieldAccess(expr) => Type {
				base: expr.fld.ty.borrow().base.clone(),
				array_lvl: expr.fld.ty.borrow().array_lvl,
			},
			Variable(expr) => expr.var.ty.clone(),
			ClassId(_) => panic!("ClassId has no type"),
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignExpr {
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinArithExpr {
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
	pub op: BinOp,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnArithExpr {
	pub rhs: Box<Expr>,
	pub op: UnOp,
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnNotExpr {
	pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinLogicalExpr {
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
	pub op: BinOp,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinCmpExpr {
	pub lhs: Box<Expr>,
	pub rhs: Box<Expr>,
	pub op: BinOp,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateArrayExpr {
	pub ty: TypeBase,
	pub dims: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralExpr {
	IntLiteral(i64),
	BoolLiteral(bool),
	CharLiteral(char),
	StrLiteral(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ThisExpr {
	pub cls: Rc<Class>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateObjExpr {
	pub cls: Rc<Class>,
	pub ctor: Option<Rc<Ctor>>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MethodCallExpr {
	pub var: Box<Expr>,
	pub method: Rc<Method>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StaticMethodCallExpr {
	pub cls: Rc<Class>,
	pub method: Rc<Method>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SuperCallExpr {
	pub cls: Rc<Class>,
	pub method: Rc<Method>,
	pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayAccessExpr {
	pub var: Box<Expr>,
	pub idx: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldAccessExpr {
	pub var: Box<Expr>,
	pub cls: Rc<Class>,
	pub fld: Rc<Field>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableExpr {
	pub var: Rc<Variable>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassIdExpr {
	pub cls: Rc<Class>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
	ClassScope(Rc<Class>),
	CtorScope(Rc<Ctor>),
	MethodScope(Rc<Method>),
	BlockScope(Rc<BlockStmt>),
	WhileScope(Rc<WhileStmt>),
	GlobalScope,
}

impl Scope {
	pub fn variable_lookup(&self, name: &str) -> Option<Rc<Variable>> {
		use Scope::*;
		match self {
			// TODO: implement fields
			ClassScope(_) | CtorScope(_) | MethodScope(_) | WhileScope(_) | GlobalScope => None,
			BlockScope(blk) => {
				for v in blk.vartbl.borrow().iter() {
					if v.name == name {
						return Some(v.clone());
					}
				}
				None
			}
		}
	}

	pub fn variable_add(&self, name: String, ty: Type) {
		use Scope::*;
		match self {
			ClassScope(_) | CtorScope(_) | MethodScope(_) | WhileScope(_) | GlobalScope =>
				panic!("variable_add is supported only for block scope"),
			BlockScope(blk) => {
				if blk.vartbl.borrow().iter().any(|v| v.name == name.as_str()) {
					panic!("variable {:?} is already defined", name)
				}
				let var = Rc::new(Variable {
					name,
					ty,
					None,
				});
				blk.vartbl.borrow_mut().push(var);
			}
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
	pub name: String,
	pub ty: RefCell<Rc<Type>>,
	pub init_expr: RefCell<Option<Expr>>,
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

	pub fn set_init_expr(&self, expr: Expr) {
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
	pub vtable: RefCell<Vec<Rc<Method>>>,
}

fn prepend<T>(v: &[T], s: &[T]) -> Vec<T>
where
	T: Clone,
{
	let mut tmp: Vec<_> = s.to_owned();
	tmp.extend(v.to_owned());
	tmp
}

pub trait VTable {
	fn add_vmethod(&mut self, method: &Rc<Method>);
}

impl VTable for Vec<Rc<Method>> {
	fn add_vmethod(&mut self, method: &Rc<Method>) {
		// TODO: check that all methods vtable have different signature
		match self.iter().position(|m| m.has_same_signature(method)) {
			Some(ix) => {
				self[ix] = method.clone();
			}
			None => {
				self.push(method.clone());
			}
		}

	}
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
			vtable: RefCell::new(vec![]),
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
			None if supr.name != "Object" => panic!("Super class {} is not resolved", supr.name),
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
		for supr_prot_method in supr.prot_methods.borrow().iter() {
			self.check_prot_method_defined_as_nonprot(supr_prot_method).unwrap();
		}
		let prot_methods = prepend(&self.prot_methods.borrow()[..], &supr.prot_methods.borrow()[..]);
		*self.prot_methods.borrow_mut() = prot_methods;

		// Inherit vtable
		let mut supr_vtable = supr.vtable.borrow().clone();
		for method in self.vtable.borrow().iter() {
			supr_vtable.add_vmethod(method);
		}
		*self.vtable.borrow_mut() = supr_vtable;
	}

	pub fn get_field_by_name(&self, name: &String) -> Option<Rc<Field>> {
		for field in self.pub_fields.borrow().iter() {
			if field.name == *name {
				return Some(field.clone())
			}
		}

		for field in self.priv_fields.borrow().iter() {
			if field.name == *name {
				return Some(field.clone())
			}
		}

		for field in self.prot_fields.borrow().iter() {
			if field.name == *name {
				return Some(field.clone())
			}
		}

		None
	}

	pub fn get_method_by_signature(&self, return_ty: &TypeBase, array_lvl: u32,
								   args: &Vec<(String, TypeBase, u32)>, is_static: bool, name: &str) -> Option<Rc<Method>> {
		println!("get_method_by_signature: \n return_ty: {:?}\n array_lvl: {}\n args: {:?}\n is_static: {}", return_ty, array_lvl, args, is_static);
		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}
				if method.return_ty.borrow().base != *return_ty ||
					method.return_ty.borrow().array_lvl != array_lvl {
						continue;
					}

				for (arga, argb) in method.args.borrow().iter().zip(args.iter()) {
					if arga.0 != argb.0 || arga.1.base != argb.1 || arga.1.array_lvl != argb.2 {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};
		if is_static {
			match go(&self.pub_static_methods.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.prot_static_methods.borrow()) {
					Some(m) => Some(m),
					None => match go(&self.priv_static_methods.borrow()) {
						Some(m) => Some(m),
						None => None
					}
				}
			}
		} else {
			match go(&self.pub_methods.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.prot_methods.borrow()) {
					Some(m) => Some(m),
					None => match go(&self.priv_methods.borrow()) {
						Some(m) => Some(m),
						None => None,
					}
				}
			}
		}
	}

	pub fn get_ctor_by_signature(&self, args: &Vec<(String, TypeBase, u32)>) -> Option<Rc<Ctor>> {
		let go = |ctors: &Vec<Rc<Ctor>>| {
			'outer: for ctor in ctors.iter() {
				for (arga, argb) in ctor.args.borrow().iter().zip(args.iter()) {
					if arga.0 != argb.0 || arga.1.base != argb.1 || arga.1.array_lvl != argb.2 {
						continue 'outer;
					}
				}
				return Some(ctor.clone());
			}
			None
		};
		match go(&self.pub_ctors.borrow()) {
			Some(m) => Some(m),
			None => match go(&self.prot_ctors.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.priv_ctors.borrow()) {
					Some(m) => Some(m),
					None => None,
				}
			}
		}
	}

	pub fn get_compatible_super_ctor(&self, args: &Vec<Expr>) -> Option<Rc<Ctor>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();
		let go = |ctors: &Vec<Rc<Ctor>>| {
			'outer: for ctor in ctors.iter() {
				for (arga, ty) in ctor.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(ctor.clone());
			}
			None
		};

		match &*self.sup.borrow() {
			None => None,
			Some(sup_cls) => {
				match go(&sup_cls.pub_ctors.borrow()) {
					Some(m) => Some(m),
					None => match go(&sup_cls.prot_ctors.borrow()) {
						Some(m) => Some(m),
						None => None,
					}
				}
			}
		}
	}

	pub fn get_compatible_level0_ctor(&self, args: &Vec<Expr>) -> Option<Rc<Ctor>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();
		let go = |ctors: &Vec<Rc<Ctor>>| {
			'outer: for ctor in ctors.iter() {
				'inner: for (arga, ty) in ctor.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(ctor.clone());
			}
			None
		};

		match go(&self.pub_ctors.borrow()) {
			Some(m) => Some(m),
			None => match go(&self.prot_ctors.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.priv_ctors.borrow()) {
					Some(m) => Some(m),
					None => None,
				},
			}
		}
	}

	pub fn get_compatible_level1_ctor(&self, args: &Vec<Expr>) -> Option<Rc<Ctor>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();
		let go = |ctors: &Vec<Rc<Ctor>>| {
			'outer: for ctor in ctors.iter() {
				for (arga, ty) in ctor.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(ctor.clone());
			}
			None
		};

		match go(&self.pub_ctors.borrow()) {
			Some(m) => Some(m),
			None => None
		}
	}

	pub fn get_compatible_level0_method(&self, args: &Vec<Expr>, name: &str) -> Option<Rc<Method>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();

		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}
				for (arga, ty) in method.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};

		match go(&self.pub_methods.borrow()) {
			Some(m) => Some(m),
			None => match go(&self.prot_methods.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.priv_methods.borrow()) {
					Some(m) => Some(m),
					None => None,
				}
			}
		}
	}

	pub fn get_compatible_level1_method(&self, args: &Vec<Expr>, name: &str) -> Option<Rc<Method>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();

		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}

				for (arga, ty) in method.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};

		match go(&self.pub_methods.borrow()) {
			Some(m) => Some(m),
			None => None,
		}
	}

	pub fn get_compatible_level0_static_method(&self, args: &Vec<Expr>, name: &str) -> Option<Rc<Method>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();

		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}
				for (arga, ty) in method.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};

		match go(&self.pub_static_methods.borrow()) {
			Some(m) => Some(m),
			None => match go(&self.prot_static_methods.borrow()) {
				Some(m) => Some(m),
				None => match go(&self.priv_static_methods.borrow()) {
					Some(m) => Some(m),
					None => None,
				}
			}
		}
	}

	pub fn get_compatible_level1_static_method(&self, args: &Vec<Expr>, name: &str) -> Option<Rc<Method>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();

		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}
				for (arga, ty) in method.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};

		match go(&self.pub_static_methods.borrow()) {
			Some(m) => Some(m),
			None => None,
		}
	}

	pub fn get_compatible_level0_super_method(&self, args: &Vec<Expr>, name: &str) -> Option<Rc<Method>> {
		let tys: Vec<Type> = args.iter().map(|e| e.get_type()).collect();

		let go = |methods: &Vec<Rc<Method>>| {
			'outer: for method in methods.iter() {
				if method.name != name {
					continue;
				}
				for (arga, ty) in method.args.borrow().iter().zip(tys.iter()) {
					if arga.1.base != ty.base || arga.1.array_lvl != ty.array_lvl {
						continue 'outer;
					}
				}
				return Some(method.clone());
			}
			None
		};

		match &*self.sup.borrow() {
			None => None,
			Some(sup_cls) => {
				match go(&sup_cls.pub_methods.borrow()) {
					Some(m) => Some(m),
					None => match go(&sup_cls.prot_methods.borrow()) {
						Some(m) => Some(m),
						None => None,
					}
				}
			}
		}
	}

	pub fn get_level0_field_by_name(&self, name: &str) -> Option<Rc<Field>> {
		match self.pub_fields.borrow().iter().find(|f| f.name == name) {
			Some(f) => Some(f.clone()),
			None => match self.prot_fields.borrow().iter().find(|f| f.name == name) {
				Some(f) => Some(f.clone()),
				None => None
			}
		}
	}

	pub fn get_level1_field_by_name(&self, name: &str) -> Option<Rc<Field>> {
		match self.pub_fields.borrow().iter().find(|f| f.name == name) {
			Some(f) => Some(f.clone()),
			None => None,
		}
	}
}

// #[derive(Debug, PartialEq, Clone)]
// pub enum Expression {
// 	Binary(Box<Expression>, BinOp, Box<Expression>),
// 	Unary(UnOp, Box<Expression>),
// 	NewClassArray(Rc<Class>, Vec<Expression>),
// 	NewPrimitive(Primitive, Vec<Expression>),
// 	Literal(Litr),
// 	This,
// 	NewObject(Rc<Class>, Vec<Expression>),
// 	MethodCall(Rc<ClassInst>, Vec<Expression>),
// 	SuperCall(Rc<ClassInst>, Vec<Expression>),

// }
