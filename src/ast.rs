use crate::lnp::ast as Past;
use std::collections::BTreeMap;
use std::rc::{Rc, Weak};

trait AST {
	fn gencode() -> Result<Value>;
}

enum Visibility {
	Public,
	Protected,
	Private
}

enum Staticity {
	Static,
	Not,
}


struct Program<'a> {
	classlist: ClassList,
	cls_symtbl: BTreeMap<String, Weak<Class>>,
}

struct ClassList {
	classes: Vec<Rc<Class>>,
}

struct Class {
	vis: Visibility,
	sup: Weak<Class>,
	name: String,
	fields: Vec<Field>,
	methods: Vec<Method>,
	ctor: Option<Ctor>,
	mthd_symtbl: BTreeMap<String, Weak<Method>>,
	fld_symtbl: BTreeMap<String, Weak<Field>>,
}

struct Field {
	vis: Visibility,
	ty: Type,
	init: Option<Expr>,
}

struct Method {
	sta: Staticity,
	vis: Visibility,
	ty: Type,
	formals: Vec<Formal>,
	body: Block,
	var_symtbl: BTreeMap<String, Weak<Variable>>
}

struct Ctor {
	vis: Visibility,
	ty: Type,
	formals: Vec<Formal>,
	body: Block,
	scope: Scope,
}

struct Formal {
	ty: Type,
	name: String,
}

struct Block {
	stmts: Vec<Statement>,
	scope: Scope,
}

struct Scope {
	nmap: BTreeMap<String, Type>,
}

enum Type {
	PrimType(PrimitiveType),
	ClassType(Class),
	ArrayType(Box<Type>),
}

enum PrimitiveType {
	Boolean,
	Char,
	Int,
	Void,
}

struct VarDecl {
	id: String,
	cnt: Option<u32>, // If the id has [%d] suffix
	init: Option<Exp>, // If cnt is present, then
}

enum Statement {
	EmptyStmt,
	DeclStmt(Type, Vec<VarDecl>)
}


pub mod convert {
	pub fn parse_tree_to_ast(ptree Past::Program) -> Program {
		let mut ast_builder = AstBuilder::new();

		p = ptree.accept(ast_builder)?;
	}
}


impl GenCode for IfStmt {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<LLVMValueRef> {
		let cond = self.condition.gencode()?;

		unsafe {
			let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(llvm.builder));
			let ifbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"if\0".as_ptr() as *const _);

			if let Some(else_block) = self.else_block {
				// Create the basic block for else
				let elsebb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"else\0".as_ptr() as *const _);

				// Create the basic block for next
				let nextbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"ifnext\0".as_ptr() as *const _);

				// Generate conditional branch
				LLVMBuildCondBr(llvm.builder, cond, ifbb, elsebb);

				// Move builder position to if block
				LLVMPositionBuilderAtEnd(llvm.builder, ifbb);

				// Generate code for if block
				let if_val = self.if_block.gencode(llvm)?;

				// Branch from the end of if block to next block
				if !self.if_block.has_return() {
					LLVMBuildBr(llvm.builder, nextbb);
				}

				// Move builder position to else block
				LLVMPositionBuilderAtEnd(llvm.builder, elsebb);

				// Generate code for else block
				let else_val = else_block.gencode(llvm)?;

				// Branch from the end of else block to next block
				if !self.else_block.has_return() {
					LLVMBuildBr(llvm.builder, nextbb);
				}

			} else {
				// Create a basic block for next
				let nextbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"ifnext\0".as_ptr() as *const _);

				// Generate conditional branch
				LLVMBuildCondBr(llvm.builder, cond, ifbb, nextbb);

				// Move builder position to if block
				LLVMPositionBuilderAtEnd(llvm.builder, ifbb);

				// Generate code for if block
				let if_val = self.if_block.gencode(llvm)?;

				// Branch from the end of if block to next block
				if !self.if_block.has_return() {
					LLVMBuildBr(llvm.builder, nextbb);
				}
			}
			// Return a dummy value
			Ok(LLVMConstant(LLVMInt32Type(), 0, false));
		}
	}
}

impl GenCode for WhileStmt {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<LLVMValueRef> {

	}
}
