use crate::lnp::ast as Past;
use std::collections::BTreeMap;
use std::rc::{Rc, Weak};

pub enum DecafType {
	ClassType(Class),
	IntType,
	CharType,
	StringType,
	VoidType,
	ArrayType(Box<DecafType>),
}

pub enum DecafValueKind {
	Value,
	Location,
}

pub struct DecafValue {
	llvmval: LLVMValueRef,
	ty: DecafType,
	kind; DecafValueKind,
}

trait AST {
	fn gencode() -> Result<DecafValue>;
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

				// Move builder position to next block
				LLVMPositionBuilderAtEnd(llvm.builder, nextbb;)

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

				// Move builder position to next block
				LLVMPositionBuilderAtEnd(llvm.builder, nextbb);
			}
			// Return a dummy value
			Ok(LLVMConstant(LLVMInt32Type(), 0, false));
		}
	}
}

impl GenCode for WhileStmt {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<LLVMValueRef> {

		// The idea is to create three basic blocks
		// Block 1 is for condition evaluation and conditional branching
		// Block 2 is for loop body

		unsafe {
			// Get function
			let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(llvm.builder));

			// Create the aforementioned basic blocks
			let condbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"whilecond\0".as_ptr() as *const _);
			let loopbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"whilebody\0".as_ptr() as *const _);
			let nextbb = LLVMAppendBasicBlockInContext(llvm.ctx, func, b"next\0".as_ptr() as *const _);

			// Build instructions for condbb
			LLVMPositionBuilderAtEnd(llvm.builder, condbb);

			let condval = self.condition.gencode()?;

			match self.condition.etype() {
				ExprType::Location => {
					let condval = LLVMBuildLoad(llvm.builder, condval, b"condval\0".as_ptr() as *const _);
				}
				_ => {}
			}

			// Generate comparison instruction according to its type
			// We treat any value that's not 0 as true
			match LLVMGetIntTypeWidth(LLVMTypeOf(condval)) {
				1 => {
					LLVMBuildICmp(llvm.builder,
								  LLVMIntPredicate::LLVMIntNE,
								  condval,
								  LLVMConstant(LLVMInt1Type(), 0, false));
				},
				32 => {
					LLVMBuildICmp(llvm.builder,
								  LLVMIntPredicate::LLVMIntNE,
								  condval,
								  LLVMConstant(LLVMInt32Type(), 0, false));
				},
				_ => {
					return Err(SemanticError::InvalidPredicateType);
				}
			}

			LLVMBuildCondBr(llvm.builder, condval, loopbb, nextbb);

			// Add the current loop info to stack
			llvm.add_loopinfo(condbb, loopbb, nextbb);

			// Build instructions for loopbb
			LLVMPositionBuilderAtEnd(llvm.builder, loopbb);
			self.body.gencode()?;

			// Move builder position to next block
			LLVMPositionBuilderAtEnd(llvm.builder, nextbb);

			// Dummy value
			Ok(LLVMConstant(LLVMInt32Type(), 0, false));
		}
	}
}

impl GenCode for ContinueStmt {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<LLVMValueRef> {
		unsafe {
			let loopinfo = llvm.pop_loopinfo();
			LLVMBuildBr(llvm.builder, loopinfo.condbb);
		}
	}
}

impl GenCode for BreakStmt {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<LLVMValueRef> {
		unsafe {
			let loopinfo = llvm.pop_loopinfo();
			LLVMBuildBr(llvm.builder, loopinfo.nextbb);
		}
	}
}


impl GenCode for MethodCall {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<DecafValue> {
		unsafe {
			// Get parent function
			let func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(llvm.builder));

			match self.subject {
				ClassSubject(cls) => {
					// TODO: (In type checker) Check if this is a static
					// method of cls or ant of its super class
					// For now we assume type checker is already done

					// We assume all functions resides in vtable
					// (regardless of whether it's virtual or not)

					// Evaluate all arguments first
					let mut vs = vec![];
					for expr in self.args.iter() {
						vs.push(expr.gencode()?);
					}

					// Get vtable location
					let mut num_meta: u32 = DECAF_METADATA_NUM;
					let metadata = LLVMGlobalCopyAllMetadata(cls.structval, &mut num_meta as *mut _);
					let vtable = LLVMValueMetadataEntriesGetMetadata(metedata, 0);
					let vtable = LLVMMetadataAsValue(llvm.ctx, vtable);
					// TODO: Verify kind
					let num_entries = LLVMGetMDNodeNumOperands(vtable);
					let mut vtable_entries: Vec<LLVMValueRef> = Vec::with_capacity(num_entries);
					LLVMGetMDNodeOperands(vtable, vtable_entries.as_mut_ptr() as *mut _);

					// Lookup method index
					let method_ix = cls.get_method_ix(self.name)?;
					let val = LLVMBuildCall(llvm.builder,
											vtable_entries[method_ix],
											vs.as_slice().as_mut_ptr(),
											vs.len(),
											b"static_method_call\0".as_ptr() as *const _);  // TODO: use "static_method_call" + name

					return val;

				},
				ObjectSubject(obj) => {
					let mut vs = vec![obj.thisval];
					for expr in self.args.iter() {
						vs.push(expr.gencode()?);
					}

					// Get vtable location
					let mut num_meta: u32 = DECAF_METADATA_NUM;
					let metadata = LLVMGlobalCopyAllMetadata(obj.structval, &mut num_meta as *mut _);
					let vtable = LLVMValueMetadataEntriesGetMetadata(metadata, 0);
					let num_entries = LLVMGetMDNodeNumOperands(vtable);
					let mut vtable_entries: Vec<LLVMValueRef> = Vec::with_capacity(num_entries);  // FIXME: we may have to put dummy data in there
					LLVMGetMDNodeOperands(vtable, vtable_entries.as_mut_ptr() as *mut _);
					let method_ix = cls.get_method_ix(self.name)?;
					let val = LLVMBuildCall(llvm.builder,
											vtable_entries[method_ix],
											vs.as_slice().as_mut_ptr(),
											vs.len(),
											b"object_method_call\0".as_ptr() as *const _);  // TODO: put name in there
				},
				SuperSubject(cls) => {
					// Note: cls is the immediate parent class of "this"

					// And this can only be an object method call,
					// since we do not allow "super" to appear in a
					// static call

					// In a valid AST, cls will have the method, so we
					// don't have to traverse the parent tree

					let mut vs = vec![obj.thisval];
					for expr in self.args.iter() {
						vs.push(expr.gencode()?);
					}

					// Get vtable location
					let mut num_meta: u32 = DECAF_METADATA_NUM;
					let metadata = LLVMGlobalCopyAllMetadata(cls.structval, &mut num_meta as *mut _);
					let vtable = LLVMValueMetadataEntriesGetMetadata(metedata, 0);
					let vtable = LLVMMetadataAsValue(llvm.ctx, vtable);
					// TODO: Verify kind
					let num_entries = LLVMGetMDNodeNumOperands(vtable);
					let mut vtable_entries: Vec<LLVMValueRef> = Vec::with_capacity(num_entries);
					LLVMGetMDNodeOperands(vtable, vtable_entries.as_mut_ptr() as *mut _);

					// Lookup method index
					let method_ix = cls.get_method_ix(self.name)?;
					let val = LLVMBuildCall(llvm.builder,
											vtable_entries[method_ix],
											vs.as_slice().as_mut_ptr(),
											vs.len(),
											b"static_method_call\0".as_ptr() as *const _);  // TODO: use "static_method_call" + name

					return val;
				},
				MethodCallSubject(m) => {
					// First generate code for m

					// In a valid AST, the value returned from
					// m.gencode() is the location of the object
					unsafe {
						let mut val = m.gencode(llvm);
						let obj = val.get_object()?;

						let mut vs = vec![obj.thisval];
						for expr in self.args.iter() {
							vs.push(expr.gencode()?);
						}

						// Get vtable location
						let mut num_meta: u32 = DECAF_METADATA_NUM;
						let metadata = LLVMGlobalCopyAllMetadata(obj.structval, &mut num_meta as *mut _);
						let vtable = LLVMValueMetadataEntriesGetMetadata(metadata, 0);
						let num_entries = LLVMGetMDNodeNumOperands(vtable);
						let mut vtable_entries: Vec<LLVMValueRef> = Vec::with_capacity(num_entries);  // FIXME: we may have to put dummy data in there
						LLVMGetMDNodeOperands(vtable, vtable_entries.as_mut_ptr() as *mut _);
						let method_ix = cls.get_method_ix(self.name)?;
						let val = LLVMBuildCall(llvm.builder,
												vtable_entries[method_ix],
												vs.as_slice().as_mut_ptr(),
												vs.len(),
												b"object_method_call\0".as_ptr() as *const _);  // TODO: put name in there
					}
				},
			}
		}
	}
}

impl GenCode for NewExpr {
	fn gencode(&mut self, &mut llvm: CompilerConstruct) -> Res<DecafValue> {
		// Malloc
	}
}
