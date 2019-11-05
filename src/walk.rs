impl Visitor {

	pub fn visit_class(c: Class) -> Result<semantic::Class, semantic::SemanticError> {
		// Check if the name of the class is duplicate

		// Put a new entry of symbol for the class to the parent scope

		// If we have a parent node, find it (report error if not
		// already analyzed), copy all of its fields and methods to
		// the current class.

		// Visit each member
	}

	pub fn visit_field(f: Field) -> Result<semantic::Field, semantic::SemanticError> {
		assert!(no_static_modifier(&f.modies,
								   "field cannot be declared static : {}",
								   &f.span));
		assert!(is_modifer_len(&f.modies,
							   1,
							   "field cannot have more than one modifiers : {}",
							   &f.span));

		// Convert modifier visibility
		// if no access modifier is present, assume public)

		// Register all ids in vardecls to be of type ty in the
		// class's filed symbol table

		// In this process, if we haven't seen the typename, mark it
		// as unresolved by putting it into an unresovled type table,
		// but still register it (but keep a reference to the
		// unresolved type table)

		// We may override the field symbol table from the copy of our parent

	}


	pub fn visit_method(m: Method) -> Result<semantic::Field, semantic::SemanticError> {
		// make sure modifier.len() <= 2 && must have a single
		// visibility modifier and an optional static modifier

		// Look at ty, if we haven't seen the type, mark it as
		// unresolved by putting the typename into the unresolved type table.

		// Register the typename of the method call to the type by
		// using a reference to the corresponding type entry.

		// Iterate through fargs, for each argument check the type. If
		// we havn't seen the type, mark it as unresolved; Register
		// the variable to the method's symbol table.

		// Visit the block

		// We may overwrite the method table from the copy of our parent
	}

	pub fn visit_ctor(c: Ctor) -> Res<semantic::Ctor> {
		// make sure modifier is valid

		// make sure the name of the Ctor is the same as the class

		// iterate through formal args, for each argument visit it.

		// Visit the block
	}

}


impl TreeWalk for Class {
	fn walk(self, &mut llvm: CompilerConstruct) -> Self {
		// Assume Class is a valid AST node
		// 2 tasks:
		//     1. Generate an LLVM type for the class
		//     2. Add function for each methods and put them in metadata

		// Fields and private methods
		let	types: Vec<LLVMValueRef> = Vec::with_capcity(self.fields.len() +
													  self.priv_methods.len());
		for field in self.fields.iter() {
			types.push(field.ty);
		}

		for func in self.priv_methods.iter() {
			types.push(func.ty);
		}

		let clstype = LLVMStructTypeInContext(llvm.ctx,
											  types.as_mut_slice().as_mut_ptr() as *mut _,
											  types.len(),
											  true);

		// Pubic and protected methods

		// We assume that at this stage the class node has already
		// inherited all the public and protected methods from its
		// parent
		let funcs: Vec<LLVMValueRef> = Vec::with_capacity(self.pub_methods.len() +
														  self.prot_methods.len());

		for func in self.pub_methods.iter() {
			unsafe {
				let ftype = LLVMFunctionType(func.return_type,
											 func.param_types.as_mut_slice().as_ptr() as *mut _,
											 func.param_count,
											 false);
				let funval = LLVMAddFunction(llvm.module,
											 func.name as *const _,
											 ftype);
				funcs.push(funval);
			}
		}

		for func in self.prot_methods.iter() {
			unsafe {
				let ftype = LLVMFunctionType(func.return_type,
											 func.param_types.as_mut_slice().as_ptr() as *mut _,
											 func.param_count,
											 false);
				let funval = LLVMAddFunction(llvm.module,
											 func.name as *const _,
											 ftype);
				funcs.push(funval);
			}
		}

		// Convert all functions to metadata
		let func_metas = Vec::with_capcity(self.pub_methods.len() +
										   self.prot_methods.len());
		for func in funcs.iter() {
			func_metas.push(LLVMValueAsMetadata(func));
		}

		// Gete MD node
		let func_md = LLVMMDNodeInContext2(llvm.ctx,
										   func_metas.as_mut_slice().as_ptr() as *mut _,
										   func_metas.len());

		// Associate MD node with the class type
		let cls_undef = LLVMGetUndef(clstype);
		LLVMGlobalSetMetadata(cls_undef, DECAF_METADATA_VTABLE_KIND, func_md);
	}
}
