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
