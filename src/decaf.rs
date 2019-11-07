struct Class {}

impl Class {
	fn set_super(self: Rc<Self>, supr: Type) {
		match supr {
			Type::Class(suprc) => {
				// Set the super field
				self.clone().supr = supr.clone();

				// Try to resolve unresolved method calls
				for method in suprc.pub_methods.iter() {
					for hook in self.clone().get_method_resolution_hooks(method.name).into_iter() {
						hook(method.clone());
					}
				}

				for method in suprc.prot_methods.iter() {
					for hook in self.clone().get_method_resolution_hooks(method.name).into_iter() {
						hook(method.clone());
					}
				}

				match suprc.supr {
					Type::Unknown(grandparent_name) => {
						// There might be method calls that we haven't
						// been able to resolve for this grandchild
						// class. So we must add a new hook for it.
						self.add_type_resolution_hook(grandparent_name,
													  move |ty| {
														  self.clone().set_ancestor(ty);
													  });
					}
				}
			},
			_ => panic!("cannot extend non class type")
		}

	}
}
