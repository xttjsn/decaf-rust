pub trait Normalize {
	fn normalize(self) -> Self;
}

impl<R: Normalize> Normalize for Vec<R> {
	fn normalize(self) -> Self {
		self.into_iter().map(Normalize::normalize).collect()
	}
}
