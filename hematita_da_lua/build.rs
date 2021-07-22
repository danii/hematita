use self::tests::matches_standard;

pub mod tests {
	use std::env::current_dir;

	pub mod matches_standard {
		include!("./tests/matches_standard/build.rs");
	}

	pub fn matches_standard() {
		let current = current_dir().unwrap().join("tests/matches_standard/");
		matches_standard::build(&current);
	}
}

fn main() {
	matches_standard();
}
