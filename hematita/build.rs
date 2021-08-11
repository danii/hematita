//! This file mainly just runs different build scripts for integration tests.

use self::tests::matches_standard;

pub mod tests {
	use std::env::{current_dir, var};

	pub mod matches_standard {
		include!("./tests/matches_standard/build.rs");
	}

	pub fn matches_standard() {
		let build = var("OUT_DIR").unwrap();
		let current = current_dir().unwrap().join("tests/matches_standard/");
		matches_standard::build(&current, build.as_ref());
	}
}

fn main() {
	matches_standard();
}
