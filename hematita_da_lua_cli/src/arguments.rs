pub use pico_args::Error;
use pico_args::Arguments as Parser;
use std::path::PathBuf;

pub static HELP: &str = "\
Lua 5.4.3 Interpeter

The Hematita Da Lua interpreter is licensed under MIT.
The Lua programming language is owned and copyrighted by PUC-Rio.

SYNOPSIS:
	lua [OPTIONS] [SOURCE [ARGS]]

DESCRIPTION:
	lua is a standalone Lua interpeter. It parsed and compiles Lua source code to bytecode, then executes said bytecode.

	This is not the standard Lua interpreter, and is known as Hematita Da Lua. It is a safe and secure sandbox for running Lua code. It's written in safe Rust, garunteeing memory safety.

	After parsing the command line arguments, SOURCE is either evaluated directly, if -e is passed, otherwise the file named SOURCE is loaded and evaluated. If -i is passed, after evaluation, an interactive prompt with SOURCE's state is started. If SOURCE is not provided, an interactive prompt always starts.

	Passing -v will print verbose information to standard error.


OPTIONS:
	-i, --help        Displays this and quits
	-V, --version     Displays version information
	-v, --verbose     Runs with verbose output
	-i, --interactive Runs in interactive mode, after running SOURCE
	-e, --evaluate    Treats source as direct source code, rather than a file";

#[derive(Debug)]
pub enum Arguments {
	Help,
	Version,
	File(PathBuf, bool),
	Inline(String, bool)
}

impl Arguments {
	pub fn from_env() -> Result<Self, Error> {
		let mut parser = Parser::from_env();

		if parser.contains(["-h", "--help"]) {return Ok(Self::Help)}
		if parser.contains(["-V", "--version"]) {return Ok(Self::Version)}

		let interactive = parser.contains(["-i", "--interactive"]);
		Ok(if parser.contains(["-e", "--evaluate"]) {
			Self::Inline(parser.free_from_str()?, interactive)
		} else {
			Self::File(parser.free_from_str()?, interactive)
		})
	}
}
