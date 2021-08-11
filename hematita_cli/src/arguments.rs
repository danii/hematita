pub use pico_args::Error;
use pico_args::Arguments as Parser;
use std::{
	convert::TryFrom,
	fs::File,
	io::{Read, Error as IOError},
	path::PathBuf
};

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
	-e, --evaluate    Treats source as direct source code, rather than a file
	-b, --byte-code   Shows byte code rather than executing
	-s, --ast         Shows abstract syntax tree rather than executing
	-t, --tokens      Shows tokens rather than executing";

#[derive(Debug)]
pub enum Arguments {
	ShowHelp,
	ShowVersion,
	Run {
		source: Source,
		execution: ExecutionType
	}
}

#[derive(Debug)]
pub enum Source {
	File(PathBuf),
	Code(String)
}

#[derive(Debug)]
pub enum ExecutionType {
	Run,
	RunInteractively,
	ShowByteCode,
	ShowSyntaxTree,
	ShowTokens
}

impl Arguments {
	pub fn from_env() -> Result<Self, Error> {
		let mut parser = Parser::from_env();

		if parser.contains(["-h", "--help"]) {return Ok(Self::ShowHelp)}
		if parser.contains(["-V", "--version"]) {return Ok(Self::ShowVersion)}

		let interactive = parser.contains(["-i", "--interactive"]);
		let byte_code = parser.contains(["-b", "--byte-code"]);
		let syntax_tree = parser.contains(["-s", "--ast"]);
		let tokens = parser.contains(["-t", "--tokens"]);

		let source = if parser.contains(["-e", "--evaluate"])
				{Source::Code(parser.free_from_str()?)}
			else {Source::File(parser.free_from_str()?)};

		Ok(if interactive {
			Self::Run {source, execution: ExecutionType::RunInteractively}
		} else if byte_code {
			Self::Run {source, execution: ExecutionType::ShowByteCode}
		} else if syntax_tree {
			Self::Run {source, execution: ExecutionType::ShowSyntaxTree}
		} else if tokens {
			Self::Run {source, execution: ExecutionType::ShowTokens}
		} else {
			Self::Run {source, execution: ExecutionType::Run}
		})
	}
}

impl TryFrom<Source> for String {
	type Error = IOError;

	fn try_from(value: Source) -> Result<Self, IOError> {
		Ok(match value {
			Source::Code(code) => code,
			Source::File(file) => {
				let mut file = File::open(&file)?;
				let mut code = String::new();
				file.read_to_string(&mut code)?;
				code
			}
		})
	}
}
