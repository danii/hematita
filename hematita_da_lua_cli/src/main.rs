mod arguments;
mod repl;

use self::{arguments::{HELP, Arguments, Error as ArgumentError}, repl::repl};
use std::{fs::File, io::{Error as IOError, Read}, process::exit};
use hematita_da_lua::{
	ast::{
		lexer::Lexer,
		parser::{
			Block,
			Error as ParserError,
			Statement,
			TokenIterator,
			parse_block, parse_expression
		}
	},
	compiler::compile_block,
	lua_lib::standard_globals,
	vm::{VirtualMachine, value::{Function, Table}}
};

pub static VERSION: &str = "\
Hematita Da Lua 0.1.0 Beta
Targeting Lua 5.4.3";

fn main() {
	match Arguments::from_env() {
		Ok(Arguments::Help) => eprintln!("{}", HELP),
		Ok(Arguments::Version) => eprintln!("{}", VERSION),
		Ok(Arguments::Inline(code, interactive)) => run_code(&code, interactive),
		Ok(Arguments::File(file, interactive)) => {
			let mut file = handle_io(File::open(&file));
			let mut code = String::new();
			handle_io(file.read_to_string(&mut code));
			run_code(&code, interactive)
		},
		Err(ArgumentError::MissingArgument) => repl(init_vm()),
		a => todo!("{:?}", a)
	}
}

fn handle_io<T>(result: Result<T, IOError>) -> T {
	match result {
		Ok(value) => value,
		Err(error) => {
			eprintln!("io error: {}", error);
			exit(3);
		}
	}
}

fn run_code(code: &str, interactive: bool) {
	match compile(&code) {
		Ok(function) => {
			let vm = init_vm();
			match vm.execute(&function, Table::default().arc()) {
				Ok(_) => if interactive {repl(vm)},
				Err(error) => {
					eprintln!("runtime error: {}", error);
					exit(1);
				}
			}
		},
		Err(error) => {
			eprintln!("syntax error: {}", error);
			exit(2);
		}
	}
}

fn compile(code: &str) -> Result<Function, ParserError> {
	let tokens = Lexer {source: code.chars().peekable()};
	let mut tokens = TokenIterator(tokens.peekable());
	let block = parse_block(&mut tokens)?;
	if let Some(token) = tokens.next() {return Err(ParserError(Some(token)))};
	let chunk = compile_block(&block);
	Ok(chunk.into())
}

fn compile_expression(code: &str) -> Result<Function, ParserError> {
	let tokens = Lexer {source: code.chars().peekable()};
	let mut tokens = TokenIterator(tokens.peekable());
	let expression = parse_expression(&mut tokens)?;
	if let Some(token) = tokens.next() {return Err(ParserError(Some(token)))};
	let statement = Statement::Return {values: vec![expression]};
	let block = Block(vec![statement]);
	let chunk = compile_block(&block);
	Ok(chunk.into())
}

fn init_vm() -> VirtualMachine {
	VirtualMachine::new(standard_globals().arc())
}

