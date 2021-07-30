mod arguments;
mod repl;

use self::{
	arguments::{HELP, Arguments, ExecutionType, Error as ArgumentError},
	repl::repl
};
use std::{convert::TryFrom, io::Error as IOError, process::exit};
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
		Ok(Arguments::ShowHelp) => eprintln!("{}", HELP),
		Ok(Arguments::ShowVersion) => eprintln!("{}", VERSION),
		Ok(Arguments::Run {source, execution: ExecutionType::Run}) =>
			run_code(&handle_io(String::try_from(source)), false),
		Ok(Arguments::Run {source, execution: ExecutionType::RunInteractively}) =>
			run_code(&handle_io(String::try_from(source)), true),
		Ok(Arguments::Run {source, execution: ExecutionType::ShowByteCode}) =>
			show_byte_code(&handle_io(String::try_from(source))),
		Ok(Arguments::Run {source, execution: ExecutionType::ShowSyntaxTree}) =>
			show_syntax_tree(&handle_io(String::try_from(source))),
		Ok(Arguments::Run {source, execution: ExecutionType::ShowTokens}) =>
			show_tokens(&handle_io(String::try_from(source))),
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
	match compile(code) {
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

fn show_byte_code(code: &str) {
	match compile(code) {
		Ok(function) => {
			println!("{}", function.chunk)
		},
		Err(error) => {
			eprintln!("syntax error: {}", error);
			exit(2);
		}
	}
}

fn show_syntax_tree(code: &str) {
	let tokens = Lexer {source: code.chars().peekable()};
	let mut tokens = TokenIterator(tokens.peekable());

	match parse_block(&mut tokens) {
		Ok(block) => {
			println!("{}", block)
		},
		Err(error) => {
			eprintln!("syntax error: {}", error);
			exit(2);
		}
	}
}

fn show_tokens(code: &str) {
	let tokens = Lexer {source: code.chars().peekable()};
	let tokens: Vec<_> = tokens.collect();

	println!("{:#?}", tokens)
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
	VirtualMachine::new(standard_globals())
}

