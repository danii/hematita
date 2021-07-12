use luamoon::{
	ast::{lexer::Lexer, parser::{TokenIterator, parse}},
	compiler::compile,
	lua_lib::print,
	vm::{value::Value, execute}
};
use maplit::hashmap;
use std::{env::args, fs::File, io::Read};

fn main() {
	let args = args().collect::<Vec<_>>();

	let mut code = String::new();
	File::open(&args[2]).unwrap().read_to_string(&mut code).unwrap();

	match &args[1] as &str {
		"lex" => {
			let tokens = Lexer {source: code.chars().peekable()};

			println!("{:#?}", tokens.collect::<Vec<_>>());
		},
		"parse" => {
			let tokens = Lexer {source: code.chars().peekable()};
			let block = parse(&mut TokenIterator(tokens.peekable()));

			match block {
				Ok(block) => println!("{:#?}", block),
				Err(error) => println!("SYNTAX ERROR: {}", error)
			}
		},
		"compile" => {
			let tokens = Lexer {source: code.chars().peekable()};
			let block = parse(&mut TokenIterator(tokens.peekable()));
			let function = match block {
				Ok(block) => compile(&block),
				Err(error) => return println!("SYNTAX ERROR: {}", error)
			};

			println!("{:#?}", function)
		},
		"run" => {
			let tokens = Lexer {source: code.chars().peekable()};
			let block = parse(&mut TokenIterator(tokens.peekable()));
			let function = match block {
				Ok(block) => compile(&block),
				Err(error) => return println!("SYNTAX ERROR: {}", error)
			};

			let locals = hashmap! {
				Value::String("print".to_owned().into_boxed_str()) =>
					Value::NativeFunction(print),
			};

			match execute(&function, locals, &mut hashmap! {}) {
				Ok(_) => (),
				Err(error) =>println!("ERROR: {}", error)
			}
		},
		_ => println!("unknown verb")
	}
}
