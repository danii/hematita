use lua_hematita::{
	ast::{lexer::Lexer, parser::{Error, TokenIterator, parse}},
	compiler::compile,
	lua_lib::{print, r#type, getmetatable, setmetatable},
	vm::{VirtualMachine, value::{Table, Value}}
};
use maplit::hashmap;
use std::{env::args, fs::File, io::{BufRead, Read, Write}};

fn main() {
	let args = args().collect::<Vec<_>>();

	if args.len() != 3 {
		repl();
		return
	}


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

			println!("{}", function)
		},
		"run" => {
			let tokens = Lexer {source: code.chars().peekable()};
			let block = parse(&mut TokenIterator(tokens.peekable()));
			let function = match block {
				Ok(block) => compile(&block),
				Err(error) => return println!("SYNTAX ERROR: {}", error)
			};

			let globals = Table::from_hashmap(hashmap! {
				Value::new_string("print") =>
					Value::NativeFunction(print),
				Value::new_string("type") =>
					Value::NativeFunction(r#type),
				Value::new_string("getmetatable") =>
					Value::NativeFunction(getmetatable),
				Value::new_string("setmetatable") =>
					Value::NativeFunction(setmetatable)/*,
				Value::new_string("pcall") =>
					Value::NativeFunction(pcall)*/
			}).arc();

			{
				let mut data = globals.data.lock().unwrap();
				data.insert(Value::new_string("_G"), Value::Table(globals.clone()));
			}

			let arguments = Table::default().arc();

			match VirtualMachine::new(globals).execute(&function.into(), arguments) {
				Ok(_) => (),
				Err(error) => println!("ERROR: {}", error)
			}
		},
		_ => println!("unknown verb")
	}
}

fn repl() {
	let globals = Table::from_hashmap(hashmap! {
		Value::new_string("print") =>
			Value::NativeFunction(print),
		Value::new_string("type") =>
			Value::NativeFunction(r#type),
		Value::new_string("getmetatable") =>
			Value::NativeFunction(getmetatable),
		Value::new_string("setmetatable") =>
			Value::NativeFunction(setmetatable)/*,
		Value::new_string("pcall") =>
			Value::NativeFunction(pcall)*/
	}).arc();

	{
		let mut data = globals.data.lock().unwrap();
		data.insert(Value::new_string("_G"), Value::Table(globals.clone()));
	}

	let mut virtual_machine = VirtualMachine::new(globals);
	let stdin = std::io::stdin();
	
	'a: loop {
		let mut stdin = stdin.lock();
		let mut code = String::new();

		let block = loop {
			let mut line = String::new();
			print!("> ");
			std::io::stdout().flush().unwrap();
			stdin.read_line(&mut line).unwrap();

			match &line as &str {
				".exit\n" => return,
				_ => ()
			}

			code.push_str(&line);

			let mut tokens = TokenIterator(Lexer {
				source: code.chars().peekable()
			}.peekable());
			let block = parse(&mut tokens);

			if let Some(token) = tokens.0.peek() {
				eprintln!("unexpected {}", token);
				continue 'a
			}
			match block {
				Ok(block) => break block,
				Err(error) => {
					eprintln!("{}", error);
					continue 'a
				}
			}
		};

		let chunk = compile(&block);
		match virtual_machine.execute(&chunk.into(), Table::default().arc()) {
			Err(error) => eprintln!("{}", error),
			Ok(_) => ()
		}
	}
}
