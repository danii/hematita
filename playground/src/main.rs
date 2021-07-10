use luamoon::{ast::{lexer::Lexer, parser::{TokenIterator, parse}}, lua_lib::print, vm::{value::{Table, Value}, bytecode::generate_bytecode, execute}};
use std::{fs::File, io::Read, sync::{Arc, Mutex}};

fn main() {
	let mut code = String::new();
	File::open("test.lua").unwrap().read_to_string(&mut code).unwrap();

	let tokens = Lexer {source: code.chars().peekable()};
	let block = parse(&mut TokenIterator(tokens.peekable()));

	let block = match block {
		Ok(block) => block,
		Err(error) => panic!("{:?}", error)
	};

	let function = generate_bytecode(block);
	let local = maplit::hashmap! {
		Value::String("print".to_owned().into_boxed_str()) =>
			Value::NativeFunction(print),
		Value::String("a".to_owned().into_boxed_str()) =>
			Value::Table(Arc::new(Table {data: Mutex::new(maplit::hashmap! {
				Value::String("b".to_owned().into_boxed_str()) =>
					Value::Integer(10)
			}), metatable: None}))
	};

	execute(&Arc::new(function), local, &mut maplit::hashmap! {}).unwrap();

	//println!("{:?}", lexer.collect::<Vec<_>>());
}
