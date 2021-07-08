use luamoon::{ast::{lexer::Lexer, parser::parse}, lua_lib::print, vm::{Table, Value, bytecode::generate_bytecode, execute}};
use std::{fs::File, io::Read, sync::{Arc, Mutex}};

fn main() {
	let mut code = String::new();
	File::open("test.lua").unwrap().read_to_string(&mut code).unwrap();

	let tokens = Lexer {source: code.chars().peekable()};
	let block = parse(&mut tokens.peekable());

	let function = generate_bytecode(block);
	let local = maplit::hashmap! {
		Value::String("print".to_owned().into_boxed_str()) =>
			Value::NativeFunction(print),
		Value::String("a".to_owned().into_boxed_str()) =>
			Value::Table(Arc::new(Table(Mutex::new(maplit::hashmap! {
				Value::String("b".to_owned().into_boxed_str()) =>
					Value::Integer(10)
			}))))
	};

	println!("{:#?}", function);

	execute(Arc::new(function), local, maplit::hashmap! {}).unwrap();

	//println!("{:?}", lexer.collect::<Vec<_>>());
}
