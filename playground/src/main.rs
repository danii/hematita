use luamoon::{
	ast::{lexer::Lexer, parser::parse},
	lua_lib::print,
	vm::{bytecode::generate_bytecode, Value, execute}
};
use std::{fs::File, io::Read, sync::Arc};

fn main() {
	let mut code = String::new();
	File::open("test.lua").unwrap().read_to_string(&mut code).unwrap();

	let tokens = Lexer {source: code.chars().peekable()};
	let block = parse(&mut tokens.peekable());

	let function = generate_bytecode(block);
	let local = maplit::hashmap! {
		Value::String("print".to_owned().into_boxed_str()) =>
			Value::NativeFunction(print)
	};

	execute(Arc::new(function), local);

	//println!("{:?}", lexer.collect::<Vec<_>>());
}
