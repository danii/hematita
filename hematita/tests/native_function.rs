use hematita::{
	ast::{lexer::Lexer, parser::{TokenIterator, parse_block}},
	compiler::compile_block, lua_lib::standard_globals,
	vm::{value::Value, VirtualMachine},
	lua_tuple, lua_value
};
use std::sync::Mutex;

fn main() {
	let source = "counter() counter() counter()";
	let lexer = Lexer {source: source.chars().peekable()}.peekable();
	let parsed = parse_block(&mut TokenIterator(lexer)).unwrap();
	let compiled = compile_block(&parsed);

	let number = Mutex::new(0);
	let counter = move |_, _: &_| {
		let mut lock = number.lock().unwrap();
		let old = *lock;
		*lock += 1;
		Ok(lua_tuple![old].arc())
	};

	let globals = {
		let globals = standard_globals();

		let mut data = globals.data.lock().unwrap();
		data.insert(lua_value!("counter"), Value::NativeFunction(&counter));
		drop(data);

		globals
	};

	let virtual_machine = VirtualMachine::new(globals);
	virtual_machine.execute(&compiled.into(), lua_tuple![].arc()).unwrap();
}