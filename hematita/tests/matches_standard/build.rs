// This file builds integration tests for each file in the lua directory. The
// test is to check if the execution of a Lua source file in Hematita produces
// the same standard output as the standard Lua implementation. Files in the
// lua folder should not produce psuedo-random or random output (e.g. by)
// printing pointers. 

use std::{path::Path, fs::{File, create_dir_all, read_dir}, io::{Error, Write}};

static HEADER: &str = "\
use hematita::{
	ast::{lexer::Lexer, parser::{TokenIterator, parse_block}},
	compiler::compile_block, lua_lib::standard_globals,
	vm::{value::{IntoNillable, Table, Value}, VirtualMachine},
	lua_tuple
};
use itertools::Itertools;
use std::{process::Command, sync::{Arc, Mutex}};
use diff::{Result as Diff, lines};

fn table_to_vector<'n>(table: &Table<'n>) -> Vec<Option<Value<'n>>> {
	let table = table.data.lock().unwrap();
	let end = table.get(&Value::Integer(0)).unwrap().integer().unwrap();

	(1..=end)
		.map(|index| table.get(&Value::Integer(index)).cloned())
		.collect()
}

/// Executes [code] using Hematita.
fn hematita(code: &str) -> String {
	let output = Arc::new(Mutex::new(String::new()));
	let print_output = output.clone();
	let print = move |arguments: Arc<_>, _: &'_ _| {
		let message = table_to_vector(&*arguments).into_iter()
			.map(|argument| format!(\"{}\", argument.nillable()))
			.join(\"\t\");
		let mut output = output.lock().unwrap();
		output.push_str(&message); output.push('\\n');
		Ok(lua_tuple![].arc())
	};

	let tokens = Lexer {source: code.chars().peekable()}.peekable();
	let parsed = parse_block(&mut TokenIterator(tokens)).unwrap();
	let function = compile_block(&parsed).into();

	let globals = {
		let globals = standard_globals();
		{
			let mut data = globals.data.lock().unwrap();
			data.insert(Value::new_string(\"print\"), Value::NativeFunction(&print));
		}
		globals
	};

	let arguments = lua_tuple![].arc();
	let virtual_machine = VirtualMachine::new(globals);
	virtual_machine.execute(&function, arguments).unwrap();

	let result = print_output.lock().unwrap().clone();
	result
}

/// Executes [code] using the standard Lua interpreter.
fn standard(code: &str) -> String {
	let stdout = Command::new(\"lua\")
		.args([\"-e\", code])
		.output()
		.unwrap()
		.stdout;
	String::from_utf8_lossy(&stdout).into_owned()
}
";

macro_rules! manufacture_test {
	($name:ident, $lua:ident) => {{
		format!(
"\
\n#[test]
fn {}() -> Result<(), ()> {{
	static CODE: &str = include_str!({:?});

	let hematita = hematita(CODE);
	let standard = standard(CODE);

	if hematita != standard {{
		eprintln!(\"The output of {{:?}} from Hematita differs from the standard Lua implementation.\", {:?});
		lines(&hematita, &standard).into_iter()
			.for_each(|result| match result {{
				Diff::Left(left) => eprintln!(\"\\x1B[31m-{{}}\\x1B[0m\", left),
				Diff::Right(right) => eprintln!(\"\\x1B[32m+{{}}\\x1B[0m\", right),
				Diff::Both(both, _) => eprintln!(\" {{}}\", both)
			}});
		Err(())
	}} else {{Ok(())}}
}}
",
			$name, $lua, $lua.file_name().unwrap()
		)
	}}
}

fn snake_case(camel_case: &str) -> String {
	let mut snake_case = String::with_capacity(
		(camel_case.len() as f32 * 1.25) as usize);

	camel_case.chars().for_each(|char| {
		if char.is_uppercase() {snake_case.push('_')};
		char.to_lowercase().for_each(|char| snake_case.push(char))
	});

	snake_case
}

pub fn build(working: &Path, output: &Path) {
	println!("cargo:rerun-if-changed={}", working.join("lua").to_string_lossy());

	let file = read_dir(working.join("lua")).unwrap()
		.map(|file| {
			let file = file?;

			let file_type = file.file_type()?;
			let path = file.path();

			let function = {
				let mut function = file.file_name().into_string().unwrap();
				function.truncate(function.len() - 4);
				snake_case(&function)
			};

			if file_type.is_file() {
				Ok(manufacture_test!(function, path))
			} else {todo!()}
		})
		.collect::<Result<String, Error>>()
		.unwrap();

	let file = format!("{}{}", HEADER, file);
	create_dir_all(output.join("codegen")).unwrap();
	File::create(output.join("codegen/hematita-tests-matches_standard-main")).unwrap()
		.write_all(file.as_bytes()).unwrap();
}
