use crate::vm::value::Nillable;

use self::super::{
	vm::{value::{IntoNillable, Nillable::NonNil, Table, Value}, VirtualMachine},
	lua_tuple, lua_table
};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};

pub fn table_to_vector<'n>(table: &Table<'n>)
		-> Result<Vec<Nillable<'n>>, String> {
	let table = table.data.lock().map_err(|error| format!("{}", error))?;
	let end = table.get(&Value::Integer(0)).and_then(Value::integer)
		.ok_or_else(|| "call convention failure".to_string())?;

	Ok(
		(1..=end)
			.map(|index| table.get(&Value::Integer(index)).nillable())
			.collect()
	)
}

pub fn vector_to_table(vector: Vec<Option<Value>>) -> HashMap<Value, Value> {
	vector.into_iter().enumerate()
		.filter_map(|(index, value)| value.map(|value| (index, value)))
		.map(|(index, value)| (Value::Integer(index as i64 + 1), value))
		.collect::<HashMap<_, _>>()
}

pub fn print<'n>(arguments: Arc<Table<'n>>, _: &VirtualMachine)
		-> Result<Arc<Table<'n>>, String> {
	let message = table_to_vector(&*arguments)?.into_iter()
		.map(|argument| format!("{}", argument.nillable()))
		.join("\t");
	println!("{}", message);
	Ok(lua_tuple![].arc())
}

pub fn pcall<'n>(arguments: Arc<Table<'n>>, vm: &VirtualMachine<'n>)
		-> Result<Arc<Table<'n>>, String> {
	Ok(match arguments.array_remove(1) {
		NonNil(Value::Function(function)) =>
				match vm.execute(&*function, arguments) {
			Ok(result) => {result.tuple_insert(1, true.into()); result},
			Err(error) => lua_tuple![false, error].arc()
		},
		NonNil(Value::NativeFunction(function)) => match function(arguments, vm) {
			Ok(result) => {result.tuple_insert(1, true.into()); result},
			Err(error) => lua_tuple![false, error].arc()
		},
		value => lua_tuple![
			false,
			format!("attempt to call a {} value", value.type_name())
		].arc()
	})
}

pub fn error<'n>(arguments: Arc<Table<'n>>, _: &VirtualMachine<'n>)
		-> Result<Arc<Table<'n>>, String> {
	Err(arguments.index(&Value::Integer(1)).option()
		.map(|value| value.string().map(str::to_string)).flatten()
		.unwrap_or_else(|| "(non string errors are unsupported)".to_owned()))
}

pub fn setmetatable<'n>(arguments: Arc<Table<'n>>, _: &VirtualMachine<'n>)
		-> Result<Arc<Table<'n>>, String> {
	let arguments = table_to_vector(&arguments)?;
	let meta = match arguments.get(1) {
		Some(NonNil(Value::Table(meta))) => meta.clone(),
		_ => return Err("metatable error".to_owned())
	};

	match arguments.get(0) {
		Some(NonNil(Value::Table(table))) => {
			let mut table = table.metatable.lock()
				.map_err(|error| format!("{}", error))?;
			*table = Some(meta)
		},
		_ => return Err("metatable error".to_owned())
	}

	Ok(lua_tuple![].arc())
}

pub fn getmetatable<'n>(arguments: Arc<Table<'n>>, _: &VirtualMachine<'n>)
		-> Result<Arc<Table<'n>>, String> {
	let arguments = table_to_vector(&arguments)?;
	Ok(match arguments.get(0) {
		Some(NonNil(Value::Table(table))) => match table.metatable.lock()
				.map_err(|error| format!("{}", error))?.clone() {
			Some(metatable) => {
				let data = metatable.data.lock()
					.map_err(|error| format!("{}", error))?;
				match data.get(&Value::new_string("__metatable")) {
					Some(fake) => lua_tuple![fake],
					None => lua_tuple![&metatable]
				}
			},
			None => lua_tuple![]
		},
		_ => lua_tuple![]
	}.arc())
}

pub fn r#type<'n>(arguments: Arc<Table<'n>>, _: &VirtualMachine<'n>)
		-> Result<Arc<Table<'n>>, String> {
	Ok(lua_tuple![arguments.index(&1i64.into()).type_name()].arc())
}

pub fn standard_globals<'n>() -> Arc<Table<'n>> {
	let globals = lua_table! {
		print = Value::NativeFunction(&print),
		type = Value::NativeFunction(&r#type),
		setmetatable = Value::NativeFunction(&setmetatable),
		getmetatable = Value::NativeFunction(&getmetatable),
		pcall = Value::NativeFunction(&pcall),
		error = Value::NativeFunction(&error)
	}.arc();

	{
		let mut data = globals.data.lock().expect("unreachable");
		data.insert(Value::new_string("_G"), Value::Table(globals.clone()));
	}

	globals
}
