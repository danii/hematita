use crate::vm::value::NillableValue;

use self::super::{
	vm::{
		value::{IntoNillableValue, NillableValue::NonNil, Table, Value, OptionExt},
		VirtualMachine
	},
	lua_tuple, lua_table
};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};

pub fn table_to_vector(table: &Table) -> Vec<NillableValue<Value>> {
	let table = table.data.lock().unwrap();
	let end = table.get(&Value::Integer(0)).unwrap().integer().unwrap();

	(1..=end)
		.map(|index| table.get(&Value::Integer(index)).nillable().cloned())
		.collect()
}

pub fn vector_to_table(vector: Vec<Option<Value>>) -> HashMap<Value, Value> {
	vector.into_iter().enumerate()
		.filter_map(|(index, value)| value.map(|value| (index, value)))
		.map(|(index, value)| (Value::Integer(index as i64 + 1), value))
		.collect::<HashMap<_, _>>()
}

pub fn print(arguments: Arc<Table>, _: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	let message = table_to_vector(&*arguments).into_iter()
		.map(|argument| format!("{}", argument.nillable()))
		.join("\t");
	println!("{}", message);
	Ok(lua_tuple![].arc())
}

pub fn pcall(arguments: Arc<Table>, vm: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	Ok(match arguments.array_remove(1) {
		NonNil(Value::Function(function)) =>
				match vm.execute(&*function, arguments) {
			Ok(result) => {result.tuple_insert(1, true.into()); result},
			Err(error) => lua_tuple![false, error.into()].arc()
		},
		NonNil(Value::NativeFunction(function)) => match function(arguments, vm) {
			Ok(result) => {result.tuple_insert(1, true.into()); result},
			Err(error) => lua_tuple![false, error.into()].arc()
		},
		value => lua_tuple![
			false,
			format!("attempt to call a {} value", value.type_name()).into()
		].arc()
	})
}

pub fn error(arguments: Arc<Table>, _: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	Err(arguments.index(&Value::Integer(1)).option()
		.map(|value| value.string().map(str::to_string)).flatten()
		.unwrap_or_else(|| "(non string errors are unsupported)".to_owned()))
}

pub fn setmetatable(arguments: Arc<Table>, _: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	let arguments = table_to_vector(&arguments);
	let meta = match arguments.get(1) {
		Some(NonNil(Value::Table(meta))) => meta.clone(),
		_ => return Err("metatable error".to_owned())
	};

	match arguments.get(0) {
		Some(NonNil(Value::Table(table))) => {
			let mut table = table.metatable.lock().unwrap();
			*table = Some(meta)
		},
		_ => return Err("metatable error".to_owned())
	}

	Ok(Table::from_hashmap(vector_to_table(vec![])).arc())
}

pub fn getmetatable(arguments: Arc<Table>, _: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	let arguments = table_to_vector(&arguments);
	Ok(Table::from_hashmap(match arguments.get(0) {
		Some(NonNil(Value::Table(table))) =>
				match table.metatable.lock().unwrap().clone() {
			Some(metatable) => {
				let data = metatable.data.lock().unwrap();
				match data.get(&Value::new_string("__metatable")) {
					Some(fake) => vector_to_table(vec![Some(fake.clone())]),
					None => vector_to_table(vec![Some(Value::Table(metatable.clone()))])
				}
			},
			None => vector_to_table(vec![])
		},
		_ => vector_to_table(vec![])
	}).arc())
}

pub fn r#type(arguments: Arc<Table>, _: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	let arguments = table_to_vector(&arguments);
	let r#type = arguments.get(0).cloned().flatten().nillable().type_name();
	Ok(lua_tuple![Value::String(r#type.to_owned().into_boxed_str())].arc())
}

pub fn standard_globals() -> Arc<Table> {
	let globals = lua_table! {
		print = Value::NativeFunction(&print),
		type = Value::NativeFunction(&r#type),
		setmetatable = Value::NativeFunction(&setmetatable),
		getmetatable = Value::NativeFunction(&getmetatable),
		pcall = Value::NativeFunction(&pcall),
		error = Value::NativeFunction(&error)
	}.arc();

	{
		let mut data = globals.data.lock().unwrap();
		data.insert(Value::new_string("_G"), Value::Table(globals.clone()));
	}

	globals
}
