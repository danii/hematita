use self::super::{
	vm::{
		value::{IntoNillableValue, NillableValue::NonNil, Table, Value},
		VirtualMachine
	},
	lua_table
};
use itertools::Itertools;
use std::{collections::HashMap, sync::Arc};

pub fn table_to_vector(table: &Table) -> Vec<Option<Value>> {
	let table = table.data.lock().unwrap();
	let array = table.iter()
		.filter_map(|(key, value)| if let Value::Integer(integer) = key
			{Some((integer, value))} else {None})
		.sorted_unstable_by(|(a, _), (b, _)| a.cmp(b));

	let vec = array.clone().collect::<Vec<_>>();
	array.last()
		.map(|(highest, _)| (1..=*highest)
			.map(|index| vec.iter().find(|value| *value.0 == index)
				.map(|value| value.1.clone()))
			.collect::<Vec<_>>())
		.unwrap_or_else(Vec::new)
}

/*
pub fn table_to_vector(table: Table) -> Vec<Value> {
	table.0.into_inner().unwrap().into_iter()
		.filter_map(|(key, value)| if let Value::Integer(integer) = key
			{Some((integer, value))} else {None})
		.sorted_unstable_by(|(index_a, _), (index_b, _)| index_a.cmp(index_b))
		.enumerate()
		.filter_map(|(index, (key, value))| if (key as usize) - 1 == index
			{Some(value)} else {None})
		.collect()
}
*/

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
	Ok(lua_table! {}.arc())
}

pub fn pcall(arguments: Arc<Table>, vm: &VirtualMachine)
		-> Result<Arc<Table>, String> {
	Ok(match arguments.array_remove(1) {
		NonNil(Value::Function(function)) =>
				match vm.execute(&*function, arguments) {
			Ok(result) => {result.array_insert(1, true.into()); result},
			Err(error) => Table::array([&false.into(), &error.into()]).arc()
		},
		NonNil(Value::NativeFunction(function)) => match function(arguments, vm) {
			Ok(result) => {result.array_insert(1, true.into()); result},
			Err(error) => Table::array([&false.into(), &error.into()]).arc()
		},
		value => Table::array([
			&false.into(),
			&format!("attempt to call a {} value", value.type_name()).into()
		]).arc()
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
		Some(Some(Value::Table(meta))) => meta.clone(),
		_ => return Err("metatable error".to_owned())
	};

	match arguments.get(0) {
		Some(Some(Value::Table(table))) => {
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
		Some(Some(Value::Table(table))) =>
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
	let result = vec![Some(Value::String(r#type.to_owned().into_boxed_str()))];
	Ok(Table::from_hashmap(vector_to_table(result)).arc())
}

pub fn standard_globals() -> Table {
	lua_table! {
		print = Value::NativeFunction(&print),
		type = Value::NativeFunction(&r#type),
		setmetatable = Value::NativeFunction(&setmetatable),
		getmetatable = Value::NativeFunction(&getmetatable),
		pcall = Value::NativeFunction(&pcall),
		error = Value::NativeFunction(&error)
	}
}
