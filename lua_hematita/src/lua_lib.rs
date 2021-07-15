use self::super::vm::value::{IntoNillableValue, Table, Value};
use std::{collections::HashMap, sync::Arc};
use itertools::Itertools;

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

pub fn print(arguments: Arc<Table>, _: Arc<Table>)
		-> Result<Arc<Table>, String> {
	let message = table_to_vector(&*arguments).into_iter()
		.map(|argument| format!("{}", argument.nillable()))
		.join("\t");
	println!("{}", message);
	Ok(Table::from_hashmap(vector_to_table(Vec::new())).arc())
}

/*
pub fn pcall(arguments: Arc<Table>, global: Arc<Table>)
		-> Result<Arc<Table>, String> {
	let mut arguments = table_to_vector(&*arguments);
	match arguments.remove(0).nillable() {
		NonNil(Value::Function(function)) => {
			let arguments = vector_to_table(arguments);
			let result = match execute(&*function, arguments, global.clone()) {
				Ok(result) => vec![
					Some(Value::Boolean(true)),
					Some(Value::Table(result))
				],
				Err(err) => vec![
					Some(Value::Boolean(false)),
					Some(Value::String(err.into_boxed_str()))
				]
			};

			Ok(Table::from_hashmap(vector_to_table(result)).arc())
		},
		value => {
			let result = format!("attempt to call a {} value", value.type_name());
			let result = vec![
				Some(Value::Boolean(false)),
				Some(Value::String(result.into_boxed_str()))
			];

			Ok(Table::from_hashmap(vector_to_table(result)).arc())
		}
	}
}*/

pub fn getmetatable(arguments: Arc<Table>) -> Result<Arc<Table>, String> {
	let arguments = table_to_vector(&arguments);
	Ok(Table::from_hashmap(match arguments.get(0) {
		Some(Some(Value::Table(table))) => match table.metatable.clone() {
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
