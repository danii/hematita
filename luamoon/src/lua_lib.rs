use self::super::vm::value::{Table, Value};
use std::{collections::HashMap, iter::once, sync::{Arc, Mutex}};
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

pub fn vector_to_table(vector: Vec<Value>) -> Table {
	let len = vector.len();
	let mut map = vector.into_iter().enumerate()
		.map(|(index, value)| (Value::Integer(index as i64 + 1), value))
		.collect::<HashMap<_, _>>();
	map.extend(once((Value::Integer(0), Value::Integer(len as i64))));
	Table {data: Mutex::new(map), ..Default::default()}
}

pub fn print(arguments: Arc<Table>) -> Arc<Table> {
	println!("{:?}", table_to_vector(&*arguments));
	Arc::new(vector_to_table(Vec::new()))
}
