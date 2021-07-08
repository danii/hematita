use self::super::vm::{Table, Value};
use itertools::Itertools;
use std::sync::{Arc, Mutex};

pub fn table_to_vector(table: &Table) -> Vec<Value> {
	let lock = table.0.lock().unwrap();
	lock.iter()
		.filter_map(|(key, value)| if let Value::Integer(integer) = key
			{Some((integer, value))} else {None})
		.sorted_unstable_by(|(index_a, _), (index_b, _)| index_a.cmp(index_b))
		.enumerate()
		.filter_map(|(index, (key, value))| if (*key as usize) - 1 == index
			{Some(value.clone())} else {None})
		.collect()
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
	Table(Mutex::new(vector.into_iter().enumerate()
		.map(|(index, value)| (Value::Integer(index as i64 + 1), value))
		.collect()))
}

pub fn print(arguments: Arc<Table>) -> Arc<Table> {
	println!("{:?}", table_to_vector(&*arguments));
	Arc::new(vector_to_table(Vec::new()))
}
