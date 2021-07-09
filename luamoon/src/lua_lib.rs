use self::super::vm::value::{Table, Value};
use std::{collections::HashMap, iter::once, sync::{Arc, Mutex}};

pub fn table_to_vector(table: &Table) -> Vec<Option<Value>> {
	let lock = table.data.lock().unwrap();
	let len = match lock.get(&Value::Integer(0)).unwrap() {
		Value::Integer(len) => *len as usize,
		_ => panic!()
	};

	(1..=len)
		.map(|index| lock.get(&Value::Integer(index as i64)).map(Clone::clone))
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
