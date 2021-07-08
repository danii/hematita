
#[derive(Debug)]
enum OpCode<'s> {
	/// Calls the a value with the identifier [0], with [1] arguments, and store
	/// the result into [2].
	Call {
		function: &'s str,
		arguments: &'s str,
		destination: &'s str,
		destination_local: bool
	},

	/// Value to index, index with, store.
	IndexRead {
		indexee: &'s str,
		index: &'s str,
		destination: &'s str,
		destination_local: bool
	},

	IndexWrite {
		indexee: &'s str,
		index: &'s str,
		value: &'s str
	},

	Load {
		constant: u16,
		destination: &'s str,
		destination_local: bool
	},

	Create {
		destination: &'s str,
		destination_local: bool
	},

	BinaryOperation {
		first_operand: &'s str,
		second_operand: &'s str,
		destination: &'s str,
		destination_local: bool,
		operation: BinaryOperation
	},

	UnaryOperation {
		operand: &'s str,
		destination: &'s str,
		destination_local: bool,
		operation: UnaryOperation
	},

	Jump {
		operation: u64,
		r#if: Option<&'s str>
	},

	Return {
		result: &'s str
	}
}

#[derive(Debug)]
enum BinaryOperation {
	LessThan,
	LessThanOrEqual
}

#[derive(Debug)]
enum UnaryOperation {
	Not
}

#[derive(Debug)]
struct Function {
	constants: Vec<Value>,
	opcodes: Vec<OpCode<'static>>
}

#[derive(Debug)]
struct Table(Mutex<HashMap<Value, Value>>);

use std::{collections::HashMap, sync::{Arc, Mutex}};

#[derive(Debug, Clone)]
#[warn(clippy::large_enum_variant)]
enum Value {
	Integer(i64),
	//Float(f64),
	String(Box<str>),
	Boolean(bool),
	Function(Arc<Function>),
	Table(Arc<Table>)
}

impl Eq for Value {}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Integer(a), Self::Integer(b)) => *a == *b,
			(Self::String(a), Self::String(b)) => *a == *b,
			(Self::Boolean(a), Self::Boolean(b)) => *a == *b,
			(Self::Function(a), Self::Function(b)) => Arc::as_ptr(a) == Arc::as_ptr(b),
			(Self::Table(a), Self::Table(b)) => Arc::as_ptr(a) == Arc::as_ptr(b),
			_ => false
		}
	}
}

impl std::hash::Hash for Value {
	fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
		match self {
			Self::Integer(integer) => integer.hash(state),
			Self::String(string) => string.hash(state),
			Self::Boolean(boolean) => boolean.hash(state),
			Self::Function(arc) => Arc::as_ptr(arc).hash(state),
			Self::Table(arc) => Arc::as_ptr(arc).hash(state)
		}
	}
}

//struct Stack {
	//stack: HashMap<
//}

fn execute(function: Arc<Function>, mut local: HashMap<Value, Value>) -> Value {
	let mut index = 0;
	loop {
		println!("{:?}\n{:?}\n", function.opcodes[index], local);

		match function.opcodes[index] {
			// OpCode::Call
			// OpCode::IndexRead
			OpCode::IndexWrite {indexee, index, value} => {
				let indexee = Value::String(indexee.to_string().into_boxed_str());
				let index = Value::String(index.to_string().into_boxed_str());
				let value = Value::String(value.to_string().into_boxed_str());

				let indexee = local.get(&indexee);
				let index = local.get(&index);
				let value = local.get(&value);

				match indexee {
					Some(Value::Table(table)) => {
						table.0.lock().unwrap().insert(index.unwrap().clone(), value.unwrap().clone());
					},
					_ => todo!()
				}
			},
			OpCode::Load {constant, destination, ..} => {
				local.insert(Value::String(destination.to_string().into_boxed_str()), function.constants.get(constant as usize).unwrap().clone());
			},
			OpCode::Create {destination, ..} => {
				local.insert(Value::String(destination.to_string().into_boxed_str()), Value::Table(Arc::new(Table(Mutex::new(HashMap::new())))));
			},
			OpCode::BinaryOperation {first_operand, second_operand, destination, operation: BinaryOperation::LessThanOrEqual, ..} => {
				let first = local.get(&Value::String(first_operand.to_string().into_boxed_str()));
				let second = local.get(&Value::String(second_operand.to_string().into_boxed_str()));

				let result = match (first, second) {
					(Some(Value::Integer(first)), Some(Value::Integer(second))) => first <= second,
					_ => todo!()
				};

				local.insert(Value::String(destination.to_string().into_boxed_str()), Value::Boolean(result));
			},
			// OpCode::BinaryOperation
			// OpCode::UnaryOperation
			OpCode::Jump {operation, r#if: None} => {
				index = operation as usize;
				continue;
			},
			OpCode::Jump {operation, r#if: Some(check)} => {
				let check = local.get(&Value::String(check.to_string().into_boxed_str()));
				match check {
					Some(Value::Boolean(true)) => {
						index = operation as usize;
						continue;
					},
					Some(Value::Boolean(false)) => (),
					_ => todo!()
				}
			},
			OpCode::Return {result} => break local.remove(&Value::String(result.to_string().into_boxed_str())).unwrap(),
			_ => todo!()
		}

		index = index + 1;
	}
}

fn main() {
	let function = Arc::new(Function {
		constants: vec![Value::Integer(0)],
		opcodes: vec![
			OpCode::BinaryOperation { // (t0 = a <= b
				first_operand: "a",
				second_operand: "b",
				destination: "(t0",
				destination_local: true,
				operation: BinaryOperation::LessThanOrEqual
			},
			OpCode::Create { // (r = {}
				destination: "(r",
				destination_local: true
			},
			OpCode::Load { // (t1 = 0
				constant: 0, // Number 0
				destination: "(t1",
				destination_local: true
			},
			OpCode::Jump { // if (t0 then goto 7 end
				operation: 6,
				r#if: Some("(t0")
			},
			OpCode::IndexWrite {
				indexee: "(r",
				index: "(t1",
				value: "a"
			},
			OpCode::Return {
				result: "(r"
			},
			OpCode::IndexWrite {
				indexee: "(r",
				index: "(t1",
				value: "b"
			},
			OpCode::Return {
				result: "(r"
			}
		]
	});

	let args = maplit::hashmap! {
		Value::String("a".to_string().into_boxed_str()) => Value::Integer(5),
		Value::String("b".to_string().into_boxed_str()) => Value::Integer(1)
	};
	println!("{:?}", execute(function, args));
}
