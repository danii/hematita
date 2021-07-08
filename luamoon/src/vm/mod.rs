pub mod bytecode;

/// The operation codes used within the lua virtual machine. All lua code is
/// compiled to blocks of opcodes. Opcodes are the primitive block of "action"
/// (lua code ran from this vm cannot perform any actions more specific than
/// what these opcodes can provide). Each opcode does a different thing, but
/// most opcodes operate with memory directly from the local scope via symbols.
///
/// Typically, the lua compiler will use temporary variable names that cannot
/// be accessed from lua directly. All of these temporary variables start with
/// a left parenthesis, because that makes these variables impossible to access
/// or alter accidentally in lua. Theoretically, you can use methods from the
/// debug module to access these temporary variables, but tampering with them
/// won't do much more than corrupt the state of the currently executing
/// function.
#[derive(Debug)]
pub enum OpCode<'s> {
	/// Calls a function with the name [function], with the arguments array
	/// [arguments], and stores the result array in [destination]. [arguments]
	/// must be a lua array (a table, typically with numbered keys starting from
	/// 1), or else an error will be thrown.
	///
	/// [destination_local] determines if the result will be stored to the local
	/// scope or global scope.
	Call {
		/// The name of the function, in scope, to be called. Must be a function
		/// or else an error will be thrown.
		function: &'s str,
		/// The name of the arguments array, in scope. Must be a table or else an
		/// error will be thrown.
		arguments: &'s str,
		/// The name of where the return values will be stored.
		destination: &'s str,
		/// Whether or not the return values will be stored in the local scope or
		/// global scope.
		destination_local: bool
	},

	/// Indexes into the object with the name [indexee], with index [index], and
	/// stores the result in [destination].
	///
	/// [destination_local] determines if the result will be stored to the local
	/// scope or global scope.
	IndexRead {
		/// The name of the object to be indexed.
		indexee: &'s str,
		/// The name of the object that serves as the index.
		index: &'s str,
		/// The name of where the result will be stored.
		destination: &'s str,
		/// Whether or not the result will be stored in the local scope or global
		/// scope.
		destination_local: bool
	},

	/// Indexes into the object with the name [indexee], with index [index], and
	/// writes [value] into [indexee].
	IndexWrite {
		/// The name of the object to be indexed.
		indexee: &'s str,
		/// The name of the object that serves as the index.
		index: &'s str,
		/// The name of the value to be stored within the indexee.
		value: &'s str
	},

	/// Loads a value from the constant pool at index [constant] to [destination].
	///
	/// [destination_local] determines if the constant will be stored to the local
	/// scope or global scope.
	Load {
		/// The constant to be loaded.
		constant: u16,
		/// The name of where the constant will be stored.
		destination: &'s str,
		/// Whether or not the constant will be stored in the local scope or global
		/// scope.
		destination_local: bool
	},

	ReAssign {
		actor: &'s str,
		destination: &'s str,
		destination_local: bool
	},

	/// Creates a new empty table at [destination].
	///
	/// [destination_local] determines if the new table will be stored to the
	/// local scope or global scope.
	Create {
		/// The name of where the new table will be stored.
		destination: &'s str,
		/// Whether or not the new table will be stored in the local scope or global
		/// scope.
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

	/// Jumps unconditionally to [operation], or conditionally if the name of a
	/// condition is specified in [r#if]. The jump operation is performed in
	/// number of opcodes, not bytes.
	Jump {
		/// The opcode to jump to.
		operation: u64,
		/// An optional condition. If specified, the value at the specified name
		/// must be true or false, otherwise an error will be thrown. If true, the
		/// jump will occur, otherwise it will not.
		r#if: Option<&'s str>
	},

	Return {
		result: &'s str
	}
}

#[derive(Debug)]
pub enum BinaryOperation {
	LessThan,
	LessThanOrEqual
}

#[derive(Debug)]
pub enum UnaryOperation {
	Not
}

#[derive(Debug)]
pub struct Function {
	constants: Vec<Value>,
	opcodes: Vec<OpCode<'static>>
}

#[derive(Debug)]
pub struct Table(pub Mutex<HashMap<Value, Value>>);

use std::{collections::HashMap, sync::{Arc, Mutex}};

#[derive(Debug, Clone)]
#[warn(clippy::large_enum_variant)]
pub enum Value {
	Integer(i64),
	//Float(f64),
	String(Box<str>),
	Boolean(bool),
	Function(Arc<Function>),
	Table(Arc<Table>),
	NativeFunction(fn(Arc<Table>) -> Arc<Table>)
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
			(Self::NativeFunction(a), Self::NativeFunction(b)) => a == b,
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
			Self::Table(arc) => Arc::as_ptr(arc).hash(state),
			Self::NativeFunction(func) => func.hash(state)
		}
	}
}

//struct Stack {
	//stack: HashMap<
//}

/// Executes a function.
pub fn execute(function: Arc<Function>, mut local: HashMap<Value, Value>,
		mut global: HashMap<Value, Value>) -> Option<Value> {
	let mut index = 0; // The current opcode we're evaluating.
	// current_opcode

	loop {
		match function.opcodes[index] {
			OpCode::Call {arguments, function, ..} => {
				let func = local.get(&Value::String(function.to_string().into_boxed_str()));
				let args = local.get(&Value::String(arguments.to_string().into_boxed_str()));

				let args = match args {
					Some(Value::Table(table)) => table,
					_ => todo!()
				};

				match func {
					Some(Value::NativeFunction(func)) => {
						func(args.clone());
					},
					Some(Value::Function(_)) => todo!(),
					_ => todo!()
				}

				// TODO: Fn stuff blah.
			},
			OpCode::IndexRead {indexee, index, destination, ..} => {
				let indexee = Value::String(indexee.to_string().into_boxed_str());
				let index = Value::String(index.to_string().into_boxed_str());
				let destination = Value::String(destination.to_string().into_boxed_str());

				let indexee = local.get(&indexee);
				let index = local.get(&index);

				match indexee {
					Some(Value::Table(table)) => {
						let table = table.0.lock().unwrap();
						let value = table.get(index.unwrap()).unwrap().clone();
						drop(table);
						local.insert(destination, value);
					},
					_ => todo!()
				}
			},
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
			OpCode::Return {result} => break Some(local.remove(&Value::String(result.to_string().into_boxed_str())).unwrap()),
			_ => todo!()
		}

		index = index + 1;
		if index == function.opcodes.len() {return None}
	}
}
