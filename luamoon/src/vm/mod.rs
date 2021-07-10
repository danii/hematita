pub mod bytecode;
pub mod value;

use self::value::{Function, IntoNillableValue, Nil, NonNil, Table, Value};
use if_chain::if_chain;
use std::{collections::HashMap, sync::Arc};

/// Executes a function.
pub fn execute(function: &Function, mut local: HashMap<Value, Value>,
		global: &mut HashMap<Value, Value>) -> Result<Arc<Table>, String> {
	let mut index = 0; // The current opcode we're evaluating.
	// current_opcode

	loop {
		match function.opcodes[index] {
			OpCode::Call {arguments, function, ..} => {
				let args = match local.get(&Value::new_string(arguments)).nillable() {
					NonNil(Value::Table(table)) => table,
					// args is not a table.
					args => break Err(format!(
						"attempt to initiate a function call with a {} value",
							args.type_name()))
				};

				match local.get(&Value::new_string(function)).nillable() {
					NonNil(Value::NativeFunction(func)) => {
						// TODO: Return...
						func(args.clone());
					},
					// TODO: Fn stuff blah.
					NonNil(Value::Function(_)) => todo!(),
					// func is not a function.
					func => break Err(format!("attempt to call a {} value", func.type_name()))
				}
			},

			OpCode::IndexRead {indexee, index, destination, ..} =>
					match local.get(&Value::new_string(indexee)).nillable() {
				// indexee is a table.
				NonNil(Value::Table(table)) => {
					let index = local.get(&Value::new_string(index))
						.ok_or("table index is nil".to_owned())?; // table index is nil.
					let table = table.data.lock().unwrap();
					let value = table.get(index).map(Clone::clone);
					drop(table); // Borrow checker stuff.

					let destination = Value::new_string(destination);
					// value is not nil.
					if let Some(value) = value {
						local.insert(destination, value);
					// value is nil.
					} else {
						local.remove(&destination);
					}
				},
				// indexee is not a table.
				indexee => break Err(format!(
					"attempt to index a {} value", indexee.type_name()))
			},

			OpCode::IndexWrite {indexee, index, value} =>
					match local.get(&Value::new_string(indexee)).nillable() {
				// indexee is a table.
				NonNil(Value::Table(table)) => {
					let mut lock = table.data.lock().unwrap();
					let index = local.get(&Value::new_string(index))
						.ok_or("table index is nil".to_owned())?; // index is nil.

						// value is not nil.
						if let Some(value) = local.get(&Value::new_string(value)) {
							lock.insert(index.clone(), value.clone());
						// value is nil.
					} else {
						lock.remove(index);
					}
				},
				// indexee is not a table.
				indexee => break Err(format!(
					"attempt to index a {} value", indexee.type_name()))
			},

			OpCode::Load {constant, destination, ..} =>
					match function.constants.get(constant as usize).nillable().cloned() {
				// constant is not nil.
				NonNil(constant) =>
					{local.insert(Value::new_string(destination), constant);},
				// constant is.... nil?
				Nil => {local.remove(&Value::new_string(destination));}
			},

			OpCode::Create {destination, ..} =>
				{local.insert(Value::new_string(destination),
					Value::Table(Arc::default()));},

			OpCode::BinaryOperation {first, second, destination, operation, ..} => {
				let first = local.get(&Value::new_string(first)).nillable();
				let second = local.get(&Value::new_string(second)).nillable();
				let destination = Value::new_string(destination);

				// Uneeded when 53667 and 51114.
				// In order to have the match statement continue after we actually get
				// the function object, we currently have to use a block within the if
				// statements condition, primarily because 51114 isn't implemented yet,
				// which would allow us to check the contents of the Mutex during match
				// and save a variable with it's contents. 53667 needs to be implemented
				// to, because we also need to pattern match through an Arc.
				let mut transfer_result = None;
				let result = match operation {
					// BinaryOperation::Equal

					// BinaryOPeration::NotEqual

					// BinaryOperation::LessThan

					BinaryOperation::LessThanOrEqual => match (&first, &second) {
						(NonNil(Value::Integer(first)), NonNil(Value::Integer(second))) =>
							Value::Boolean(first <= second),
						(NonNil(Value::String(first)), NonNil(Value::String(second))) =>
							Value::Boolean(first <= second),
						/*
							// Code for when features are added...
							if let Some(metamethod) = &metamethod.metatable &&
								if let NonNil(metamethod) = metamethod.data.lock().unwrap()
									.get(&Value::identifier("__le")).nillable() => {
						*/
						(NonNil(Value::Table(metamethod)), _) if {
							if_chain! {
								if let Some(metamethod) = &metamethod.metatable;
								if let NonNil(metamethod) = metamethod.data.lock().unwrap()
									.get(&Value::new_string("__le")).nillable();
								then {
									match metamethod {
										Value::Function(metamethod) => {
											let mut arguments = HashMap::new();
											if let NonNil(first) = first.cloned()
												{arguments.insert(Value::Integer(0), first);}
											if let NonNil(second) = second.cloned()
												{arguments.insert(Value::Integer(0), second);}
											transfer_result = Some(execute(
												&*metamethod, arguments, global)?);
											true
										},
										Value::NativeFunction(metamethod) => {
											transfer_result = Some(metamethod(
												Table::array([&first, &second]).arc()));
											true
										},
										_ => false
									}
								} else {false}
							}
						} => {
							// Panic is impossible because we put in something in the if.
							transfer_result.unwrap().data.lock().unwrap()
								.get(&Value::Integer(0)).nillable().coerce_to_boolean()
						},
						/*
							// Code for when features are added...
							if let Some(metamethod) = &metamethod.metatable &&
								if let NonNil(metamethod) = metamethod.data.lock().unwrap()
									.get(&Value::identifier("__le")).nillable() => {
						*/
						(_, NonNil(Value::Table(metamethod))) if {
							if_chain! {
								if let Some(metamethod) = &metamethod.metatable;
								if let NonNil(metamethod) = metamethod.data.lock().unwrap()
									.get(&Value::new_string("__le")).nillable();
								then {
									match metamethod {
										Value::Function(metamethod) => {
											let mut arguments = HashMap::new();
											if let NonNil(first) = first.cloned()
												{arguments.insert(Value::Integer(0), first);}
											if let NonNil(second) = second.cloned()
												{arguments.insert(Value::Integer(0), second);}
											transfer_result = Some(execute(
												&*metamethod, arguments, global)?);
											true
										},
										Value::NativeFunction(metamethod) => {
											transfer_result = Some(metamethod(
												Table::array([&first, &second]).arc()));
											true
										},
										_ => false
									}
								} else {false}
							}
						} => {
							// Panic is impossible because we put in something in the if.
							transfer_result.unwrap().data.lock().unwrap()
								.get(&Value::Integer(0)).nillable().coerce_to_boolean()
						},
						_ => todo!()
					},
					
					// BinaryOperation::GreaterThan

					// BinaryOperation::LessThan

					// BinaryOperation::Add

					// BinaryOperation::Subtract

					_ => todo!()
				};

				local.insert(destination, result);
			},

			OpCode::UnaryOperation {operand, operation, destination, ..} => {
				let operand = local.get(&Value::new_string(operand)).nillable();
				let destination = Value::new_string(destination);

				let result = match operation {
					UnaryOperation::Not => Value::Boolean(!operand.coerce_to_bool())
				};

				local.insert(destination, result);
			},

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

			OpCode::Return {result} => match local.get(
					&Value::new_string(result)).nillable() {
				NonNil(Value::Table(result)) => break Ok(result.clone()),
				_ => panic!()
			},

			_ => todo!()
		}

		index = index + 1;
		if index == function.opcodes.len() {break Ok(Table::default().arc())}
	}
}

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
		first: &'s str,
		second: &'s str,
		operation: BinaryOperation,
		destination: &'s str,
		local: bool
	},

	UnaryOperation {
		operand: &'s str,
		operation: UnaryOperation,
		destination: &'s str,
		local: bool
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

// TODO: Should we remove [crate::ast::parser::BinaryOperator] and use this
// instead? Same goes for UnaryOperation and Operator.
#[derive(Clone, Copy, Debug)]
pub enum BinaryOperation {
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Add,
	Subtract
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperation {
	Not
}
