pub mod bytecode;
pub mod value;

use self::value::{Function, IntoNillableValue, Nil, NonNil, Table, Value};
use if_chain::if_chain;
use maplit::hashmap;
use std::{collections::HashMap, sync::Arc};

/// Executes a function.
pub fn execute(function: Arc<Function>, mut local: HashMap<Value, Value>,
		mut _global: HashMap<Value, Value>) -> Result<Option<Value>, String> {
	let mut index = 0; // The current opcode we're evaluating.
	// current_opcode

	loop {
		match function.opcodes[index] {
			OpCode::Call {arguments, function, ..} => {
				let args = match local.get(&Value::identifier(arguments)).nillable() {
					NonNil(Value::Table(table)) => table,
					// args is not a table.
					args => break Err(format!(
						"attempt to initiate a function call with a {} value",
							args.type_name()))
				};

				match local.get(&Value::identifier(function)).nillable() {
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
					match local.get(&Value::identifier(indexee)).nillable() {
				// indexee is a table.
				NonNil(Value::Table(table)) => {
					let index = local.get(&Value::identifier(index))
						.ok_or("table index is nil".to_owned())?; // table index is nil.
					let table = table.data.lock().unwrap();
					let value = table.get(index).map(Clone::clone);
					drop(table); // Borrow checker stuff.

					let destination = Value::identifier(destination);
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
					match local.get(&Value::identifier(indexee)).nillable() {
				// indexee is a table.
				NonNil(Value::Table(table)) => {
					let mut lock = table.data.lock().unwrap();
					let index = local.get(&Value::identifier(index))
						.ok_or("table index is nil".to_owned())?; // index is nil.

						// value is not nil.
						if let Some(value) = local.get(&Value::identifier(value)) {
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
					{local.insert(Value::identifier(destination), constant);},
				// constant is.... nil?
				Nil => {local.remove(&Value::identifier(destination));}
			},

			OpCode::Create {destination, ..} =>
				{local.insert(Value::identifier(destination),
					Value::Table(Arc::default()));},

			OpCode::BinaryOperation {first, second, destination, operation, ..} => {
				let first = local.get(&Value::identifier(first)).nillable();
				let second = local.get(&Value::identifier(second)).nillable();

				todo!();
				match operation {
					BinaryOperation::LessThanOrEqual => match (first, second) {
						(NonNil(Value::Integer(first)), NonNil(Value::Integer(second))) => {
							Value::Boolean(first >= second);
						},
						(NonNil(Value::String(first)), NonNil(Value::String(second))) => {
							Value::Boolean(first >= second);
						},
						(NonNil(Value::Table(metamethod)), second) if {
								if_chain::if_chain! {
									if let Some(metamethod) = metamethod.metatable;
									if let Ok(metamethod) = metamethod.data.lock();
									if let NonNil(metamethod) =
										metamethod.get(&Value::identifier("__le")).nillable();
									then {
										match metamethod {
											Value::Function(metamethod) => true,
											Value::NativeFunction(metamethod) => {
												metamethod(Table::array([first, second]).arc());

												true
											},
											_ => false
										}
									} else {false}
								}
							} => (),
						_ => todo!()
					},
					_ => todo!()
				}
			},
			/*{
				let first = local.get(&Value::String(first.to_string().into_boxed_str()));
				let second = local.get(&Value::String(second.to_string().into_boxed_str()));

				let result = match (first, second) {
					(Some(Value::Integer(first)), Some(Value::Integer(second))) => first <= second,
					_ => todo!()
				};

				local.insert(Value::String(destination.to_string().into_boxed_str()), Value::Boolean(result));
			},*/
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
			OpCode::Return {result} => break Ok(Some(local.remove(&Value::String(result.to_string().into_boxed_str())).unwrap())),
			_ => todo!()
		}

		index = index + 1;
		if index == function.opcodes.len() {return Ok(None)}
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
		destination: &'s str,
		local: bool,
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

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperation {
	LessThan,
	LessThanOrEqual
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperation {
	Not
}
