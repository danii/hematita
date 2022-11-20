pub mod constant;
pub mod value;
#[cfg(test)]
pub mod tests;

use self::{
	super::{
		lua_tuple,
		ast::parser::{BinaryOperator, UnaryOperator}
	},
	constant::Constant,
	value::{
		Function, IntoNillable, MaybeUpValue, Nillable, Nil, NonNil,
		Table, Value
	}
};
use std::{
	cmp::max,
	collections::HashMap,
	convert::TryFrom,
	fmt::{Display, Formatter, Result as FMTResult},
	sync::Arc
};

macro_rules! binary_comparison {
	($self:ident, $left:ident, $right:ident, $name:ident, $rust_op:tt,
			$reverse:literal) => {{
		match ($left, $right) {
			// Primitive comparisons.
			(NonNil(Value::Integer(left)), NonNil(Value::Integer(right))) =>
				NonNil(Value::Boolean(left $rust_op right)),
			(NonNil(Value::String(left)), NonNil(Value::String(right))) =>
				NonNil(Value::Boolean(left $rust_op right)),

			(left, right)
					if $self.meta_method(&left, stringify!($name)).is_non_nil() => {
				let meta = $self.meta_method(&left, stringify!($name));
				let arguments = if $reverse {lua_tuple![right, left]}
					else {lua_tuple![left, right]};
				let result = $self.call(meta, arguments.arc())?;
				NonNil(result.index(&Value::Integer(1)).coerce_to_boolean())
			},
			(left, right)
					if $self.meta_method(&right, stringify!($name)).is_non_nil() => {
				let meta = $self.meta_method(&left, stringify!($name));
				let arguments = if $reverse {lua_tuple![right, left]}
					else {lua_tuple![left, right]};
				let result = $self.call(meta, arguments.arc())?;
				let r = result.index(&Value::Integer(1));
				NonNil(r.coerce_to_boolean())
			},

			_ => return Err("unknown binary operation error".to_owned())
		}
	}}
}

macro_rules! binary_equality {
	($self:ident, $left:ident, $right:ident, $rust_op:tt, $invert:literal) => {
		match ($left, $right) {
			(Nil, Nil) => NonNil(Value::Boolean(!$invert)),
			(left @ NonNil(Value::Table(_)), right @ NonNil(Value::Table(_)))
					if $self.meta_method(&left, "__eq").is_non_nil() => {
				let meta = $self.meta_method(&left, "__eq");
				let result = $self.call(meta, lua_tuple![left, right].arc())?;
				let result = result.index(&Value::Integer(1)).coerce_to_bool();
				NonNil(Value::Boolean(result ^ $invert))
			},
			(left @ NonNil(Value::Table(_)), right @ NonNil(Value::Table(_)))
					if $self.meta_method(&right, "__eq").is_non_nil() => {
				let meta = $self.meta_method(&right, "__eq");
				let result = $self.call(meta, lua_tuple![left, right].arc())?;
				let result = result.index(&Value::Integer(1)).coerce_to_bool();
				NonNil(Value::Boolean(result ^ $invert))
			},
			(left, right) =>
				NonNil(Value::Boolean(left $rust_op right)),
		}
	}
}

#[derive(Clone, Copy)]
enum Reference<'s> {
	Local(usize),
	Global(&'s str)
}

impl<'s> From<usize> for Reference<'s> {
	fn from(local: usize) -> Self {
		Self::Local(local)
	}
}

impl<'s> From<&'s str> for Reference<'s> {
	fn from(global: &'s str) -> Self {
		Self::Global(global)
	}
}

pub struct VirtualMachine<'n> {
	number_meta: Option<Arc<Table<'n>>>,
	string_meta: Option<Arc<Table<'n>>>,
	boolean_meta: Option<Arc<Table<'n>>>,
	function_meta: Option<Arc<Table<'n>>>,
	pub global: Arc<Table<'n>>
}

impl<'n> VirtualMachine<'n> {
	pub fn new(global: Arc<Table<'n>>) -> Self {
		Self {
			number_meta: None,
			string_meta: None,
			boolean_meta: None,
			function_meta: None,
			global
		}
	}

	/// Executes a function.
	pub fn execute(&self, function: &Function<'n>, arguments: Arc<Table<'n>>)
			-> Result<Arc<Table<'n>>, String> {
		let virtual_machine = self;
		let registers = vec![Default::default(); function.chunk.registers]
			.into_boxed_slice();

		StackFrame {virtual_machine, function, registers}.execute(arguments)
	}
}

struct StackFrame<'v, 'f, 'n> {
	virtual_machine: &'v VirtualMachine<'n>,
	function: &'f Function<'n>,
	registers: Box<[MaybeUpValue<'n>]>
}

impl<'v, 'f, 'n> StackFrame<'v, 'f, 'n> {
	/// Panics
	/// ------
	/// Panics if any encountered lock is poisoned.
	fn reference<'s>(&self, reference: impl Into<Reference<'s>>)
			-> Nillable<'n> {
		match reference.into() {
			Reference::Global(name) => {
				// TODO: Return this error? Like value.rs, there are a lot more of
				// these. (Breaking change.)
				let global = self.virtual_machine.global.data.lock()
					.expect("poison error");
				global.get(&Value::new_string(name)).nillable()
			},
			Reference::Local(id) => match &self.registers[id] {
				MaybeUpValue::Normal(value) => value.clone(),
				MaybeUpValue::UpValue(value) => value.lock()
					.expect("poison error").clone()
			}
		}
	}

	/// Panics
	/// ------
	/// Panics if any encountered lock is poisoned.
	fn write_reference<'s>(&mut self, reference: impl Into<Reference<'s>>,
			value: Nillable<'n>) {
		match reference.into() {
			Reference::Global(name) => {
				let mut global = self.virtual_machine.global.data.lock()
					.expect("poison error");

				match value {
					NonNil(value) => global.insert(Value::new_string(name), value),
					Nil => global.remove(&Value::new_string(name))
				};
			},
			Reference::Local(id) => match &mut self.registers[id] {
				MaybeUpValue::Normal(destination) =>
					*destination = value,
				MaybeUpValue::UpValue(destination) =>
					*destination.lock().expect("poison error") = value
			}
		}
	}

	/// Panics
	/// ------
	/// Panics if any encountered lock is poisoned.
	fn meta_method(&self, object: &Nillable<'n>, method: &str)
			-> Nillable<'n> {
		match object {
			NonNil(Value::Integer(_)) => {
				let meta = match self.virtual_machine.number_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().expect("poison error");
				meta.get(&Value::new_string(method)).nillable()
			},
			NonNil(Value::String(_)) => {
				let meta = match self.virtual_machine.string_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().expect("poison error");
				meta.get(&Value::new_string(method)).nillable()
			},
			NonNil(Value::Boolean(_)) => {
				let meta = match self.virtual_machine.boolean_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().expect("poison error");
				meta.get(&Value::new_string(method)).nillable()
			},
			NonNil(Value::Function(_)) | NonNil(Value::NativeFunction(_)) => {
				let meta = match self.virtual_machine.function_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().expect("poison error");
				meta.get(&Value::new_string(method)).nillable()
			},
			NonNil(Value::Table(table)) => match &*table.metatable
					.lock().expect("poison error") {
				Some(meta) => {
					let meta = meta.data.lock().expect("poison error");
					meta.get(&Value::new_string(method)).nillable()
				},
				None => Nil
			},
			NonNil(Value::UserData {meta, ..}) => match meta {
				Some(meta) => {
					let meta = meta.data.lock().expect("poison error");
					meta.get(&Value::new_string(method)).nillable()
				},
				None => Nil
			},
			Nil => Nil
		}
	}

	fn call(&self, function: Nillable<'n>, arguments: Arc<Table<'n>>)
			-> Result<Arc<Table<'n>>, String> {
		match function {
			NonNil(Value::Function(function)) =>
				self.virtual_machine.execute(&*function, arguments),
			NonNil(Value::NativeFunction(function)) =>
				function(arguments, self.virtual_machine),
			// NOTE: While this can cause unbounded recursion, this is actually what
			// happens in the main implementation.
			function if self.meta_method(&function, "__call").is_non_nil() => {
				arguments.tuple_insert(1, function.clone());
				let function = self.meta_method(&function, "__call");
				self.call(function, arguments)
			},
			function => Err(format!("attempt to call a {} value",
				function.type_name()))
		}
	}

	/// Panics
	/// ------
	/// Panics if any encountered lock is poisoned.
	fn execute(&mut self, arguments: Arc<Table<'n>>)
			-> Result<Arc<Table<'n>>, String> {
		// Set arguments.
		self.registers[0] = MaybeUpValue::Normal(NonNil(Value::Table(arguments)));

		let mut current_opcode = 0; // The current opcode we're evaluating.
		let chunk = &self.function.chunk;

		loop {
			if current_opcode == chunk.opcodes.len()
				{break Ok(Table::default().arc())}

			match chunk.opcodes[current_opcode] {
				OpCode::Call {function, arguments, destination} => {
					let arguments = match self.reference(arguments) {
						NonNil(Value::Table(arguments)) => arguments,
						value => break Err(format!("attempt to call a function with a {} value", value.type_name()))
					};

					let function = self.reference(function);
					let result = self.call(function, arguments)?;
					self.write_reference(destination, NonNil(Value::Table(result)));
				},

				OpCode::IndexRead {indexee, index, destination} => {
					let indexee = self.reference(indexee);
					let index = self.reference(index);
					self.index_read(indexee, index, destination)?;
				},

				OpCode::IndexWrite {indexee, index, value} =>
						match self.reference(indexee) {
					NonNil(Value::Table(table)) => {
						// TODO: Metatable
						let index = match self.reference(index) {
							NonNil(index) => index,
							Nil => break Err("table index is nil".to_owned())
						};
		
						let mut table = table.data.lock().expect("poison error");
						match self.reference(value) {
							NonNil(value) => table.insert(index, value),
							Nil => table.remove(&index)
						};
					},
		
					indexee => break Err(format!("attempt to index a {} value",
						indexee.type_name()))
				},

				OpCode::Create {destination, ..} => {self.write_reference(destination,
					NonNil(Value::Table(Table::default().arc())));},

				OpCode::BinaryOperation {left, right, operation, destination} =>
						self.write_reference(destination, match (self.reference(left),
							self.reference(right), operation) {
					// Arithmetic

					// Add
					// TODO: Handle overflow...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Add) => NonNil(Value::Integer(left + right)),
					(left, right, BinaryOperation::Add)
							if self.meta_method(&left, "__add").is_non_nil() => {
						let meta = self.meta_method(&left, "__add");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Add)
							if self.meta_method(&right, "__add").is_non_nil() => {
						let meta = self.meta_method(&right, "__add");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},

					// Subtract
					// TODO: Handle overflow...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Subtract) => NonNil(Value::Integer(left - right)),
					(left, right, BinaryOperation::Subtract)
							if self.meta_method(&left, "__sub").is_non_nil() => {
						let meta = self.meta_method(&left, "__sub");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Subtract)
							if self.meta_method(&right, "__sub").is_non_nil() => {
						let meta = self.meta_method(&right, "__sub");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},

					// Multiply
					// TODO: Handle overflow...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Multiply) => NonNil(Value::Integer(left * right)),
					(left, right, BinaryOperation::Multiply)
							if self.meta_method(&left, "__mul").is_non_nil() => {
						let meta = self.meta_method(&left, "__mul");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Multiply)
							if self.meta_method(&right, "__mul").is_non_nil() => {
						let meta = self.meta_method(&right, "__mul");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},

					// Divide
					// TODO: Handle overflow...
					// TODO: Handle division by zero...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Divide) => NonNil(Value::Integer(left / right)),
					(left, right, BinaryOperation::Divide)
							if self.meta_method(&left, "__div").is_non_nil() => {
						let meta = self.meta_method(&left, "__div");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Divide)
							if self.meta_method(&right, "__div").is_non_nil() => {
						let meta = self.meta_method(&right, "__div");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},

					// Relational

					// Equal
					(left, right, BinaryOperation::Equal) =>
						binary_equality!(self, left, right, ==, false),

					// Not Equal
					(left, right, BinaryOperation::NotEqual) =>
						binary_equality!(self, left, right, !=, true),

					// Less Than
					(left, right, BinaryOperation::LessThan) =>
						binary_comparison!(self, left, right, __lt, <, false),

					// Less Than Or Equal
					(left, right, BinaryOperation::LessThanOrEqual) =>
						binary_comparison!(self, left, right, __le, <=, false),

					// Greater Than
					(left, right, BinaryOperation::GreaterThan) =>
						binary_comparison!(self, left, right, __lt, >, true),

					// Greater Than Or Equal
					(left, right, BinaryOperation::GreaterThanOrEqual) =>
						binary_comparison!(self, left, right, __le, >=, true),

					// Other

					// Concat
					(NonNil(Value::String(left)), NonNil(Value::String(right)),
						BinaryOperation::Concat) => NonNil(Value::String(
							format!("{}{}", left, right).into_boxed_str())),
					(NonNil(Value::String(left)), NonNil(Value::Integer(right)),
						BinaryOperation::Concat) => NonNil(Value::String(
							format!("{}{}", left, right).into_boxed_str())),
					(NonNil(Value::Integer(left)), NonNil(Value::String(right)),
						BinaryOperation::Concat) => NonNil(Value::String(
							format!("{}{}", left, right).into_boxed_str())),
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
						BinaryOperation::Concat) => NonNil(Value::String(
							format!("{}{}", left, right).into_boxed_str())),
					(left, right, BinaryOperation::Concat)
							if self.meta_method(&left, "__concat").is_non_nil() => {
						let meta = self.meta_method(&left, "__concat");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Concat)
							if self.meta_method(&right, "__concat").is_non_nil() => {
						let meta = self.meta_method(&right, "__concat");
						let result = self.call(meta, lua_tuple![left, right].arc())?;
						result.index(&Value::Integer(1))
					},

					// TODO: Better error handling...
					_ => return Err("unknown binary operation error".to_owned())
				}),

				// NOTE: Providing two arguments on meta methods is required by spec,
				// because of historical reasons.
				OpCode::UnaryOperation {operand, operation, destination} =>
						self.write_reference(destination, match (
							self.reference(operand), operation) {
					// Arithmetic

					// Negate
					(NonNil(Value::Integer(operand)), UnaryOperation::Negate) =>
						NonNil(Value::Integer(-operand)),
					(operand, UnaryOperation::Negate)
							if self.meta_method(&operand, "__unm").is_non_nil() => {
						let meta = self.meta_method(&operand, "__unm");
						let result = self.call(meta, lua_tuple![&operand, operand].arc())?;
						result.index(&Value::Integer(1))
					},
					(operand, UnaryOperation::Negate) =>
						return Err(format!("attempt to perform arithmetic on a {} value",
							operand.type_name())),

					// Logical

					// Not
					(operand, UnaryOperation::LogicalNot) =>
						NonNil(Value::Boolean(!operand.coerce_to_bool())),

					// Other

					// Length
					// TODO: Would this become a float if it would overflow an i64?
					(NonNil(Value::String(operand)), UnaryOperation::Length) =>
						NonNil(Value::Integer(operand.len() as i64)),
					(operand, UnaryOperation::Length)
							if self.meta_method(&operand, "__len").is_non_nil() => {
						let meta = self.meta_method(&operand, "__len");
						let result = self.call(meta, lua_tuple![&operand, &operand].arc())?;
						result.index(&Value::Integer(1))
					},
					(NonNil(Value::Table(operand)), UnaryOperation::Length) => {
						let operand = operand.data.lock().expect("poison error");
						// NOTE: This is not what the main lua implementation does, but it
						// is valid, as the spec says the length operator may return *any
						// border*. The table length operator in lua just may not always
						// return the last border of the table. See more at:
						// https://www.lua.org/manual/5.4/manual.html#3.4.7
						let length = operand.keys()
							.filter_map(|key| match key {
								Value::Integer(key) => Some(*key),
								_ => None
							})
							.max().map(|key| max(key, 0)).unwrap_or(0);
						NonNil(Value::Integer(length))
					},
					(operand, UnaryOperation::Length) =>
						return Err(format!("attempt to get length of a {} value",
							operand.type_name())),

					_ => return Err("unknown unary operation error".to_owned())
				}),

				OpCode::Jump {operation, r#if: None} => {
					current_opcode = operation as usize;
					continue;
				},

				OpCode::Jump {operation, r#if: Some(r#if)} => {
					let r#if = self.reference(r#if);
					if r#if.nillable().coerce_to_bool() {
						current_opcode = operation as usize;
						continue
					}
				},

				OpCode::Return {result} => match self.registers.get(result) {
					Some(&MaybeUpValue::Normal(NonNil(Value::Table(ref result)))) =>
						return Ok(result.clone()),
					Some(&MaybeUpValue::Normal(ref value)) =>
						return Err(format!("attempt to return a {} value", value.type_name())),
					Some(&MaybeUpValue::UpValue(ref value)) =>
							match &*value.lock().expect("poison error") {
						NonNil(Value::Table(result)) => return Ok(result.clone()),
						value => return Err(format!("attempt to return a {} value", value.type_name()))
					},
					None => return Err("attempt to access a non existant register".to_owned())
				},

				OpCode::ReAssign {actor, destination, ..} => {
					// TODO: error handling
					self.registers[destination] = self.registers[actor].clone();
				},

				OpCode::LoadConst {constant, register, ..} => {
					let constant = self.function.chunk.constants.get(constant as usize);
					let constant = match constant.cloned() {
						Some(Constant::String(string)) =>
							NonNil(Value::String(string.into_boxed_str())),
						Some(Constant::Integer(integer)) => NonNil(Value::Integer(integer)),
						Some(Constant::Boolean(boolean)) => NonNil(Value::Boolean(boolean)),
						Some(Constant::Chunk(chunk)) => {
							let up_values = chunk.up_values.iter()
								.map(|(up_value, use_up_value)| match use_up_value {
									true => self.function.up_values[*up_value].clone(),
									false => self.registers[*up_value].up_value().clone()
								})
								.collect::<Vec<_>>().into_boxed_slice();
							NonNil(Value::Function(Function {up_values, chunk}.arc()))
						},
						None => Nil
					};
					self.write_reference(register, constant);
				},

				OpCode::LoadGlobal {global, register} => {
					let value = self.reference(global);
					self.write_reference(register, value);
				},

				OpCode::SaveGlobal {register, global} => {
					let value = self.reference(register);
					self.write_reference(global, value);
				},

				OpCode::LoadUpValue {register, up_value} => {
					// TODO: Error handling.
					let up_value = self.function.up_values[up_value]
						.lock().expect("poison error");
					self.registers[register] = MaybeUpValue::Normal(up_value.clone());
				},

				OpCode::SaveUpValue {up_value, register} => {
					// TODO: Error handling.
					let mut up_value = self.function.up_values[up_value]
						.lock().expect("poison error");
					*up_value = match self.registers[register] {
						MaybeUpValue::Normal(ref value) => value.clone(),
						MaybeUpValue::UpValue(ref value) => {
							value.lock().expect("poison error").clone()
						}
					};
				},

				OpCode::NoOp => ()
			}

			current_opcode += 1;
		}
	}

	/// Panics
	/// ------
	/// Panics if any encountered lock is poisoned.
	fn index_read(&mut self, indexee: Nillable<'n>,
			index: Nillable<'n>, destination: usize) -> Result<(), String> {
		enum IndexOperation<'n> {
			Value(Nillable<'n>),
			NonTable,
			NilIndex
		}

		// Triple match FTW
		match match match (&indexee, &index) {
			(NonNil(Value::Table(table)), NonNil(index)) => {
				let data = table.data.lock().expect("poison error");
				IndexOperation::Value(data.get(index).nillable())
			},
			(NonNil(Value::Table(_)), Nil) => IndexOperation::NilIndex,
			_ => IndexOperation::NonTable
		} {
			operation @ IndexOperation::NilIndex |
					operation @ IndexOperation::NonTable |
					operation @ IndexOperation::Value(Nil) => {
				match self.meta_method(&indexee, "__index") {
					// NOTE: Despite possibly being infinitely recursive, this is what
					// happens in standard.
					proto @ NonNil(Value::Table(_)) =>
						return self.index_read(proto, index, destination),
					function @ NonNil(Value::Function(_)) => {
						let arguments = lua_tuple![&indexee, index].arc();
						let result = self.call(function, arguments)?;
						IndexOperation::Value(result.index(&Value::Integer(1)))
					},
					_ => operation
				}
			},
			IndexOperation::Value(NonNil(value)) =>
				IndexOperation::Value(NonNil(value))
		} {
			IndexOperation::Value(value) =>
				self.write_reference(destination, value.clone()),
			IndexOperation::NilIndex => return Err(
				"table index is nil".to_owned()),
			IndexOperation::NonTable => return Err(
				format!("attempt to index a {} value", indexee.type_name()))
		}

		Ok(())
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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
		function: usize,
		/// The name of the arguments array, in scope. Must be a table or else an
		/// error will be thrown.
		arguments: usize,
		/// The name of where the return values will be stored.
		destination: usize
	},

	/// Indexes into the object with the name [indexee], with index [index], and
	/// stores the result in [destination].
	///
	/// [destination_local] determines if the result will be stored to the local
	/// scope or global scope.
	IndexRead {
		/// The name of the object to be indexed.
		indexee: usize,
		/// The name of the object that serves as the index.
		index: usize,
		/// The name of where the result will be stored.
		destination: usize
	},

	/// Indexes into the object with the name [indexee], with index [index], and
	/// writes [value] into [indexee].
	IndexWrite {
		/// The name of the object to be indexed.
		indexee: usize,
		/// The name of the object that serves as the index.
		index: usize,
		/// The name of the value to be stored within the indexee.
		value: usize
	},

	/// Creates a new empty table at [destination].
	///
	/// [destination_local] determines if the new table will be stored to the
	/// local scope or global scope.
	Create {
		/// The name of where the new table will be stored.
		destination: usize
	},

	BinaryOperation {
		left: usize,
		right: usize,
		operation: BinaryOperation,
		destination: usize
	},

	UnaryOperation {
		operand: usize,
		operation: UnaryOperation,
		destination: usize,
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
		r#if: Option<usize>
	},

	Return {
		result: usize
	},

	ReAssign {
		actor: usize,
		destination: usize
	},

	/// Loads a value from the constant pool at index [constant] to [destination].
	///
	/// [destination_local] determines if the constant will be stored to the local
	/// scope or global scope.
	LoadConst {
		/// The constant to be loaded.
		constant: u16,
		/// The name of where the constant will be stored.
		register: usize
	},

	LoadGlobal {
		global: &'s str,
		register: usize
	},

	SaveGlobal {
		register: usize,
		global: &'s str
	},

	LoadUpValue {
		up_value: usize,
		register: usize
	},

	SaveUpValue {
		register: usize,
		up_value: usize
	},

	/*
	LoadUpValue {
		up_value: usize,
		destination: usize
	},

	WriteUpValue {
		up_value: usize,
		value: &'s str
	},

	WriteUpValueLocal {
		up_value: usize,
		value: usize
	},

	CallLocal {
		function: usize,
		arguments: usize,
		destination: &'s str
	},

	CallToLocal {
		function: &'s str,
		arguments: usize,
		destination: usize
	},

	CallLocalToLocal {
		function: usize,
		arguments: usize,
		destination: usize
	},


	*/

	NoOp
}

impl<'s> Display for OpCode<'s> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Call {function, arguments, destination} =>
				write!(f, "call {}, {}, {};", function, arguments, destination),
			Self::IndexRead {indexee, index, destination} =>
				write!(f, "idxr {}, {}, {};", indexee, index, destination),
			Self::IndexWrite {indexee, index, value} =>
				write!(f, "idxw {}, {}, {};", indexee, index, value),
			Self::Create {destination} =>
				write!(f, "crt {};", destination),
			Self::BinaryOperation {left, right, destination, operation} =>
					match operation {
				// Arithmetic
				BinaryOperation::Add =>
					write!(f, "aadd {}, {}, {};", left, right, destination),
				BinaryOperation::Subtract =>
					write!(f, "asub {}, {}, {};", left, right, destination),
				BinaryOperation::Multiply =>
					write!(f, "amul {}, {}, {};", left, right, destination),
				BinaryOperation::Divide =>
					write!(f, "adiv {}, {}, {};", left, right, destination),
				BinaryOperation::FloorDivide =>
					write!(f, "afdiv {}, {}, {};", left, right, destination),
				BinaryOperation::Exponent =>
					write!(f, "aexp {}, {}, {};", left, right, destination),
				BinaryOperation::Modulo =>
					write!(f, "amod {}, {}, {};", left, right, destination),

				// Bitwise
				BinaryOperation::BitwiseAnd =>
					write!(f, "band {}, {}, {};", left, right, destination),
				BinaryOperation::BitwiseOr =>
					write!(f, "bor {}, {}, {};", left, right, destination),
				BinaryOperation::BitwiseXOr =>
					write!(f, "bxor {}, {}, {};", left, right, destination),
				BinaryOperation::ShiftLeft =>
					write!(f, "bshl {}, {}, {};", left, right, destination),
				BinaryOperation::ShiftRight =>
					write!(f, "bshr {}, {}, {};", left, right, destination),

				// Relational
				BinaryOperation::Equal =>
					write!(f, "req {}, {}, {};", left, right, destination),
				BinaryOperation::NotEqual =>
					write!(f, "rne {}, {}, {};", left, right, destination),
				BinaryOperation::LessThan =>
					write!(f, "rlt {}, {}, {};", left, right, destination),
				BinaryOperation::LessThanOrEqual =>
					write!(f, "rle {}, {}, {};", left, right, destination),
				BinaryOperation::GreaterThan =>
					write!(f, "rgt {}, {}, {};", left, right, destination),
				BinaryOperation::GreaterThanOrEqual =>
					write!(f, "rge {}, {}, {};", left, right, destination),

				// Other
				BinaryOperation::Concat =>
					write!(f, "cat {}, {}, {};", left, right, destination)
			},
			Self::UnaryOperation {operand, destination, operation} =>
					match operation {
				// Arithmetic
				UnaryOperation::Negate =>
					write!(f, "aneg {}, {};", operand, destination),

				// Bitwise
				UnaryOperation::BitwiseNot =>
					write!(f, "bnot {}, {};", operand, destination),

				// Logical
				UnaryOperation::LogicalNot =>
					write!(f, "not {}, {};", operand, destination),

				// Other
				UnaryOperation::Length =>
					write!(f, "len {}, {};", operand, destination)
			},
			Self::Jump {operation, r#if: None} =>
				write!(f, "jmp {};", operation),
			Self::Jump {operation, r#if: Some(r#if)} =>
				write!(f, "cjmp {}, {};", operation, r#if),
			Self::Return {result} =>
				write!(f, "ret {};", result),
			Self::ReAssign {actor, destination} =>
				write!(f, "reas {}, {};", actor, destination),
			Self::LoadConst {constant, register} =>
				write!(f, "lcst [{}], {};", constant, register),
			Self::LoadGlobal {global, register} =>
				write!(f, "lglb {{{}}}, {};", global, register),
			Self::SaveGlobal {register, global} =>
				write!(f, "sglb {}, {{{}}};", register, global),
			Self::LoadUpValue {up_value, register} =>
				write!(f, "luv ^{}, {};", up_value, register),
			Self::SaveUpValue {register, up_value} =>
				write!(f, "suv {}, ^{};", register, up_value),
			Self::NoOp =>
				write!(f, "noop")
		}
	}
}

// TODO: Should we remove [crate::ast::parser::BinaryOperator] and use this
// instead? Same goes for UnaryOperation and Operator.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOperation {
	// Arithmetic
	Add,
	Subtract,
	Multiply,
	Divide,
	FloorDivide,
	Modulo,
	Exponent,

	// Bitwise
	BitwiseAnd,
	BitwiseOr,
	BitwiseXOr,
	ShiftLeft,
	ShiftRight,

	// Relational
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,

	// Other
	Concat
}

impl TryFrom<BinaryOperator> for BinaryOperation {
	type Error = ();

	fn try_from(ast: BinaryOperator) -> Result<Self, ()> {
		Ok(match ast {
			// Arithmetic
			BinaryOperator::Add => Self::Add,
			BinaryOperator::Subtract => Self::Subtract,
			BinaryOperator::Multiply => Self::Multiply,
			BinaryOperator::Divide => Self::Divide,
			BinaryOperator::FloorDivide => Self::FloorDivide,
			BinaryOperator::Modulo => Self::Modulo,
			BinaryOperator::Exponent => Self::Exponent,

			// Bitwise
			BinaryOperator::BitwiseAnd => Self::BitwiseAnd,
			BinaryOperator::BitwiseOr => Self::BitwiseOr,
			BinaryOperator::BitwiseXOr => Self::BitwiseXOr,
			BinaryOperator::ShiftLeft => Self::ShiftLeft,
			BinaryOperator::ShiftRight => Self::ShiftRight,

			// Relational
			BinaryOperator::Equal => Self::Equal,
			BinaryOperator::NotEqual => Self::NotEqual,
			BinaryOperator::LessThan => Self::LessThan,
			BinaryOperator::LessThanOrEqual => Self::LessThanOrEqual,
			BinaryOperator::GreaterThan => Self::GreaterThan,
			BinaryOperator::GreaterThanOrEqual => Self::GreaterThanOrEqual,

			// Other
			BinaryOperator::Concat => Self::Concat,
			_ => return Err(())
		})
	}
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOperation {
	// Arithmetic
	Negate,

	// Bitwise
	BitwiseNot,

	// Logical
	LogicalNot,

	// Other
	Length
}

impl From<UnaryOperator> for UnaryOperation {
	fn from(ast: UnaryOperator) -> Self {
		match ast {
			// Arithmetic
			UnaryOperator::Negate => Self::Negate,

			// Bitwise
			UnaryOperator::BitwiseNot => Self::BitwiseNot,

			// Logical
			UnaryOperator::LogicalNot => Self::LogicalNot,

			// Other
			UnaryOperator::Length => Self::Length
		}
	}
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Chunk {
	pub registers: usize,
	pub up_values: Vec<(usize, bool)>,
	pub constants: Vec<Constant>,
	pub opcodes: Vec<OpCode<'static>>
}

impl Chunk {
	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	fn recursive_fmt(&self, f: &mut Formatter, id: &mut usize) -> FMTResult {
		let my_id = *id;
		*id += 1;

		let mut id_map = HashMap::new();
		self.constants.iter().enumerate()
			.try_for_each(|(index, constant)| match constant {
				Constant::Chunk(chunk) => {
					id_map.insert(index, *id);
					chunk.recursive_fmt(f, id)
				},
				_ => Ok(())
			})?;

		write!(f, ".function{}<{}>;", my_id, self.registers)?;
		self.constants.iter().enumerate()
			.try_for_each(|(index, constant)| match (id_map.get(&index), constant) {
				(Some(id), _) if index == 0 => write!(f, " .function{}", id),
				(Some(id), _) => write!(f, ", .function{}", id),
				(_, constant) if index == 0 => {write!(f, " ")?; constant.fmt(f)},
				(_, constant) => {write!(f, " ")?; constant.fmt(f)}
			})?;
		writeln!(f, ";")?;

		self.opcodes.iter().enumerate()
			.try_for_each(|(index, opcode)| {
				if index != 0 {writeln!(f)?}
				opcode.fmt(f)
			})?;
		writeln!(f, "\n")
	}
}

impl Display for Chunk {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		let mut id = 0;
		self.recursive_fmt(f, &mut id)
	}
}

#[macro_export]
macro_rules! byte_code {
	($($code:tt)*) => {{
		use $crate::byte_code_inner;

		#[allow(non_camel_case_types)]
		struct byte_code<F>
				where F: Fn(&mut usize) -> Option<OpCode<'static>> + 'static {
			index: usize,
			next: F,
			//_d: std::marker::PhantomData<&'f ()>
		}

		impl<F> Iterator for byte_code<F>
				where F: Fn(&mut usize) -> Option<OpCode<'static>> {
			type Item = OpCode<'static>;

			fn next(&mut self) -> Option<OpCode<'static>> {
				(self.next)(&mut self.index)
			}
		}

		byte_code {
			index: 0,
			next: move |index| {
				#[allow(unused_assignments)]
				{
					let mut counter = 0;
					let value = byte_code_inner!(index counter {$($code)*});
					if let Some(_) = &value {*index += 1}
					value
				}
			},
			//_d: std::marker::PhantomData
		}
	}}
}

#[macro_export]
macro_rules! byte_code_inner {
	($index:ident $counter:ident {call $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::Call {function: $a, arguments: $b, destination: $c})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {idxr $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::IndexRead {indexee: $a, index: $b, destination: $c})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {idxw $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::IndexWrite {indexee: $a, index: $b, value: $c})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {crt $a:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::Create {destination: $a})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Arithmetic
	($index:ident $counter:ident {aadd $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Add})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {asub $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Subtract})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {amul $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Multiply})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {adiv $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Divide})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {afdiv $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::FloorDivide})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {aexp $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Exponent})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {amod $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Modulo})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Bitwise
	($index:ident $counter:ident {band $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::BitwiseAnd})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {bor $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::BitwiseOr})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {bxor $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::BitwiseXOr})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {bshl $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::ShiftLeft})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {bshr $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::ShiftRight})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Relational
	($index:ident $counter:ident {req $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Equal})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {rne $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::NotEqual})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {rlt $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::LessThan})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {rle $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::LessThanOrEqual})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {rgt $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::GreaterThan})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {rge $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::GreaterThanOrEqual})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Other
	($index:ident $counter:ident {cat $a:expr, $b:expr, $c:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::BinaryOperation {left: $a, right: $b, destination: $c, operation: BinaryOperation::Concat})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Arithmetic
	($index:ident $counter:ident {aneg $a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::UnaryOperation {operand: $a, destination: $b, operation: UnaryOperation::Negate})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Bitwise
	($index:ident $counter:ident {bnot $a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::UnaryOperation {operand: $a, destination: $b, operation: UnaryOperation::BitwiseNot})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Logical
	($index:ident $counter:ident {not $a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::UnaryOperation {operand: $a, destination: $b, operation: UnaryOperation::LogicalNot})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	// Other
	($index:ident $counter:ident {len $a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::UnaryOperation {operand: $a, destination: $b, operation: UnaryOperation::Length})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};

	($index:ident $counter:ident {jmp $a:ident $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::Jump {operation: $a, r#if: None})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {cjmp $a:ident, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::Jump {operation: $a, r#if: Some($b)})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {ret $a:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::Return {result: $a})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {reas $a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::ReAssign {actor: $a, destination: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {lcst [$a:expr], $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::LoadConst {constant: $a, register: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {lglb {$a:expr}, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::LoadGlobal {global: $a, register: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {sglb $a:expr, {$b:expr} $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::SaveGlobal {register: $a, global: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {luv ^$a:expr, $b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::LoadUpValue {up_value: $a, register: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {suv $a:expr, ^$b:expr $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::SaveUpValue {register: $a, up_value: $b})
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {noop $(; $($rest:tt)*)?}) => {
		if *$index == $counter {
			Some(OpCode::NoOp)
		} else {
			$counter += 1;
			byte_code_inner!($index $counter {$($($rest)*)?})
		}
	};
	($index:ident $counter:ident {}) => {None}
}
