pub mod constant;
pub mod value;

use self::{
	super::ast::parser::{BinaryOperator, UnaryOperator},
	constant::Constant,
	value::{
		Function, IntoNillableValue, MaybeUpValue, NillableValue, Nil, NonNil,
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

macro_rules! binary_relational {
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
				let arguments = if $reverse {[&right, &left]} else {[&left, &right]};
				let result = $self.call(meta, Table::array(arguments).arc())?;
				NonNil(result.index(&Value::Integer(1)).coerce_to_boolean())
			},
			(left, right)
					if $self.meta_method(&right, stringify!($name)).is_non_nil() => {
				let meta = $self.meta_method(&left, stringify!($name));
				let arguments = if $reverse {[&right, &left]} else {[&left, &right]};
				let result = $self.call(meta, Table::array(arguments).arc())?;
				NonNil(result.index(&Value::Integer(1)).coerce_to_boolean())
			},

			_ => return Err("unknown binary operation error".to_owned())
		}
	}}
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

pub struct VirtualMachine {
	number_meta: Option<Arc<Table>>,
	string_meta: Option<Arc<Table>>,
	boolean_meta: Option<Arc<Table>>,
	function_meta: Option<Arc<Table>>,
	global: Arc<Table>
}

impl VirtualMachine {
	pub fn new(global: Arc<Table>) -> Self {
		Self {
			number_meta: None,
			string_meta: None,
			boolean_meta: None,
			function_meta: None,
			global
		}
	}

	/// Executes a function.
	pub fn execute(&self, function: &Function, arguments: Arc<Table>)
			-> Result<Arc<Table>, String> {
		StackFrame {
			virtual_machine: &self,
			function,
			registers: vec![Default::default(); function.chunk.registers].into_boxed_slice()
		}.execute(arguments)
	}
}

struct StackFrame<'v, 'f> {
	virtual_machine: &'v VirtualMachine,
	function: &'f Function,
	registers: Box<[self::value::MaybeUpValue]>
}

impl<'v, 'f> StackFrame<'v, 'f> {
	fn reference<'s>(&self, reference: impl Into<Reference<'s>>)
			-> NillableValue<Value> {
		match reference.into() {
			Reference::Global(name) => {
				let global = self.virtual_machine.global.data.lock().unwrap();
				global.get(&Value::new_string(name)).nillable().cloned()
			},
			Reference::Local(id) => match &self.registers[id] {
				MaybeUpValue::Normal(value) => value.clone(),
				MaybeUpValue::UpValue(value) => value.lock().unwrap().clone()
			}
		}
	}

	fn write_reference<'s>(&mut self, reference: impl Into<Reference<'s>>,
			value: NillableValue<Value>) {
		match reference.into() {
			Reference::Global(name) => {
				let mut global = self.virtual_machine.global.data.lock().unwrap();

				match value {
					NonNil(value) => global.insert(Value::new_string(name), value),
					Nil => global.remove(&Value::new_string(name))
				};
			},
			Reference::Local(id) => match &mut self.registers[id] {
				MaybeUpValue::Normal(destination) =>
					*destination = value,
				MaybeUpValue::UpValue(destination) =>
					*destination.lock().unwrap() = value
			}
		}
	}

	fn meta_method(&self, object: &NillableValue<Value>, method: &str)
			-> NillableValue<Value> {
		match object {
			NonNil(Value::Integer(_)) => {
				let meta = match self.virtual_machine.number_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().unwrap();
				meta.get(&Value::new_string(method)).nillable().cloned()
			},
			NonNil(Value::String(_)) => {
				let meta = match self.virtual_machine.string_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().unwrap();
				meta.get(&Value::new_string(method)).nillable().cloned()
			},
			NonNil(Value::Boolean(_)) => {
				let meta = match self.virtual_machine.boolean_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().unwrap();
				meta.get(&Value::new_string(method)).nillable().cloned()
			},
			NonNil(Value::Function(_)) | NonNil(Value::NativeFunction(_)) => {
				let meta = match self.virtual_machine.function_meta.as_ref() {
					Some(meta) => meta,
					None => return Nil
				};

				let meta = meta.data.lock().unwrap();
				meta.get(&Value::new_string(method)).nillable().cloned()
			},
			NonNil(Value::Table(table)) => match &*table.metatable.lock().unwrap() {
				Some(meta) => {
					let meta = meta.data.lock().unwrap();
					meta.get(&Value::new_string(method)).nillable().cloned()
				},
				None => Nil
			},
			_ => Nil
		}
	}

	fn call(&self, function: NillableValue<Value>, arguments: Arc<Table>)
			-> Result<Arc<Table>, String> {
		match function {
			NonNil(Value::Function(function)) =>
				self.virtual_machine.execute(&*function, arguments),
			NonNil(Value::NativeFunction(function)) =>
				function(arguments, self.virtual_machine.global.clone()),
			// NOTE: While this can cause unbounded recursion, this is actually what
			// happens in the main implementation.
			function if self.meta_method(&function, "__call").is_non_nil() => {
				let arguments = arguments.data.lock().unwrap();
				let mut arguments = arguments.iter()
					.map(|(key, value)| match key {
						Value::Integer(index) => (Value::Integer(index + 1), value.clone()),
						key => (key.clone(), value.clone())
					}).collect::<HashMap<_, _>>();
				match &function {
					NonNil(function) => arguments.insert(
						Value::Integer(1), function.clone()),
					Nil => arguments.remove(&Value::Integer(1))
				};

				let function = self.meta_method(&function, "__call");
				self.call(function, Table::from_hashmap(arguments).arc())
			},
			function => Err(format!("attempt to call a {} value",
				function.type_name()))
		}
	}

	fn execute(&mut self, arguments: Arc<Table>) -> Result<Arc<Table>, String> {
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

				OpCode::IndexRead {indexee, index, destination, ..} =>
						match self.reference(indexee) {
					NonNil(Value::Table(table)) => {
						// TODO: Metatable
						let index = match self.reference(index) {
							NonNil(index) => index,
							Nil => return Err("table index is nil".to_owned())
						};
		
						let table = table.data.lock().unwrap();
						self.write_reference(destination,
							table.get(&index).nillable().cloned());
					},
		
					indexee => break Err(format!("attempt to index a {} value",
						indexee.type_name()))
				},

				OpCode::IndexWrite {indexee, index, value} =>
						match self.reference(indexee) {
					NonNil(Value::Table(table)) => {
						// TODO: Metatable
						let index = match self.reference(index) {
							NonNil(index) => index,
							Nil => break Err("table index is nil".to_owned())
						};
		
						let mut table = table.data.lock().unwrap();
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
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Add)
							if self.meta_method(&right, "__add").is_non_nil() => {
						let meta = self.meta_method(&right, "__add");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},

					// Subtract
					// TODO: Handle overflow...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Subtract) => NonNil(Value::Integer(left - right)),
					(left, right, BinaryOperation::Subtract)
							if self.meta_method(&left, "__sub").is_non_nil() => {
						let meta = self.meta_method(&left, "__sub");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Subtract)
							if self.meta_method(&right, "__sub").is_non_nil() => {
						let meta = self.meta_method(&right, "__sub");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},

					// Multiply
					// TODO: Handle overflow...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Multiply) => NonNil(Value::Integer(left * right)),
					(left, right, BinaryOperation::Multiply)
							if self.meta_method(&left, "__mul").is_non_nil() => {
						let meta = self.meta_method(&left, "__mul");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Multiply)
							if self.meta_method(&right, "__mul").is_non_nil() => {
						let meta = self.meta_method(&right, "__mul");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},

					// Multiply
					// TODO: Handle overflow...
					// TODO: Handle division by zero...
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
							BinaryOperation::Divide) => NonNil(Value::Integer(left / right)),
					(left, right, BinaryOperation::Divide)
							if self.meta_method(&left, "__div").is_non_nil() => {
						let meta = self.meta_method(&left, "__div");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(left, right, BinaryOperation::Divide)
							if self.meta_method(&right, "__div").is_non_nil() => {
						let meta = self.meta_method(&right, "__div");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},

					// Relational

					// Equal
					(Nil, Nil, BinaryOperation::Equal) => NonNil(Value::Boolean(true)),
					(NonNil(Value::Integer(left)), NonNil(Value::Integer(right)),
						BinaryOperation::Equal) => NonNil(Value::Boolean(left == right)),
					(NonNil(Value::Boolean(left)), NonNil(Value::Boolean(right)),
						BinaryOperation::Equal) => NonNil(Value::Boolean(left == right)),
					(NonNil(Value::String(left)), NonNil(Value::String(right)),
						BinaryOperation::Equal) => NonNil(Value::Boolean(left == right)),
					(NonNil(Value::Function(left)), NonNil(Value::Function(right)),
						BinaryOperation::Equal) => NonNil(Value::Boolean(left == right)),
					(NonNil(Value::NativeFunction(left)),
						NonNil(Value::NativeFunction(right)), BinaryOperation::Equal) =>
							NonNil(Value::Boolean(left == right)),
					(left @ NonNil(Value::Table(_)), right @ NonNil(Value::Table(_)),
							BinaryOperation::Equal) if self.meta_method(&left, "__eq")
								.is_non_nil() => {
						let meta = self.meta_method(&left, "__eq");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(left @ NonNil(Value::Table(_)), right @ NonNil(Value::Table(_)),
							BinaryOperation::Equal) if self.meta_method(&right, "__eq")
								.is_non_nil() => {
						let meta = self.meta_method(&right, "__eq");
						let result = self.call(meta, Table::array([&left, &right]).arc())?;
						result.index(&Value::Integer(1))
					},
					(NonNil(Value::Table(left)), NonNil(Value::Table(right)),
						BinaryOperation::Equal) => NonNil(Value::Boolean(left == right)),
					(_, _, BinaryOperation::Equal) => NonNil(Value::Boolean(false)),

					// Less Than
					(left, right, BinaryOperation::LessThan) =>
						binary_relational!(self, left, right, __lt, <, false),

					// Less Than Or Equal
					(left, right, BinaryOperation::LessThanOrEqual) =>
						binary_relational!(self, left, right, __le, <=, false),

					// Greater Than
					(left, right, BinaryOperation::GreaterThan) =>
						binary_relational!(self, left, right, __lt, >, true),

					// Greater Than Or Equal
					(left, right, BinaryOperation::GreaterThanOrEqual) =>
						binary_relational!(self, left, right, __le, >=, true),

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
						let result = self.call(meta,
							Table::array([&operand, &operand]).arc())?;
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
						let result = self.call(meta,
							Table::array([&operand, &operand]).arc())?;
						result.index(&Value::Integer(1))
					},
					(NonNil(Value::Table(operand)), UnaryOperation::Length) => {
						let operand = operand.data.lock().unwrap();
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
							match &*value.lock().unwrap() {
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
					let up_value = self.function.up_values[up_value].lock().unwrap();
					self.registers[register] = MaybeUpValue::Normal(up_value.clone());
				},

				OpCode::SaveUpValue {up_value, register} => {
					// TODO: Error handling.
					let mut up_value = self.function.up_values[up_value].lock().unwrap();
					*up_value = match self.registers[register] {
						MaybeUpValue::Normal(ref value) => value.clone(),
						MaybeUpValue::UpValue(ref value) => {
							value.lock().unwrap().clone()
						}
					};
				},

				OpCode::NoOp => ()
			}

			current_opcode = current_opcode + 1;
		}
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
				write!(f, "call {} {} {}", function, arguments, destination),
			Self::IndexRead {indexee, index, destination} =>
				write!(f, "idxr {} {} {}", indexee, index, destination),
			Self::IndexWrite {indexee, index, value} =>
				write!(f, "idxw {} {} {}", indexee, index, value),
			Self::Create {destination} =>
				write!(f, "crt {}", destination),
			Self::BinaryOperation {left, right, destination, ..} =>
				write!(f, "biop {} {} {}", left, right, destination),
			Self::UnaryOperation {operand, destination, ..} =>
				write!(f, "unop {} {}", operand, destination),
			Self::Jump {operation, r#if: None} =>
				write!(f, "jmp {}", operation),
			Self::Jump {operation, r#if: Some(r#if)} =>
				write!(f, "cjmp {} {}", operation, r#if),
			Self::Return {result} =>
				write!(f, "ret {}", result),
			Self::ReAssign {actor, destination} =>
				write!(f, "reas {} {}", actor, destination),
			Self::LoadConst {constant, register} =>
				write!(f, "lcst <{}> {}", constant, register),
			Self::LoadGlobal {global, register} =>
				write!(f, "lglb ({}) {}", global, register),
			Self::SaveGlobal {register, global} =>
				write!(f, "sglb {} ({})", register, global),
			Self::LoadUpValue {up_value, register} =>
				write!(f, "luv [{}] {}", up_value, register),
			Self::SaveUpValue {register, up_value} =>
				write!(f, "suv {} [{}]", register, up_value),
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
}

impl Display for Chunk {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "registers: {}\nconstants:\n", self.registers)?;
		self.constants.iter().enumerate()
			.try_for_each(|(index, constant)| {
				if index != 0 {write!(f, "\n")?}
				write!(f, "\t{}: ", index)?; constant.fmt(f)
			})?;
		write!(f, "\n")?;
		self.opcodes.iter().enumerate()
			.try_for_each(|(index, opcode)| {
				if index != 0 {write!(f, "\n")?}
				write!(f, "{}: ", index)?; opcode.fmt(f)
			})
	}
}
