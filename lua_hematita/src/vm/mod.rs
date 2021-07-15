pub mod constant;
pub mod value;

use self::{
	constant::Constant,
	value::{Function, IntoNillableValue, MaybeUpValue, NillableValue, Nil, NonNil, Table, Value}
};
use std::{fmt::{Display, Formatter, Result as FMTResult}, sync::Arc};

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
	global: Arc<Table>
}

impl VirtualMachine {
	pub fn new(global: Arc<Table>) -> Self {
		Self {
			number_meta: None,
			string_meta: None,
			boolean_meta: None,
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
	fn read_reference<'s>(&mut self, reference: impl Into<Reference<'s>>)
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
					let arguments = match self.read_reference(arguments) {
						NonNil(Value::Table(arguments)) => arguments,
						value => break Err(format!("attempt to call a function with a {} value", value.type_name()))
					};
			
					match self.read_reference(function) {
						NonNil(Value::Function(function)) => {
							let v = self.virtual_machine.execute(&*function, arguments)?;
							let v = v.data.lock().unwrap();
							self.write_reference(destination, v.get(&Value::Integer(1)).nillable().cloned());
							// TODO: Tuple stuff!
							/*self.write_reference(destination,
								NonNil(Value::Table(self.virtual_machine.execute(&*function)?)));*/
						},
			
						NonNil(Value::NativeFunction(function)) => {
							let result = function(arguments, Table::default().arc())?;
							// TODO: Returning tuples.
							let result = result.data.lock().unwrap();
							self.write_reference(destination, result.get(&Value::Integer(1)).nillable().cloned());
						},
			
						function => break Err(format!("attempt to call a {} value",
							function.type_name()))
					}
				},

				OpCode::IndexRead {indexee, index, destination, ..} =>
						match self.read_reference(indexee) {
					NonNil(Value::Table(table)) => {
						// TODO: Metatable
						let index = match self.read_reference(index) {
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
						match self.read_reference(indexee) {
					NonNil(Value::Table(table)) => {
						// TODO: Metatable
						let index = match self.read_reference(index) {
							NonNil(index) => index,
							Nil => break Err("table index is nil".to_owned())
						};
		
						let mut table = table.data.lock().unwrap();
						match self.read_reference(value) {
							NonNil(value) => table.insert(index, value),
							Nil => table.remove(&index)
						};
					},
		
					indexee => break Err(format!("attempt to index a {} value",
						indexee.type_name()))
				},

				OpCode::Create {destination, ..} => {self.write_reference(destination,
					NonNil(Value::Table(Table::default().arc())));},

				OpCode::BinaryOperation {..} => {
					/*
					let first = retrieve(&Value::new_string(first),
						&mut local, &global).nillable();
					let second = retrieve(&Value::new_string(second),
						&mut local, &global).nillable();
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
													&*metamethod, arguments, global.clone())?);
												true
											},
											Value::NativeFunction(metamethod) => {
												transfer_result = Some(metamethod(Table::array(
													[&first, &second]).arc(), global.clone())?);
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
													&*metamethod, arguments, global.clone())?);
												true
											},
											Value::NativeFunction(metamethod) => {
												transfer_result = Some(metamethod(Table::array(
													[&first, &second]).arc(), global.clone())?);
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

					local.insert(destination, result);*/

					todo!()
				},

				OpCode::UnaryOperation {..} => {
					todo!()
				},

				OpCode::Jump {operation, r#if: None} => {
					current_opcode = operation as usize;
					continue;
				},

				OpCode::Jump {operation, r#if: Some(r#if)} => {
					let r#if = self.read_reference(Reference::Local(r#if));
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
					let value = self.read_reference(Reference::Global(global));
					self.write_reference(Reference::Local(register), value);
				},

				OpCode::SaveGlobal {register, global} => {
					let value = self.read_reference(Reference::Local(register));
					self.write_reference(Reference::Global(global), value);
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
		first: &'s str,
		second: &'s str,
		operation: BinaryOperation,
		destination: &'s str,
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
			Self::BinaryOperation {..} =>
				write!(f, "biop"),
			Self::UnaryOperation {..} =>
				write!(f, "unop"),
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
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Add,
	Subtract
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOperation {
	Not
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
