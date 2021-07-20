use self::super::{
	ast::parser::{BinaryOperator, Block, Expression, KeyValue, Statement},
	vm::{constant::{Constant, KnownValue}, Chunk, BinaryOperation, OpCode, UnaryOperation}
};
use std::{collections::HashMap, convert::TryInto};

pub fn compile(block: &Block) -> Chunk {
	let mut compiler = Generator::new();
	compiler.compile(block);
	compiler.finish()
}

pub fn compile_function(block: &Block, arguments: &Vec<String>,
		up_values: HashMap<String, (usize, bool)>) -> Chunk {
	let mut compiler = Generator {up_values, ..Generator::new()};
	compiler.compile_function_header(arguments);
	compiler.compile(&block);
	compiler.finish()
}

#[derive(Debug)]
enum CompileResult {
	Evaluated(KnownValue),
	Register(usize)
}

impl CompileResult {
	fn register(self, compiler: &mut Generator) -> usize {
		match self {
			Self::Evaluated(known) => compiler.compile_known(known),
			Self::Register(register) => register
		}
	}
}

#[derive(Debug)]
struct Generator {
	/// The constant pool.
	///
	/// The constant pool is a staic list of primitive values that can be loaded
	/// during runtime. During compile time, this list is typically accessed
	/// indirectly via the [CompileResult] and [KnownValue] types.
	constants: Vec<Constant>,
	opcodes: Vec<OpCode<'static>>,

	up_values: HashMap<String, (usize, bool)>,

	// TODO: currently only written to
	registers: Vec<Option<KnownValue>>,
	evaluated_variables: HashMap<String, KnownValue>,
	variables_to_registers: HashMap<String, usize>
}

impl Generator {
	fn new() -> Self {
		Self::default()
	}

	fn opcode(&mut self, opcode: OpCode<'static>) {
		self.opcodes.push(opcode);
	}

	fn register(&mut self) -> usize {
		self.registers.push(None);
		self.registers.len() - 1
	}

	fn compile_known(&mut self, value: impl Into<KnownValue>) -> usize {
		let value = value.into();

		if let Some(register) = self.registers.iter()
				.position(|known| known.as_ref().map(|known| known == &value)
					.unwrap_or(false))
			{return register}

		let register = self.register();
		self.registers[register] = Some(value.clone());
		let value = match value {
			KnownValue::Nil => {
				self.opcode(OpCode::LoadConst {constant: u16::MAX, register});
				return register
			},
			KnownValue::Integer(value) => Constant::Integer(value),
			KnownValue::String(value) => Constant::String(value),
			KnownValue::Boolean(value) => Constant::Boolean(value)
		};

		let constant = self.constants.iter()
			.position(|constant| constant == &value)
			.unwrap_or_else(|| {
				self.constants.push(value);
				self.constants.len() - 1
			}) as u16;
		self.opcode(OpCode::LoadConst {constant, register});
		return register
	}

	fn finish(self) -> Chunk {
		let Self {constants, opcodes, registers, up_values, ..} = self;

		let registers = registers.len() + 1; // 0th register is always arguments.

		let up_values =
			(0..up_values.values().filter_map(|&(a, b)| (!b).then(|| a))
				.max().map(|v| v + 1).unwrap_or(0)).map(|a| (a, false))
			.chain((0..up_values.values().filter_map(|&(a, b)| b.then(|| a))
				.max().map(|v| v + 1).unwrap_or(0)).map(|a| (a, true)))
			.collect::<Vec<_>>();

		Chunk {constants, opcodes, registers, up_values}
	}

	fn up_value_id(&self, (up_value, use_up_value): (usize, bool)) -> usize {
		match use_up_value {
			true => up_value + self.up_values.values()
				.filter_map(|(up_value, use_up_value)|
					(!use_up_value).then(|| *up_value))
				.max().unwrap_or(0),
			false => up_value
		}
	}

	fn compile(&mut self, block: &Block) {
		let Block(block) = block;
		let mut current_statement = 0;

		loop {
			if current_statement >= block.len() {return}

			match &block[current_statement] {
				// Control

				// TODO: else_ifs
				Statement::If {condition, then, r#else, ..} =>
						match self.compile_expression(condition) {
					// We know what condition is now.
					CompileResult::Evaluated(result) => {
						if result.coerce_to_bool() {
							// If the condition is known to be true, we can skip compiling
							// any other block except then.
							self.compile(then);
						} else if let Some(r#else) = r#else {
							// Or, if it's known to be false, we can do the same thing but
							// with r#else...
							self.compile(r#else);
						}
					},
					// We don't know what condition is.
					CompileResult::Register(variable) => match r#else {
						Some(r#else) => {
							// Jump If local To 'then
							// Block r#else
							// Jump To 'done
							// 'then
							// Block then
							// 'done

							let jump_then = self.opcodes.len();
							self.opcode(OpCode::NoOp);
							self.compile(r#else);
							let jump_done = self.opcodes.len();
							self.opcode(OpCode::NoOp);

							self.opcodes[jump_then] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
							// TODO: Everytime we add a jump opcode we have to add this or
							// else internal state gets funky. Need a better long term
							// solution..
							self.registers.iter_mut().for_each(|value| *value = None);
							self.compile(then);
							self.opcodes[jump_done] = OpCode::Jump {
								r#if: None,
								operation: self.opcodes.len() as u64
							};
							self.registers.iter_mut().for_each(|value| *value = None);
						},
						None => {
							// UnaryOperation not local To local
							// Jump If local To 'skip
							// Block then
							// 'skip

							self.opcode(OpCode::UnaryOperation {
								operand: variable,
								operation: UnaryOperation::LogicalNot,
								destination: variable
							});
							let jump = self.opcodes.len();
							self.opcode(OpCode::NoOp);
							self.compile(then);

							self.registers.iter_mut().for_each(|value| *value = None);
							self.opcodes[jump] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
						}
					}
				},

				Statement::GenericFor {variable, iterator, r#do} => {
					let top = self.opcodes.len() as u64;
					let function = self.compile_expression(iterator).register(self);
					let arguments = self.register();

					self.opcode(OpCode::Create {destination: arguments});
					//let index = self.compile_known(2 as i64);
					//self.opcode(OpCode::IndexWrite {indexee: arguments, index, value})

					let destination = self.register();
					let index = self.compile_known(1 as i64);
					self.opcode(OpCode::Call {function, arguments, destination});
					self.opcode(OpCode::IndexRead {indexee: destination,
						index, destination});
					let condition = self.register();
					let right = self.compile_known(KnownValue::Nil);
					self.opcode(OpCode::BinaryOperation {left: destination, right,
						operation: BinaryOperation::Equal, destination: condition});
					let done = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					self.variables_to_registers.insert(variable.clone(), destination);
					self.compile(r#do);
					self.opcode(OpCode::Jump {operation: top, r#if: None});

					self.opcodes[done] = OpCode::Jump {
						operation: self.opcodes.len() as u64, r#if: Some(condition)};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::NumericFor {variable, first, step, limit, r#do} => {
					let value = self.compile_expression(first).register(self);
					let limit = self.compile_expression(limit).register(self);
					let step = self.compile_expression(step).register(self);
					let condition = self.register();

					let value = {
						let new = self.register();
						self.opcode(OpCode::ReAssign {actor: value, destination: new});
						new
					};

					let operation = self.opcodes.len() as u64;
					self.opcode(OpCode::BinaryOperation {left: value, right: limit,
						operation: BinaryOperation::Equal, destination: condition});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);
					self.registers.iter_mut().for_each(|value| *value = None);

					self.variables_to_registers.insert(variable.clone(), value);
					self.compile(r#do);
					self.opcode(OpCode::BinaryOperation {left: value, right: step,
						operation: BinaryOperation::Add, destination: value});
					self.opcode(OpCode::Jump {operation, r#if: None});

					self.opcodes[jump] = OpCode::Jump {r#if: Some(condition),
						operation: self.opcodes.len() as u64};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::While {block, condition, run_first: false} => {
					let operation = self.opcodes.len() as u64;
					let operand = self.compile_expression(condition).register(self);
					self.opcode(OpCode::UnaryOperation {operand, destination: operand,
						operation: UnaryOperation::LogicalNot});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);
					
					self.compile(block);
					self.opcode(OpCode::Jump {operation, r#if: None});
					self.opcodes[jump] = OpCode::Jump {operation: self.opcodes.len() as u64, r#if: Some(operand)};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::While {block, condition, run_first: true} => {
					let operation = self.opcodes.len() as u64;
					self.compile(block);

					let operand = self.compile_expression(condition).register(self);
					self.opcode(OpCode::UnaryOperation {operand, destination: operand,
						operation: UnaryOperation::LogicalNot});
					self.opcode(OpCode::Jump {operation, r#if: Some(operand)});
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::Return {values} => {
					let destination = self.register();
					self.opcode(OpCode::Create {destination});
					values.iter().enumerate().for_each(|(index, value)| {
						let index = self.compile_known(index as i64 + 1);
						let variable = self.compile_expression(value).register(self);

						self.opcode(OpCode::IndexWrite {indexee: destination, index, value: variable});
					});

					self.opcode(OpCode::Return {result: destination});
				},

				// Assign

				Statement::Assign {actor, value, local} => match local {
					true => match actor {
						Expression::Identifier(identifier) =>
								match self.compile_expression(value) {
							CompileResult::Evaluated(value) => {
								// We don't have to assign this variable immediately, because
								// it's known.
								self.evaluated_variables.insert(identifier.clone(), value);
							},
							CompileResult::Register(actor) => {
								// identifier already has it's own register. We don't want to
								// be able to change it's data if we write to the new variable,
								// so we make a new register, and reassign.
								self.evaluated_variables.remove(identifier);
								let destination = self.register();
								self.opcode(OpCode::ReAssign {actor, destination});
								self.variables_to_registers.insert(identifier.clone(), destination);
							}
						},
						_ => panic!() // TODO: Can't happen? Shouldn't happen.
					},
					false => match actor {
						Expression::Identifier(identifier) => match (
							self.compile_expression(value),
							self.evaluated_variables.get(identifier),
							self.variables_to_registers.get(identifier),
							self.up_values.get(identifier)
						) {
							(CompileResult::Evaluated(value), Some(_), _, _) =>
								{self.evaluated_variables.insert(identifier.clone(), value);},
							(CompileResult::Register(register), Some(_), _, _) => {
								self.evaluated_variables.remove(identifier);
								self.variables_to_registers.insert(identifier.clone(), register);
							},
							(CompileResult::Evaluated(value), None, Some(&register), _) => {
								// TODO: We could probably add destination to compile_known?
								let old = self.compile_known(value.clone());
								self.opcode(OpCode::ReAssign {actor: old, destination: register});
								self.registers[register] = Some(value);
							},
							(CompileResult::Register(old), None, Some(&new), _) => {
								// TODO: This is what destination in compile_expression was for.
								self.opcode(OpCode::ReAssign {actor: old, destination: new});
							},
							(CompileResult::Evaluated(value),
									None, None, Some(&up_value)) => {
								let register = self.compile_known(value);
								self.opcode(OpCode::SaveUpValue {register, up_value: self.up_value_id(up_value)});
							},
							(CompileResult::Register(register),
									None, None, Some(&up_value)) =>
								self.opcode(OpCode::SaveUpValue {register, up_value: self.up_value_id(up_value)}),
							(CompileResult::Evaluated(value), None, None, None) => {
								let global = Box::leak(identifier.clone().into_boxed_str());
								let register = self.compile_known(value);
								self.opcode(OpCode::SaveGlobal {register, global});
							},
							(CompileResult::Register(register), None, None, None) => {
								let global = Box::leak(identifier.clone().into_boxed_str());
								self.opcode(OpCode::SaveGlobal {register, global});
							}
						},
						Expression::Index {indexee, index} =>
								match self.compile_expression(value) {
							CompileResult::Evaluated(value) => {
								let indexee = self.compile_expression(indexee).register(self);
								let index = self.compile_expression(index).register(self);
								let register = self.compile_known(value);
								self.opcode(OpCode::IndexWrite {indexee, index, value: register});
							},
							CompileResult::Register(value) => {
								let indexee = self.compile_expression(indexee).register(self);
								let index = self.compile_expression(index).register(self);

								self.opcode(OpCode::IndexWrite {indexee, index, value});
							}
						},
						_ => panic!() // TODO: Can't happen? Shouldn't happen.
					}
				},

				// Expressions

				Statement::Call {function, arguments} =>
					{self.compile_call(function, &arguments);},

				Statement::Function {name, arguments, body, local} => {
					// We need to realize all values...
					let evaluated = self.evaluated_variables.iter()
						.map(|(name, value)| (name.clone(), value.clone()))
						.collect::<Vec<_>>();
					evaluated.into_iter()
						.for_each(|(name, value)| {
							let register = self.compile_known(value);
							self.variables_to_registers.insert(name, register);
						});

					let register = self.register();
					if *local {
						self.variables_to_registers.insert(name.clone(), register);
					}
					let up_values = self.variables_to_registers.iter()
						.map(|(key, &value)| (key.clone(), (value, false)))
						.chain(self.up_values.iter()
							.map(|(key, &(value, _))| (key.clone(), (value, true))))
						.collect();

					let function = compile_function(body, arguments, up_values);
					self.constants.push(Constant::Chunk(function.arc()));
					let constant = self.constants.len() as u16 - 1;
					self.opcode(OpCode::LoadConst {constant, register});
					
					if !*local {
						self.opcode(OpCode::SaveGlobal {register,
							global: Box::leak(name.clone().into_boxed_str())});
					}
				}
			}

			current_statement = current_statement + 1;
		}
	}

	fn compile_expression(&mut self, expression: &Expression) -> CompileResult {
		match expression {
			// Identifier

			Expression::Identifier(identifier) =>
					match self.evaluated_variables.get(identifier) {
				// If we evaluated a value for identifier, return it.
				Some(known) => CompileResult::Evaluated(known.clone()),
				// Otherwise, check the local scope.
				None => match self.variables_to_registers.get(identifier) {
					// If it's in local scope, return it's register.
					Some(&register) => CompileResult::Register(register),
					// Otherwise, check up values.
					None => match self.up_values.get(identifier) {
						Some(&up_value) => {
							let register = self.register();
							self.opcode(OpCode::LoadUpValue {up_value: self.up_value_id(up_value), register});
							CompileResult::Register(register)
						},
						// Otherwise, load from global scope.
						None => {
							let register = self.register();
							self.opcode(OpCode::LoadGlobal {global: Box::leak(
								identifier.clone().into_boxed_str()), register});
							CompileResult::Register(register)
						}
					}
				}
			},

			// Singleton literals

			// These all just return evaluated.
			Expression::Nil => CompileResult::Evaluated(KnownValue::Nil),
			Expression::True => CompileResult::Evaluated(KnownValue::Boolean(true)),
			Expression::False => CompileResult::Evaluated(KnownValue::Boolean(false)),

			// Literals

			// Same with these.
			Expression::Integer(integer) =>
				CompileResult::Evaluated(KnownValue::Integer(*integer)),
			Expression::String(string) =>
				CompileResult::Evaluated(KnownValue::String(string.clone())),

			// Complex literals

			Expression::Table {array, key_value} => {
				let register = self.register();
				self.opcode(OpCode::Create {destination: register});

				key_value.iter().for_each(|KeyValue {key, value}| {
					let key = self.compile_expression(key).register(self);
					let value = self.compile_expression(value).register(self);

					self.opcode(OpCode::IndexWrite {index: key, value: value, indexee: register});
				});
				array.iter().enumerate().for_each(|(index, value)| {
					let value = self.compile_expression(value).register(self);
					let temporary = self.compile_known(index as i64 + 1);
					self.opcode(OpCode::IndexWrite {index: temporary, value, indexee: register});
				});

				CompileResult::Register(register)
			},

			Expression::Function {arguments, body} => {
				// We need to realize all values...
				let evaluated = self.evaluated_variables.iter()
					.map(|(name, value)| (name.clone(), value.clone()))
					.collect::<Vec<_>>();
				evaluated.into_iter()
					.for_each(|(name, value)| {
						let register = self.compile_known(value);
						self.variables_to_registers.insert(name, register);
					});

				let up_values = self.variables_to_registers.iter()
					.map(|(key, &value)| (key.clone(), (value, false)))
					.chain(self.up_values.iter()
						.map(|(key, &(value, _))| (key.clone(), (value, true))))
					.collect();
				let function = compile_function(body, arguments, up_values);
				self.constants.push(Constant::Chunk(function.arc()));
				let constant = self.constants.len() as u16 - 1;
				let destination = self.register();

				self.opcode(OpCode::LoadConst {constant, register: destination});
				CompileResult::Register(destination)
			},

			// Operators

			Expression::Call {function, arguments} =>
				CompileResult::Register(self.compile_call(function, arguments)),

			Expression::Index {indexee, index} => {
				let destination = self.register();
				let indexee = self.compile_expression(indexee).register(self);
				let index = self.compile_expression(index).register(self);
				self.opcode(OpCode::IndexRead {indexee, index, destination});
				CompileResult::Register(destination)
			},

			Expression::BinaryOperation {left, right,
					operator: BinaryOperator::LogicalAnd} =>
						match self.compile_expression(left) {
				CompileResult::Evaluated(left) =>
					if !left.coerce_to_bool() {CompileResult::Evaluated(left)}
					else {self.compile_expression(right)},
				CompileResult::Register(left) => {
					// unop not {left} {boolean}
					// cjmp {done} {boolean}
					// <right compiled to {right}>
					// reas {right} {left}
					// <location of done>

					let boolean = self.register();
					self.opcode(OpCode::UnaryOperation {operand: left,
						operation: UnaryOperation::LogicalNot, destination: boolean});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					let right = self.compile_expression(right).register(self);
					self.opcode(OpCode::ReAssign {actor: right, destination: left});
					let operation = self.opcodes.len() as u64;
					self.opcodes[jump] = OpCode::Jump {operation, r#if: Some(boolean)};
					self.registers.iter_mut().for_each(|value| *value = None);

					CompileResult::Register(left)
				}
			},

			Expression::BinaryOperation {left, right,
					operator: BinaryOperator::LogicalOr} =>
						match self.compile_expression(left) {
				CompileResult::Evaluated(left) =>
					if left.coerce_to_bool() {CompileResult::Evaluated(left)}
					else {self.compile_expression(right)},
				CompileResult::Register(left) => {
					// cjmp {done} {left}
					// <right compiled to {right}>
					// reas {right} {left}
					// <location of done>

					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					let right = self.compile_expression(right).register(self);
					self.opcode(OpCode::ReAssign {actor: right, destination: left});
					let operation = self.opcodes.len() as u64;
					self.opcodes[jump] = OpCode::Jump {operation, r#if: Some(left)};
					self.registers.iter_mut().for_each(|value| *value = None);

					CompileResult::Register(left)
				}
			},

			Expression::BinaryOperation {left, operator, right} => {
				let left = self.compile_expression(left).register(self);
				let right = self.compile_expression(right).register(self);
				let destination = self.register();
				self.opcode(OpCode::BinaryOperation {left, right, destination,
					operation: (*operator).try_into().unwrap()});
				CompileResult::Register(destination)
			},

			Expression::UnaryOperation {operator, operand} => {
				let operand = self.compile_expression(operand).register(self);
				let destination = self.register();
				self.opcode(OpCode::UnaryOperation {operand, destination,
					operation: (*operator).into()});
				CompileResult::Register(destination)
			}
		}
	}

	fn compile_call(&mut self, function: &Expression,
			arguments: &Vec<Expression>) -> usize {
		let function = self.compile_expression(function);

		let arguments_register = self.register();
		self.opcode(OpCode::Create {destination: arguments_register});

		arguments.iter().enumerate().for_each(|(index, argument)| {
			let value = self.compile_expression(argument).register(self);
			let index = self.compile_known(index as i64 + 1);
			self.opcode(OpCode::IndexWrite {indexee: arguments_register, index, value});
		});

		let indexee = self.register();
		let function = function.register(self);
		self.opcode(OpCode::Call {function,
			arguments: arguments_register, destination: indexee});

		// TODO: Tuples (tuples are actually a compile time construct)
		let destination = self.register();
		let index = self.compile_known(1i64);
		self.opcode(OpCode::IndexRead {index, indexee, destination});
		destination
	}

	fn compile_function_header(&mut self, arguments: &Vec<String>) {
		let indexee = 0; // Function arguments...
		arguments.iter().enumerate().for_each(|(index, argument)| {
			let index = self.compile_known(index as i64 + 1);
			let value = self.register();
			self.opcode(OpCode::IndexRead {index, indexee, destination: value});
			self.variables_to_registers.insert(argument.clone(), value);
		});
	}
}

impl Default for Generator {
	fn default() -> Self {
		Self {
			constants: Vec::default(),
			evaluated_variables: HashMap::default(),
			opcodes: Vec::default(),
			registers: vec![None],
			up_values: HashMap::default(),
			variables_to_registers: HashMap::default()
		}
	}
}
