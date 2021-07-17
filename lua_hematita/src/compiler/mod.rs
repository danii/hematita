use self::super::{
	ast::parser::{Block, Expression, KeyValue, Statement},
	vm::{constant::{Constant, KnownValue}, Chunk, OpCode, UnaryOperation}
};
use std::collections::HashMap;

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
	Evaluated(Option<KnownValue>),
	WroteToRegister(usize)
}

impl CompileResult {
	fn to(self, compiler: &mut Generator) -> usize {
		match self {
			Self::Evaluated(Some(evaluated)) => {
				let destination = compiler.register();
				let constant = compiler.constant(evaluated.into());
				compiler.opcodes.push(OpCode::LoadConst {constant, register: destination});
				destination
			},
			Self::Evaluated(None) => {
				let destination = compiler.register();
				compiler.opcodes.push(OpCode::LoadConst {
					constant: u16::MAX, register: destination});
				destination
			},
			Self::WroteToRegister(register) => register
		}
	}
}

#[derive(Debug)]
struct Generator {
	constants: Vec<Constant>,
	opcodes: Vec<OpCode<'static>>,

	up_values: HashMap<String, (usize, bool)>,

	// TODO: currently only written to
	registers: Vec<Option<Option<KnownValue>>>,
	evaluated_variables: HashMap<String, Option<KnownValue>>,
	variables_to_registers: HashMap<String, usize>
}

impl Generator {
	fn new() -> Self {
		Self::default()
	}

	fn register(&mut self) -> usize {
		self.registers.push(None);
		self.registers.len() - 1
	}

	fn constant(&mut self, value: Constant) -> u16 {
		self.constants.iter()
			.position(|constant| constant == &value)
			.unwrap_or_else(|| {
				self.constants.push(value);
				self.constants.len() - 1
			}) as u16
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
						if result.map(KnownValue::coerce_to_bool).unwrap_or(false) {
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
					CompileResult::WroteToRegister(variable) => match r#else {
						Some(r#else) => {
							// Jump If local To 'then
							// Block r#else
							// Jump To 'done
							// 'then
							// Block then
							// 'done

							let jump_then = self.opcodes.len();
							self.opcodes.push(OpCode::NoOp);
							self.compile(r#else);
							let jump_done = self.opcodes.len();
							self.opcodes.push(OpCode::NoOp);

							self.opcodes[jump_then] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
							self.compile(then);
							self.opcodes[jump_done] = OpCode::Jump {
								r#if: None,
								operation: self.opcodes.len() as u64
							};
						},
						None => {
							// UnaryOperation not local To local
							// Jump If local To 'skip
							// Block then
							// 'skip

							self.opcodes.push(OpCode::UnaryOperation {
								operand: variable,
								operation: UnaryOperation::Not,
								destination: variable
							});
							let jump = self.opcodes.len();
							self.opcodes.push(OpCode::NoOp);
							self.compile(then);

							self.opcodes[jump] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
						}
					}
				},

				Statement::GenericFor {..} => todo!(),

				Statement::NumericFor {..} => todo!(),

				Statement::While {..} => todo!(),

				Statement::Return {values} => {
					let destination = self.register();
					self.opcodes.push(OpCode::Create {destination});
					values.iter().enumerate().for_each(|(index, value)| {
						let index = self.constant(Constant::Integer(index as i64 + 1));
						let index_register = self.register();
						let variable = self.compile_expression(value).to(self);

						self.opcodes.push(OpCode::LoadConst {
							constant: index, register: index_register});
						self.opcodes.push(OpCode::IndexWrite {
							indexee: destination, index: index_register,
							value: variable});
					});

					self.opcodes.push(OpCode::Return {result: destination});
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
							CompileResult::WroteToRegister(actor) => {
								// identifier already has it's own register. We don't want to
								// be able to change it's data if we write to the new variable,
								// so we make a new register, and reassign.
								self.evaluated_variables.remove(identifier);
								let destination = self.register();
								self.opcodes.push(OpCode::ReAssign {actor, destination});
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
							(CompileResult::WroteToRegister(register), Some(_), _, _) => {
								self.evaluated_variables.remove(identifier);
								self.variables_to_registers.insert(identifier.clone(), register);
							},
							(CompileResult::Evaluated(value), None, Some(&register), _) => {
								let constant = match value {
									Some(ref value) => self.constant(value.clone().into()),
									None => u16::MAX
								};

								self.opcodes.push(OpCode::LoadConst {constant, register});
								self.registers[register] = Some(value);
							},
							(CompileResult::WroteToRegister(old), None, Some(&new), _) => {
								// TODO: This is what destination in compile_expression was for.
								self.opcodes.push(OpCode::ReAssign {actor: old, destination: new});
							},
							(CompileResult::Evaluated(value),
									None, None, Some(&up_value)) => {
								let register = self.register();
								let constant = match value {
									Some(value) => self.constant(value.into()),
									None => u16::MAX
								};

								self.opcodes.push(OpCode::LoadConst {constant, register});
								self.opcodes.push(OpCode::SaveUpValue {register, up_value: self.up_value_id(up_value)});
							},
							(CompileResult::WroteToRegister(register),
									None, None, Some(&up_value)) =>
								self.opcodes.push(OpCode::SaveUpValue {register, up_value: self.up_value_id(up_value)}),
							(CompileResult::Evaluated(value), None, None, None) => {
								let global = Box::leak(identifier.clone().into_boxed_str());
								let register = self.register();
								let constant = match value {
									Some(value) => self.constant(value.into()),
									None => u16::MAX
								};

								self.opcodes.push(OpCode::LoadConst {constant, register});
								self.opcodes.push(OpCode::SaveGlobal {register, global});
							},
							(CompileResult::WroteToRegister(register), None, None, None) => {
								let global = Box::leak(identifier.clone().into_boxed_str());
								self.opcodes.push(OpCode::SaveGlobal {register, global});
							}
						},
						Expression::Index {indexee, index} =>
								match self.compile_expression(value) {
							CompileResult::Evaluated(value) => {
								let indexee = self.compile_expression(indexee).to(self);
								let index = self.compile_expression(index).to(self);

								let register = self.register();
								let constant = match value {
									Some(value) => self.constant(value.into()),
									None => u16::MAX
								};

								self.opcodes.push(OpCode::LoadConst {constant, register});
								self.opcodes.push(OpCode::IndexWrite {indexee, index, value: register});
							},
							CompileResult::WroteToRegister(value) => {
								let indexee = self.compile_expression(indexee).to(self);
								let index = self.compile_expression(index).to(self);

								self.opcodes.push(OpCode::IndexWrite {indexee, index, value});
							}
						},
						_ => panic!() // TODO: Can't happen? Shouldn't happen.
					}
				},

				// Expressions

				Statement::Call {function, arguments} =>
					{self.compile_call(function, &arguments);},

				Statement::Function {name, arguments, body, local} => {
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
					let constant = self.constant(Constant::Chunk(function.arc()));
					
					self.opcodes.push(OpCode::LoadConst {
						constant, register});
					if !*local {
						self.opcodes.push(OpCode::SaveGlobal {register,
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
					Some(&register) => CompileResult::WroteToRegister(register),
					// Otherwise, check up values.
					None => match self.up_values.get(identifier) {
						Some(&up_value) => {
							let register = self.register();
							self.opcodes.push(OpCode::LoadUpValue {up_value: self.up_value_id(up_value), register});
							CompileResult::WroteToRegister(register)
						},
						// Otherwise, load from global scope.
						None => {
							let register = self.register();
							self.opcodes.push(OpCode::LoadGlobal {global: Box::leak(
								identifier.clone().into_boxed_str()), register});
							CompileResult::WroteToRegister(register)
						}
					}
				}
			},

			// Singleton literals

			// These all just return evaluated.
			Expression::Nil => CompileResult::Evaluated(None),
			Expression::True =>
				CompileResult::Evaluated(Some(KnownValue::Boolean(true))),
			Expression::False =>
				CompileResult::Evaluated(Some(KnownValue::Boolean(false))),

			// Literals

			// Same with these.
			Expression::Integer(integer) =>
				CompileResult::Evaluated(Some(KnownValue::Integer(*integer))),
			Expression::String(string) =>
				CompileResult::Evaluated(Some(KnownValue::String(string.clone()))),

			// Complex literals

			Expression::Table {array, key_value} => {
				let register = self.register();
				self.opcodes.push(OpCode::Create {destination: register});

				key_value.iter().for_each(|KeyValue {key, value}| {
					let key = self.compile_expression(key).to(self);
					let value = self.compile_expression(value).to(self);

					self.opcodes.push(OpCode::IndexWrite {index: key, value: value, indexee: register});
				});
				array.iter().enumerate().for_each(|(index, value)| {
					let value = self.compile_expression(value).to(self);
					let index = self.constant(Constant::Integer(index as i64 + 1));
					let temporary = self.register();

					self.opcodes.push(OpCode::LoadConst {constant: index, register: temporary});
					self.opcodes.push(OpCode::IndexWrite {index: temporary, value, indexee: register});
				});

				CompileResult::WroteToRegister(register)
			},

			Expression::Function {arguments, body} => {
				// We need to realize all values...

				let up_values = self.variables_to_registers.iter()
					.map(|(key, &value)| (key.clone(), (value, false)))
					.chain(self.up_values.iter()
						.map(|(key, &(value, _))| (key.clone(), (value, true))))
					.collect();
				let function = compile_function(body, arguments, up_values);
				let constant = self.constant(Constant::Chunk(function.arc()));
				let destination = self.register();

				self.opcodes.push(OpCode::LoadConst {constant, register: destination});
				CompileResult::WroteToRegister(destination)
			},

			// Operators

			Expression::Call {function, arguments} =>
				CompileResult::WroteToRegister(self.compile_call(function, arguments)),

			Expression::Index {indexee, index} => {
				let destination = self.register();
				let indexee = self.compile_expression(indexee).to(self);
				let index = self.compile_expression(index).to(self);
				self.opcodes.push(OpCode::IndexRead {indexee, index, destination});
				CompileResult::WroteToRegister(destination)
			},

			Expression::BinaryOperation {..} => todo!(),

			Expression::UnaryOperation {..} => todo!()
		}
	}

	fn compile_call(&mut self, function: &Expression,
			arguments: &Vec<Expression>) -> usize {
		let function = self.compile_expression(function);

		let arguments_register = self.register();
		self.opcodes.push(OpCode::Create {destination: arguments_register});

		arguments.iter().enumerate().for_each(|(index, argument)|
				match self.compile_expression(argument) {
			CompileResult::Evaluated(Some(argument)) => {
				let argument = self.constant(argument.into());
				let argument_register = self.register();
				let index = self.constant(Constant::Integer(index as i64 + 1));
				let index_register = self.register();

				self.opcodes.push(OpCode::LoadConst {
					constant: argument, register: argument_register});
				self.opcodes.push(OpCode::LoadConst {
					constant: index, register: index_register});
				self.opcodes.push(OpCode::IndexWrite {
					indexee: arguments_register, index: index_register,
					value: argument_register});
			},
			CompileResult::Evaluated(None) => (),
			CompileResult::WroteToRegister(argument) => {
				let index = self.constant(Constant::Integer(index as i64 + 1));
				let index_register = self.register();
				
				self.opcodes.push(OpCode::LoadConst {
					constant: index, register: index_register});
				self.opcodes.push(OpCode::IndexWrite {
					indexee: arguments_register, index: index_register, value: argument});
			}
		});

		let destination = self.register();
		match function {
			CompileResult::Evaluated(argument) => {
				let constant_register = self.register();
				let constant = match argument {
					Some(argument) => self.constant(argument.into()),
					None => u16::MAX
				};

				self.opcodes.push(OpCode::LoadConst {
					constant, register: constant_register});
				self.opcodes.push(OpCode::Call {function: constant_register,
					arguments: arguments_register, destination});
			},
			CompileResult::WroteToRegister(function) => {
				self.opcodes.push(OpCode::Call {function,
					arguments: arguments_register, destination});
			}
		}

		destination
	}

	fn compile_function_header(&mut self, arguments: &Vec<String>) {
		let indexee = 0; // Function arguments...
		arguments.iter().enumerate().for_each(|(index, argument)| {
			let constant = self.constant(Constant::Integer(index as i64 + 1));
			let index = self.register();
			let value = self.register();

			self.opcodes.push(OpCode::LoadConst {constant, register: index});
			self.opcodes.push(OpCode::IndexRead {index, indexee, destination: value});
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
