use self::super::{
	ast::parser::{Block, Expression, KeyValue, Statement},
	vm::{constant::{Constant, KnownValue}, Chunk, OpCode, UnaryOperation}
};
use std::collections::{HashMap, HashSet};

pub fn compile(block: &Block) -> Chunk {
	let mut compiler = Generator::new();
	compiler.compile(block);
	compiler.finish()
}

pub fn compile_function(block: &Block, arguments: &Vec<String>) -> Chunk {
	let mut compiler = Generator::new();
	compiler.compile_function_header(arguments);
	compiler.compile(&block);
	compiler.finish()
}

#[derive(Debug)]
enum CompileResult {
	Evaluated(Option<KnownValue>),
	WroteToVariable(String),
	WroteToTemporary(Temporary)
}

enum Name {
	Identifier(&'static str),
	Temporary(Temporary)
}

impl Name {
	fn free(self, compiler: &mut Generator) {
		if let Self::Temporary(temporary) = self {temporary.free(compiler)}
	}
}

impl std::ops::Deref for Name {
	type Target = &'static str;

	fn deref(&self) -> &&'static str {
		match self {
			Self::Identifier(id) => &id,
			Self::Temporary(tmp) => &**tmp
		}
	}
}

impl CompileResult {
	fn to(self, compiler: &mut Generator) -> Name {
		match self {
			Self::Evaluated(Some(evaluated)) => {
				let temporary = compiler.temporary();
				let constant = compiler.constant(evaluated.into());
				compiler.opcodes.push(OpCode::Load {constant,
					destination: *temporary, destination_local: true});

				Name::Temporary(temporary)
			},
			Self::Evaluated(None) => {
				let temporary = compiler.temporary();
				compiler.opcodes.push(OpCode::Load {constant: u16::MAX,
					destination: *temporary, destination_local: true});

				Name::Temporary(temporary)
			},
			Self::WroteToTemporary(temporary) =>
				Name::Temporary(temporary),
			Self::WroteToVariable(variable) =>
				Name::Identifier(Box::leak(variable.into_boxed_str()))
		}
	}
}

#[derive(Debug)]
struct Temporary(&'static str, usize);

impl Temporary {
	fn free(self, generator: &mut Generator) {
		generator.temporaries.remove(&self.1);
	}
}

impl std::ops::Deref for Temporary {
	type Target = &'static str;

	fn deref(&self) -> &&'static str {
		&self.0
	}
}

#[derive(Default)]
struct Generator {
	constants: Vec<Constant>,
	opcodes: Vec<OpCode<'static>>,
	temporaries: HashSet<usize>,
	known_values: HashMap<KnownValue, Option<KnownValue>>
}

impl Generator {
	fn new() -> Self {
		Self::default()
	}

	fn temporary(&mut self) -> Temporary {
		let id = (0..)
			.find(|temporary| matches!(self.temporaries.get(temporary), None))
			.unwrap();
		self.temporaries.insert(id);

		Temporary(Box::leak(format!("(t{}", id).into_boxed_str()), id)
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
		let Self {constants, opcodes, ..} = self;
		Chunk {constants, opcodes}
	}

	fn compile(&mut self, block: &Block) {
		let Block(block) = block;
		let mut current_statement = 0;

		loop {
			if current_statement >= block.len() {return}

			match &block[current_statement] {
				// Control

				// TODO: else_ifs
				Statement::If {condition, then, r#else, ..} => {
					// TODO: Kinda clunky?..
					let handle_wrote = |this: &mut Generator, variable| {
						match r#else {
							Some(r#else) => {
								// Jump If local To 'then
								// Block r#else
								// Jump To 'done
								// 'then
								// Block then
								// 'done

								let jump_then = this.opcodes.len();
								this.opcodes.push(OpCode::NoOp);
								this.compile(r#else);
								let jump_done = this.opcodes.len();
								this.opcodes.push(OpCode::NoOp);

								this.opcodes[jump_then] = OpCode::Jump {
									r#if: Some(variable),
									operation: this.opcodes.len() as u64
								};
								this.compile(then);
								this.opcodes[jump_done] = OpCode::Jump {
									r#if: None,
									operation: this.opcodes.len() as u64
								};
							},
							None => {
								// UnaryOperation not local To local
								// Jump If local To 'skip
								// Block then
								// 'skip

								this.opcodes.push(OpCode::UnaryOperation {
									operand: variable,
									operation: UnaryOperation::Not,
									destination: variable,
									local: false
								});
								let jump = this.opcodes.len();
								this.opcodes.push(OpCode::NoOp);
								this.compile(then);

								this.opcodes[jump] = OpCode::Jump {
									r#if: Some(variable),
									operation: this.opcodes.len() as u64
								};
							}
						}
					};

					match self.compile_expression(condition, None) {
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
						CompileResult::WroteToVariable(variable) =>
							handle_wrote(self, Box::leak(variable.into_boxed_str())),
						CompileResult::WroteToTemporary(variable) => {
							handle_wrote(self, *variable);
							variable.free(self)
						}
					}
				},

				Statement::Return {values} => {
					let destination = self.temporary();

					self.opcodes.push(OpCode::Create {
						destination: *destination, destination_local: true});
					values.iter().enumerate().for_each(|(index, value)| {
						let index = self.constant(Constant::Integer(index as i64 + 1));
						let index_temporary = self.temporary();
						let variable = self.compile_expression(value, None).to(self);

						self.opcodes.push(OpCode::Load {constant: index,
							destination: *index_temporary, destination_local: true});
						self.opcodes.push(OpCode::IndexWrite {indexee: *destination,
							index: *index_temporary, value: *variable});

						index_temporary.free(self);
						variable.free(self);
					});

					self.opcodes.push(OpCode::Return {result: *destination});
					destination.free(self);
				},

				// Assign

				Statement::Assign {actor, value, local} => match actor {
					Expression::Identifier(identifier) =>
							match self.compile_expression(value, Some(identifier)) {
						CompileResult::Evaluated(value) => {self.known_values.insert(
							KnownValue::String(identifier.clone()), value);},
						CompileResult::WroteToVariable(variable)
							if &variable == identifier => (),
						CompileResult::WroteToVariable(variable) => {
							let variable = self.constant(Constant::String(variable));
							let variable_temporary = self.temporary();
							let identifier = Box::leak(identifier.clone().into_boxed_str());

							self.opcodes.push(OpCode::Load {constant: variable,
								destination: *variable_temporary, destination_local: true});
							self.opcodes.push(OpCode::ReAssign {actor: *variable_temporary,
								destination: identifier, destination_local: *local});
						},
						a => todo!("{:?}", a)
					},
					_ => todo!()
				},

				// Expressions

				Statement::Call {function, arguments} => {
					let function = Box::new(function.clone());
					let arguments = arguments.clone();
					self.compile_expression(&Expression::Call {function, arguments}, Some(&"(_".to_owned()));
				},

				Statement::Function {name, arguments, body, local} => {
					println!("{}:'{:?}'", name, body);
					// TODO: Upvals
					let function = compile_function(body, arguments);
					let constant = self.constant(Constant::Chunk(function.arc()));

					self.opcodes.push(OpCode::Load {
						constant,
						destination: Box::leak(name.to_owned().into_boxed_str()),
						destination_local: *local
					})
				},

				a => todo!("{:?}", a)
			}

			current_statement = current_statement + 1;
		}
	}

	fn compile_expression(&mut self, expression: &Expression,
			destination: Option<&String>) -> CompileResult {
		match expression {
			// Identifier

			Expression::Identifier(identifier) =>
					match self.known_values.get(&KnownValue::String(identifier.clone())) {
				Some(known) => CompileResult::Evaluated(known.clone()),
				None => CompileResult::WroteToVariable(identifier.clone())
			},

			// Singleton literals

			Expression::Nil => CompileResult::Evaluated(None),
			Expression::True =>
				CompileResult::Evaluated(Some(KnownValue::Boolean(true))),
			Expression::False =>
				CompileResult::Evaluated(Some(KnownValue::Boolean(false))),

			// Literals

			Expression::Integer(integer) =>
				CompileResult::Evaluated(Some(KnownValue::Integer(*integer))),
			Expression::String(string) =>
				CompileResult::Evaluated(Some(KnownValue::String(string.clone()))),

			// Complex literals

			Expression::Table {array, key_value} => {
				let (destination, temporary) = destination
					.map(|value| (&*Box::leak(value.clone().into_boxed_str()), None))
					.unwrap_or_else(|| {
						let temporary = self.temporary();
						(*temporary, Some(temporary))
					});

				self.opcodes.push(OpCode::Create {destination,
					destination_local: temporary.is_some()});
				key_value.iter().for_each(|KeyValue {key, value}| {
					let key = self.compile_expression(key, None).to(self);
					let value = self.compile_expression(value, None).to(self);

					self.opcodes.push(OpCode::IndexWrite {indexee: destination,
						index: *key, value: *value});

					key.free(self);
					value.free(self);
				});
				array.iter().enumerate().for_each(|(index, value)| {
					let value = self.compile_expression(value, None).to(self);
					let index = self.constant(Constant::Integer(index as i64 + 1));
					let index_temporary = self.temporary();

					self.opcodes.push(OpCode::Load {constant: index,
						destination: *index_temporary, destination_local: true});
					self.opcodes.push(OpCode::IndexWrite {indexee: destination,
						index: *index_temporary, value: *value});

					value.free(self);
					index_temporary.free(self);
				});

				match temporary {
					Some(temporary) => CompileResult::WroteToTemporary(temporary),
					None => CompileResult::WroteToVariable(destination.to_owned())
				}
			},

			Expression::Function {arguments, body} => {
				// TODO: Upvals
				let function = compile_function(body, arguments);
				let constant = self.constant(Constant::Chunk(function.arc()));

				match destination {
					Some(destination_string) => {
						let destination = Box::leak(
							destination_string.clone().into_boxed_str());
						self.opcodes.push(OpCode::Load {constant, destination,
							destination_local: true});
						CompileResult::WroteToVariable(destination_string.clone())
					},
					None => {
						let destination = self.temporary();
						self.opcodes.push(OpCode::Load {constant, destination: *destination,
							destination_local: true});
						CompileResult::WroteToTemporary(destination)
					}
				}
			},

			// Operators

			Expression::Call {function, arguments} => {
				match destination {
					Some(destination) => {
						let static_dest = Box::leak(destination.clone().into_boxed_str());
						self.compile_call(function, arguments, static_dest);
						CompileResult::WroteToVariable(destination.clone())
					},
					None => {
						let temporary = self.temporary();
						self.compile_call(function, arguments, *temporary);
						CompileResult::WroteToTemporary(temporary)
					}
				}
			},

			a => todo!("{:?}", a)
		}
	}

	fn compile_call(&mut self, function: &Expression, arguments: &Vec<Expression>,
			destination: &'static str) {
		let arguments_table = self.temporary();

		self.opcodes.push(OpCode::Create {destination: *arguments_table,
			destination_local: true});
		let function = self.compile_expression(function, None);

		let handle_wrote = |this: &mut Generator, argument, index| {
			let index = this.constant(Constant::Integer(index as i64 + 1));
			let index_temporary = this.temporary();

			this.opcodes.push(OpCode::Load {constant: index,
				destination: *index_temporary, destination_local: true});
			this.opcodes.push(OpCode::IndexWrite {indexee: *arguments_table,
				index: *index_temporary, value: argument});
		};
		arguments.iter().enumerate().for_each(|(index, argument)|
				match self.compile_expression(argument, None) {
			CompileResult::Evaluated(Some(argument)) => {
				let argument = self.constant(argument.into());
				let argument_temporary = self.temporary();
				let index = self.constant(Constant::Integer(index as i64 + 1));
				let index_temporary = self.temporary();

				self.opcodes.push(OpCode::Load {constant: argument,
					destination: *argument_temporary, destination_local: true});
				self.opcodes.push(OpCode::Load {constant: index,
					destination: *index_temporary, destination_local: true});
				self.opcodes.push(OpCode::IndexWrite {indexee: *arguments_table,
					index: *index_temporary, value: *argument_temporary});

				argument_temporary.free(self);
				index_temporary.free(self);
			},
			CompileResult::Evaluated(None) => (),
			CompileResult::WroteToVariable(argument) =>
				handle_wrote(self, Box::leak(argument.into_boxed_str()), index),
			CompileResult::WroteToTemporary(argument) => {
				handle_wrote(self, *argument, index);
				argument.free(self)
			}
		});

		match function {
			CompileResult::Evaluated(Some(argument)) => {
				let constant = self.constant(argument.into());
				let constant_temporary = self.temporary();

				self.opcodes.push(OpCode::Load {constant,
					destination: *constant_temporary, destination_local: true});
				self.opcodes.push(OpCode::Call {function: *constant_temporary,
					arguments: *arguments_table, destination, destination_local: true});

				constant_temporary.free(self);
			},
			CompileResult::Evaluated(None) => panic!(),
			CompileResult::WroteToVariable(function) => {
				let function = Box::leak(function.into_boxed_str());
				self.opcodes.push(OpCode::Call {function,
					arguments: *arguments_table, destination, destination_local: true});
			},
			CompileResult::WroteToTemporary(function) => {
				self.opcodes.push(OpCode::Call {function: *function,
					arguments: *arguments_table, destination, destination_local: true});
				function.free(self);
			}
		}

		arguments_table.free(self);
	}

	fn compile_function_header(&mut self, arguments: &Vec<String>) {
		arguments.iter().enumerate().for_each(|(index, argument)| {
			let index = self.constant(Constant::Integer(index as i64 + 1));
			let index_temporary = self.temporary();
			let argument = Box::leak(argument.clone().into_boxed_str());

			self.opcodes.push(OpCode::Load {constant: index,
				destination: *index_temporary, destination_local: true});
			self.opcodes.push(OpCode::ReAssign {actor: *index_temporary,
				destination: argument, destination_local: true});

			index_temporary.free(self);
		});
	}
}

