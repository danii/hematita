use self::super::{super::ast::parser::{Block, Expression, Statement}, Value, OpCode, Function};
use std::collections::HashSet;

pub fn generate_bytecode(block: Block) -> Function {
	ByteCodeGenerator::new().generate_block(block)
}

struct ByteCodeGenerator {
	constants: Vec<Value>,
	opcodes: Vec<OpCode<'static>>,
	temporaries: HashSet<usize>
}

impl ByteCodeGenerator {
	fn new() -> Self {
		Self {
			constants: Vec::new(),
			opcodes: Vec::new(),
			temporaries: HashSet::new()
		}
	}

	fn constant(&mut self, constant: Value) -> u16 {
		match self.constants.iter()
				.position(|others| *others == constant) {
			Some(id) => id as u16,
			None => {
				self.constants.push(constant);
				self.constants.len() as u16 - 1
			}
		}
	}

	fn temporary(&mut self) -> (&'static str, usize) {
		let tmp = (0..).find(|tmp|
			matches!(self.temporaries.get(tmp), None)).unwrap();
		self.temporaries.insert(tmp);

		(self.temporary_name(tmp), tmp)
	}

	fn temporary_name(&self, tmp: usize) -> &'static str {
		// LEAK
		Box::leak(format!("(t{}", tmp).into_boxed_str())
	}

	fn temporary_free(&mut self, tmp: usize) {
		self.temporaries.remove(&tmp);
	}

	fn push(&mut self, opcode: OpCode<'static>) {
		self.opcodes.push(opcode);
	}

	fn generate_statement(&mut self, statement: Statement) {
		match statement {
			Statement::Call {function, arguments} => {
				let (function, tmp) = self.generate_expression(function, None);
				let function = function.unwrap_or_else(|| self.temporary_name(tmp));

				self.push(OpCode::Create {
					destination: "(f",
					destination_local: true
				});

				{
					let (index, index_tmp) = self.temporary();
					let (value, value_tmp) = self.temporary();

					let constant = self.constant(Value::Integer(0));
					self.push(OpCode::Load {
						constant,
						destination: index,
						destination_local: true
					});
					let constant = self.constant(Value::Integer(arguments.len() as i64));
					self.push(OpCode::Load {
						constant,
						destination: value,
						destination_local: true
					});
					self.push(OpCode::IndexWrite {
						indexee: "(f",
						index,
						value
					});

					self.temporary_free(index_tmp);
					self.temporary_free(value_tmp);
				}

				for (index, argument) in arguments.into_iter().enumerate() {
					let (argument, arg_tmp) = self.generate_expression(argument, None);
					let argument = argument.unwrap_or_else(|| self.temporary_name(arg_tmp));

					let constant = self.constant(Value::Integer(index as i64 + 1));
					let (index_dest, index_tmp) = self.temporary();
					self.push(OpCode::Load {
						constant,
						destination: index_dest,
						destination_local:  true
					});
					self.push(OpCode::IndexWrite {
						indexee: "(f",
						index: index_dest,
						value: argument
					});

					self.temporary_free(arg_tmp);
					self.temporary_free(index_tmp);
				}

				self.push(OpCode::Call {
					function,
					arguments: "(f",
					destination: "(_",
					destination_local: true
				});

				self.temporary_free(tmp);
			},
			Statement::Assign {actor, value, local} => match actor {
				Expression::Identifier(identifier) => {
					// LEAK
					let identifier = Box::leak(identifier.into_boxed_str());
					let (value, _) = self.generate_expression(value, Some(identifier));

					match value {
						Some(value) => self.push(OpCode::ReAssign {
							actor: value,
							destination: identifier,
							destination_local: local
						}),
						None => ()
					}
				},
				Expression::Index {indexee, index} => {
					let (indexee, indexee_tmp) =
						self.generate_expression(*indexee, None);
					let indexee = indexee
						.unwrap_or_else(|| self.temporary_name(indexee_tmp));
					let (index, index_tmp) = self.generate_expression(*index, None);
					let index = index.unwrap_or_else(|| self.temporary_name(index_tmp));
					let (value, value_tmp) = self.generate_expression(value, None);
					let value = value.unwrap_or_else(|| self.temporary_name(value_tmp));

					self.push(OpCode::IndexWrite {indexee, index, value});
					self.temporary_free(indexee_tmp);
					self.temporary_free(index_tmp);
					self.temporary_free(value_tmp);
				},
				_ => todo!()
			},
			Statement::If {condition, then, ..} => {
				let (condition, condition_tmp) =
					self.generate_expression(condition, None);
				let condition = condition
					.unwrap_or_else(|| self.temporary_name(condition_tmp));

				// TODO: Invert bcuz lol.
				let index = self.opcodes.len();
				self.push(OpCode::Jump {
					operation: 0,
					r#if: Some(condition)
				});

				self.temporary_free(condition_tmp);

				then.0.into_iter().for_each(|stmt| self.generate_statement(stmt));
				let len = self.opcodes.len();
				match &mut self.opcodes[index] {
					OpCode::Jump {operation, ..} => *operation = len as u64,
					_ => panic!()
				}
			},
			_ => todo!()
		}
	}

	fn generate_expression(&mut self, expression: Expression,
			destination: Option<&'static str>)
				-> (Option<&'static str>, usize) {
		println!("{:?}", expression);
		let (destination, temporary) = destination.map(|dst| (dst, usize::MAX))
			.unwrap_or_else(|| self.temporary());
		let destination_local = true;

		let result = match expression {
			Expression::Integer(integer) => {
				let constant = self.constant(Value::Integer(integer));
				self.push(OpCode::Load {constant, destination, destination_local});
				None
			},
			Expression::String(string) => {
				let constant = self.constant(Value::String(string.into_boxed_str()));
				self.push(OpCode::Load {constant, destination, destination_local});
				None
			},
			// LEAK
			Expression::Identifier(identifier) => {
				self.temporary_free(temporary);
				Some(&*Box::leak(identifier.into_boxed_str()))
			},
			Expression::Call {function, arguments} => {
				let (function, tmp) = self.generate_expression(*function, None);
				let function = function.unwrap_or_else(|| self.temporary_name(tmp));

				self.push(OpCode::Create {
					destination: "(f",
					destination_local: true
				});

				{
					let (index, index_tmp) = self.temporary();
					let (value, value_tmp) = self.temporary();

					let constant = self.constant(Value::Integer(0));
					self.push(OpCode::Load {
						constant,
						destination: index,
						destination_local: true
					});
					let constant = self.constant(Value::Integer(arguments.len() as i64));
					self.push(OpCode::Load {
						constant,
						destination: value,
						destination_local: true
					});
					self.push(OpCode::IndexWrite {
						indexee: "(f",
						index,
						value
					});

					self.temporary_free(index_tmp);
					self.temporary_free(value_tmp);
				}
				
				for (index, argument) in arguments.into_iter().enumerate() {
					let (argument, arg_tmp) = self.generate_expression(argument, None);
					let argument = argument.unwrap_or_else(|| self.temporary_name(arg_tmp));

					let constant = self.constant(Value::Integer(index as i64 + 1));
					let (index_dest, index_tmp) = self.temporary();
					self.push(OpCode::Load {
						constant,
						destination: index_dest,
						destination_local:  true
					});
					self.push(OpCode::IndexWrite {
						indexee: "(f",
						index: index_dest,
						value: argument
					});

					self.temporary_free(arg_tmp);
					self.temporary_free(index_tmp);
				}

				self.push(OpCode::Call {
					function,
					arguments: "(f",
					destination,
					destination_local: true
				});

				self.temporary_free(tmp);

				None
			},
			Expression::Index {indexee, index} => {
				let (indexee, indexee_tmp) = self.generate_expression(*indexee, None);
				let indexee = indexee
					.unwrap_or_else(|| self.temporary_name(indexee_tmp));
				let (index, index_tmp) = self.generate_expression(*index, None);
				let index = index.unwrap_or_else(|| self.temporary_name(index_tmp));

				self.push(OpCode::IndexRead {
					indexee, index, destination, destination_local});
				None
			},
			Expression::Nil => {
				let constant = u16::MAX;
				self.push(OpCode::Load {constant, destination, destination_local});
				None
			},
			Expression::True => {
				let constant = self.constant(Value::Boolean(true));
				self.push(OpCode::Load {constant, destination, destination_local});
				None
			},
			Expression::False => {
				let constant = self.constant(Value::Boolean(false));
				self.push(OpCode::Load {constant, destination, destination_local});
				None
			},
			_ => todo!()
		};
		(result, temporary)
	}

	fn generate_block(mut self, block: Block) -> Function {
		block.0.into_iter().for_each(|stmt| self.generate_statement(stmt));
		let Self {constants, opcodes, ..} = self;
		Function {constants, opcodes}
	}
}
