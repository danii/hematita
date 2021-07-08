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
		let tmp = (0..).find(|tmp| {
			matches!(self.temporaries.get(tmp), None)
		})
			.unwrap();
		self.temporaries.insert(tmp);
		// LEAK
		(self.temporary_name(tmp), tmp)
	}

	fn temporary_name(&self, tmp: usize) -> &'static str {
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
			_ => todo!()
		}
	}

	fn generate_expression(&mut self, expression: Expression,
			destination: Option<&'static str>)
				-> (Option<&'static str>, usize) {
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
