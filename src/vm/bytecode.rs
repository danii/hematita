use self::super::{super::ast::parser::{Block, Expression, Statement}, Value, OpCode, Function};

pub fn generate_bytecode(block: Block) -> Function {
	let mut constants = Vec::new();
	let mut opcodes = Vec::new();

	for statement in block.0 {
		match statement {
			Statement::Call {function, arguments} => {
				let identifier = match function {
					Expression::Identifier(identifier) => identifier,
					_ => todo!()
				};

				opcodes.push(OpCode::Create {
					destination: "(f",
					destination_local: true
				});

				for (index, argument) in arguments.into_iter().enumerate() {
					let identifier = match argument {
						Expression::Identifier(identifier) => identifier,
						Expression::String(string) => {
							opcodes.push(OpCode::Load {
								constant: get_or_add_constant(Value::String(string.into_boxed_str()), &mut constants),
								destination: "(t1",
								destination_local:  true
							});

							"(t1".to_string()
						},
						_ => todo!()
					};

					let index = index as i64;
					opcodes.push(OpCode::Load {
						constant: get_or_add_constant(Value::Integer(index + 1), &mut constants),
						destination: "(t0",
						destination_local:  true
					});

					opcodes.push(OpCode::IndexWrite {
						indexee: "(f",
						index: "(t0",
						// TODO: Leak bad :(
						value: Box::leak(identifier.to_owned().into_boxed_str())
					});
				}

				opcodes.push(OpCode::Call {
					// TODO: Leak bad :(
					function: Box::leak(identifier.to_owned().into_boxed_str()),
					arguments: "(f",
					destination: "(_",
					destination_local: true
				})
			},
			_ => todo!()
		}
	}

	Function {constants, opcodes}
}

fn get_or_add_constant(constant: Value, constant_pool: &mut Vec<Value>) -> u16 {
	match constant_pool.iter().position(|others| *others == constant) {
		Some(id) => id as u16,
		None => {
			constant_pool.push(constant);
			constant_pool.len() as u16 - 1
		}
	}
}
