pub mod lexer;
pub mod parser;

/*


pub fn dord(mut file: std::fs::File) -> Function {
	let mut string = String::new();
	file.read_to_string(&mut string).unwrap();

	let mut parser = Parser {
		source: Lexer {
			source: string.chars().peekable()
		}.peekable()
	};

	let mut constants = Vec::new();
	let mut opcodes = Vec::new();
	for statement in parser {
		match statement {
			Statement::Call(target, expressions) => {
				opcodes.push(OpCode::Create {
					destination: "(f",
					destination_local: true
				});

				expressions.into_iter().enumerate().for_each(|(index, exp)| {
					match exp {
						Expression::Identifier(e) => {
							constants.push(Value::Integer(index as i64));

							// TODO: Constant stuff blah CBA.
							opcodes.push(OpCode::Load {
								destination: "(t",
								constant: (constants.len() - 1) as u16,
								destination_local: true
							});

							opcodes.push(OpCode::IndexWrite {
								indexee: "(f",
								// TODO: Fix leak.
								index: "(t",
								// TODO: Fix leak.
								value: Box::leak(e.into_boxed_str()),
							})
						},
						_ => todo!() // CBA
					}
				});

				opcodes.push(OpCode::Call {
					arguments: "(f",
					// TODO: Fix leak.
					function: Box::leak(target.into_boxed_str()),
					destination: "(_",
					destination_local: true
				})
			},
			Statement::Assignment(target, Expression::Number(number)) => {
				let value = Value::Integer(number);
				let value = match constants.iter().position(|c| c == &value) {
					Some(value) => value,
					None => {constants.push(value); constants.len() - 1}
				};

				opcodes.push(OpCode::Load {
					constant: value as u16,
					// TODO: Fix leak.
					destination: Box::leak(target.into_boxed_str()),
					destination_local: true
				});
			},
			_ => todo!()
		}
	}

	Function {
		constants,
		opcodes
	}
}
*/