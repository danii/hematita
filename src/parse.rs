use self::super::{OpCode, Value, Function};
use std::{io::Read, iter::Peekable};

#[derive(Debug, Eq, PartialEq)]
enum Token {
	Identifier(String),
	Number(i64),
	Comma,
	OpenParen,
	CloseParen,
	Equals,
	Label,
	SemiColon,
	LiteralNil,
	LiteralFalse,
	LiteralTrue,
	KeywordBreak,
	KeywordDo,
	KeywordElse,
	KeywordElseIf,
	KeywordEnd,
	KeywordFor,
	KeywordFunction,
	KeywordGoto,
	KeywordIf,
	KeywordIn,
	KeywordLocal,
	KeywordRepeat,
	KeywordThen,
	KeywordUntil,
	KeywordWhile
}

struct Lexer<T>
		where T: Iterator<Item = char> {
	source: Peekable<T>
}

impl<T> Lexer<T>
		where T: Iterator<Item = char> {
	fn eat(&mut self) {
		self.source.next();
	}

	fn parse_identifier(&mut self) -> Token {
		let mut l = String::new();
		match loop {
			match self.source.peek().unwrap() {
				'a'..='z' | 'A'..='Z' => l.push(self.source.next().unwrap()),
				_ => break &l as &str
			}
		} {
			"local" => Token::KeywordLocal,
			_ => Token::Identifier(l)
		}
	}

	fn parse_string(&mut self) -> Token {
		todo!()
	}

	fn parse_number(&mut self) -> Token {
		let mut l = String::new();
		Token::Number(loop {
			match self.source.peek().unwrap() {
				'-' | '0'..='9' => l.push(self.source.next().unwrap()),
				_ => break l
			}
		}.parse().unwrap())
	}

	fn parse_whitespace(&mut self) -> Option<char> {
		loop {
			match *self.source.peek()? {
				' ' | '\n' => self.eat(),
				character => break Some(character)
			}
		}
	}
}

impl<T> Iterator for Lexer<T> where T: Iterator<Item = char> {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		Some(match self.parse_whitespace()? {
			';' => {self.eat(); Token::SemiColon},
			'=' => {self.eat(); Token::Equals},
			'(' => {self.eat(); Token::OpenParen},
			')' => {self.eat(); Token::CloseParen},
			',' => {self.eat(); Token::Comma},
			'"' => self.parse_string(),
			'-' | '0'..='9' => self.parse_number(),
			_ => self.parse_identifier()
		})
	}
}

enum Expression {
	Number(i64),
	Identifier(String)
}

enum Statement {
	Call(String, Vec<Expression>),
	Assignment(String, Expression)
}

struct Parser<T>
		where T: Iterator<Item = Token> {
	source: Peekable<T>
}

impl<T> Parser<T>
		where T: Iterator<Item = Token> {
	fn exp_value(&mut self) -> Expression {
		match self.source.next().unwrap() {
			Token::Identifier(identifier) => Expression::Identifier(identifier),
			Token::Number(number) => Expression::Number(number),
			_ => todo!()
		}
	}
}

impl<T> Iterator for Parser<T>
		where T: Iterator<Item = Token> {
	type Item = Statement;

	fn next(&mut self) -> Option<Statement> {
		Some(match self.source.next()? {
			Token::Identifier(actor) => match self.source.next() {
				Some(Token::OpenParen) => {
					let mut call_expressions = Vec::new();

					loop {
						match self.source.peek() {
							Some(Token::CloseParen) => {
								self.source.next();
								break Statement::Call(actor, call_expressions)
							},
							Some(Token::Comma) => {
								self.source.next();
								// TODO: Make commas make sense
							},
							Some(_) => call_expressions.push(self.exp_value()),
							_ => todo!()
						}
					}
				},

				Some(Token::Equals) => {
					let expr = self.exp_value();
					Statement::Assignment(actor, expr)
				},
				_ => todo!()
			},
			_ => todo!()
		})
	}
}

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
