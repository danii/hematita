//use self::super::{OpCode, Value, Function};
use self::super::lexer::Token;
use std::{fmt::{Display, Formatter, Result as FMTResult}, iter::Peekable};
use itertools::Itertools;

/// Parses a block of lua tokens.
pub fn parse(iter: &mut Peekable<impl Iterator<Item = Token>>) -> Block {
	let mut statements = Vec::new();

	loop {
		match iter.peek() {
			Some(Token::SemiColon) => (),

			Some(Token::Identifier(_)) => match parse_expression(iter) {
				Expression::Call {function, arguments} => {
					let function = *function;
					statements.push(Statement::Call {function, arguments});
				},
				actor => {
					assert_eq!(iter.next(), Some(Token::Assign));
					let value = parse_expression(iter);
					statements.push(Statement::Assign {actor, value, local: false})
				}
			},

			Some(Token::KeywordLocal) => match {iter.next(); iter.peek().unwrap()} {
				Token::Identifier(actor) => {
					let actor = Expression::Identifier(actor.clone()); // TIDY
					iter.next();
					assert_eq!(iter.next(), Some(Token::Assign));
					let value = parse_expression(iter);
					statements.push(Statement::Assign {actor, value, local: true})
				},
				Token::KeywordFunction =>
					statements.push(parse_function_statement(iter, true)),
				_ => todo!()
			},

			Some(Token::KeywordFunction) =>
				statements.push(parse_function_statement(iter, false)),

			Some(Token::KeywordIf) => statements.push(parse_if(iter)),

			_ => break Block(statements)
		}
	}
}

pub fn parse_if(iter: &mut Peekable<impl Iterator<Item = Token>>) -> Statement {
	assert_eq!(iter.next(), Some(Token::KeywordIf)); // todo!()
	let condition = parse_expression(iter);
	assert_eq!(iter.next(), Some(Token::KeywordThen)); // todo!()
	let then = parse(iter);

	match iter.next().unwrap() {
		Token::KeywordEnd => {
			Statement::If {
				condition,
				then,
				else_ifs: Vec::new(),
				r#else: None
			}
		},
		_ => todo!()
	}
}

pub fn parse_expression(iter: &mut Peekable<impl Iterator<Item = Token>>)
		-> Expression {
	match iter.next().unwrap() {
		Token::Identifier(actor) =>
			parse_inner_expression(iter, Expression::Identifier(actor)),
		Token::Integer(integer) =>
			parse_inner_expression(iter, Expression::Integer(integer)),
		Token::String(string) =>
			parse_inner_expression(iter, Expression::String(string)),
		Token::OpenCurly => {
			assert_eq!(iter.next(), Some(Token::CloseCurly));
			Expression::Table()
		},
		Token::KeywordFunction => parse_function_expression(iter),
		a => todo!("{:?}", a)
		//_ => todo!()
	}
}

pub fn parse_inner_expression(iter: &mut Peekable<impl Iterator<Item = Token>>,
		actor: Expression) -> Expression {
	match iter.peek() {
		// actor.
		Some(Token::Period) => match {iter.next(); iter.next().unwrap()} {
			// actor.identifier
			Token::Identifier(index) => {
				let expression = Expression::Index {
					indexee: Box::new(actor),
					index: Box::new(Expression::String(index))
				};

				parse_inner_expression(iter, expression)
			},
			_ => todo!()
		},
		// actor[]
		Some(Token::OpenBracket) => {
			iter.next();
			let expression = Expression::Index {
				indexee: Box::new(actor),
				index: Box::new(parse_expression(iter))
			};

			let result = parse_inner_expression(iter, expression);
			assert_eq!(iter.next(), Some(Token::CloseBracket));
			parse_inner_expression(iter, result)
		},
		// actor + / -
		Some(Token::Add | Token::Subtract) => {
			Expression::BinaryOperation {
				left: Box::new(actor),
				operator: if matches!(iter.next().unwrap(), Token::Add)
					{BinaryOperator::Add} else {BinaryOperator::Subtract},
				right: Box::new(parse_expression(iter))
			}
		},
		// actor()
		Some(Token::OpenParen) => {
			iter.next();

			let mut arguments = Vec::new();

			if let Some(Token::CloseParen) = iter.peek() {
				iter.next();
			} else {
				loop {
					arguments.push(parse_expression(iter));
					match iter.peek().unwrap() {
						Token::Comma => {iter.next();},
						Token::CloseParen => {iter.next(); break}
						_ => todo!()
					}
				}
			}

			let expression = Expression::Call {
				function: Box::new(actor),
				arguments
			};
			parse_inner_expression(iter, expression)
		},
		_ => actor
	}
}

pub fn parse_function_statement(iter: &mut Peekable<impl Iterator<Item = Token>>,
		local: bool) -> Statement {
	assert_eq!(iter.next(), Some(Token::KeywordFunction));
	let name = match iter.next().unwrap() {
		Token::Identifier(name) => name,
		_ => todo!() // TIDY todo!()
	};
	assert_eq!(iter.next(), Some(Token::OpenParen));

	let mut arguments = Vec::new();
	if let Some(Token::CloseParen) = iter.peek() {
		iter.next();
	} else {
		loop {
			let name = iter.next().unwrap();
			match name {
				Token::Identifier(name) => arguments.push(name),
				_ => todo!() // TIDY todo!()
			}

			match iter.peek().unwrap() {
				Token::Comma => {iter.next();},
				Token::CloseParen => {iter.next(); break}
				_ => todo!()
			}
		}
	}

	let block = parse(iter);

	match iter.next().unwrap() {
		Token::KeywordEnd => {
			Statement::Function {
				name,
				arguments,
				body: block,
				local
			}
		},
		_ => todo!()
	}
}

pub fn parse_function_expression(iter: &mut Peekable<impl Iterator<Item = Token>>)
		-> Expression {
	assert_eq!(iter.next(), Some(Token::OpenParen));

	let mut arguments = Vec::new();
	if let Some(Token::CloseParen) = iter.peek() {
		iter.next();
	} else {
		loop {
			let name = iter.next().unwrap();
			match name {
				Token::Identifier(name) => arguments.push(name),
				_ => todo!() // TIDY todo!()
			}

			match iter.peek().unwrap() {
				Token::Comma => {iter.next();},
				Token::CloseParen => {iter.next(); break}
				_ => todo!()
			}
		}
	}

	let block = parse(iter);

	match iter.next().unwrap() {
		Token::KeywordEnd => {
			Expression::Function {
				arguments,
				body: block
			}
		},
		_ => todo!()
	}
}

#[derive(Debug)]
pub enum BinaryOperator {
	Add,
	Subtract,
	Multiply,
	Divide
}

impl Display for BinaryOperator {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Add => write!(f, "+"),
			Self::Subtract => write!(f, "-"),
			Self::Multiply => write!(f, "*"),
			Self::Divide => write!(f, "/")
		}
	}
}

#[derive(Debug)]
pub struct Block(pub Vec<Statement>);

impl Display for Block {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		self.0.iter().try_for_each(|stmt| write!(f, "{}\n", stmt))
	}
}

#[derive(Debug)]
pub struct ElseIf {
	condition: Expression,
	then: Block
}

#[derive(Debug)]
pub enum Expression {
	Integer(i64),
	String(String),
	Identifier(String),
	Table(),
	Call {
		function: Box<Expression>,
		arguments: Vec<Expression>
	},
	Index {
		indexee: Box<Expression>,
		index: Box<Expression>
	},
	BinaryOperation {
		left: Box<Expression>,
		operator: BinaryOperator,
		right: Box<Expression>
	},
	Function {
		arguments: Vec<String>,
		body: Block
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Integer(integer) => write!(f, "{}", integer),
			Self::String(string) => write!(f, "{:?}", string),
			Self::Identifier(identifier) => write!(f, "{}", identifier),
			Self::Call {function, arguments} => {
				write!(f, "{}(", function)?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")")
			},
			Self::Index {indexee, index} => write!(f, "{}[{}]", indexee, index),
			Self::BinaryOperation {left, operator, right} =>
				write!(f, "{} {} {}", left, operator, right),
			Self::Function {arguments, body} => {
				write!(f, "function(")?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")\n{}end", body)
			}	,
			_ => write!(f, "<pls dipslay me>")
		}
	}
}

#[derive(Debug)]
pub enum Statement {
	Call {
		function: Expression,
		arguments: Vec<Expression>
	},
	Assign {
		actor: Expression,
		value: Expression,
		local: bool
	},
	If {
		condition: Expression,
		then: Block,
		else_ifs: Vec<ElseIf>,
		r#else: Option<Block>
	},
	Function {
		name: String,
		arguments: Vec<String>,
		body: Block,
		local: bool
	}
}

impl Display for Statement {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Call {function, arguments} => {
				write!(f, "{}(", function)?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")")
			},
			Self::Assign {actor, value, local} => if *local
				{write!(f, "local {} = {}", actor, value)} else
				{write!(f, "{} = {}", actor, value)},
			Self::If {condition, then, ..} =>
				write!(f, "if {} then\n{}end", condition, then),
			Self::Function {name, arguments, body, local} => {
				if *local {write!(f, "local ")?}
				write!(f, "function {}(", name)?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")\n{}end", body)
			}	
		}
	}
}

/*
pub struct Parser<T>
		where T: Iterator<Item = Token> {
	pub source: Peekable<T>
}

impl<T> Parser<T>
		where T: Iterator<Item = Token> {
	fn exp_value(&mut self) -> Expression {
		match self.source.next().unwrap() {
			Token::Identifier(identifier) => Expression::Identifier(identifier),
			Token::Integer(number) => Expression::Integer(number),
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

				Some(Token::Assign) => {
					let expr = self.exp_value();
					Statement::Assignment(actor, expr)
				},
				_ => todo!()
			},
			_ => todo!()
		})
	}
}*/
