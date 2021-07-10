use itertools::Itertools;

use self::super::lexer::Token;
use std::{fmt::{Display, Formatter, Result as FMTResult}, iter::{Peekable, from_fn}};

macro_rules! expect {
	($value:expr, $type:pat) => {
		let value = $value;
		if !matches!(value, Some($type)) {return Err(Error(value))}
	}
}

macro_rules! iter_expect {
	($value:expr, $type:pat) => {
		let value = $value;
		if !matches!(value, Some($type)) {return Some(Err(Error(value)))}
	}
}

macro_rules! iter_throw {
	($value:expr) => {
		match $value {
			Ok(value) => value,
			Err(error) => return Some(Err(error))
		}
	}
}

#[derive(Debug)]
pub struct Error(Option<Token>);

impl std::error::Error for Error {}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match &self.0 {
			Some(token) => write!(f, "unexpected '{}'", token),
			None => write!(f, "unexpected end of file")
		}
	}
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct TokenIterator<I>(pub Peekable<I>)
	where I: Iterator<Item = Token>;

impl<I> TokenIterator<I>
		where I: Iterator<Item = Token> {
	// Eats a token, disposing of it.
	fn eat(&mut self) {
		self.next();
	}

	/// Returns the next token, if any.
	fn next(&mut self) -> Option<Token> {
		self.0.next()
	}

	/// Eats a token, then peeks the next one, if any.
	fn eat_peek(&mut self) -> Option<&Token> {
		self.eat();
		self.peek()
	}

	/// Returns the next token, assuming it's an identifier.
	fn identifier(&mut self) -> String {
		match self.next() {
			Some(Token::Identifier(identifier)) => identifier,
			_ => unreachable!()
		}
	}

	/// Returns the next token, assuming it's a string.
	fn string(&mut self) -> String {
		match self.next() {
			Some(Token::String(string)) => string,
			_ => unreachable!()
		}
	}

	/// Returns the next token, assuming it's an integer.
	fn integer(&mut self) -> i64 {
		match self.next() {
			Some(Token::Integer(integer)) => integer,
			_ => unreachable!()
		}
	}

	/// Peeks the next token, if any.
	fn peek(&mut self) -> Option<&Token> {
		self.0.peek()
	}
}

/// Parses a block of lua tokens.
pub fn parse(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Block> {
	let mut statements = Vec::new();

	loop {
		match iter.peek() {
			// ;
			Some(Token::SemiColon) => iter.eat(),

			// actor
			Some(Token::Identifier(_)) => match parse_expression(iter)? {
				// actor()
				Expression::Call {function, arguments} =>
					statements.push(Statement::Call {function: *function, arguments}),

				// actor = value
				actor => {
					expect!(iter.next(), Token::Assign);
					let value = parse_expression(iter)?;
					statements.push(Statement::Assign {actor, value, local: false})
				}
			},

			// local
			Some(Token::KeywordLocal) => match iter.eat_peek() {
				// local actor = value
				Some(Token::Identifier(_)) => {
					let actor = Expression::Identifier(iter.identifier());
					expect!(iter.next(), Token::Assign);
					let value = parse_expression(iter)?;
					statements.push(Statement::Assign {actor, value, local: true})
				},

				// local function actor()
				Some(Token::KeywordFunction) => {
					let (name, arguments, body) = parse_function(iter, true)?;
					let (name, local) = (name.unwrap(), true);
					statements.push(Statement::Function {name, arguments, body, local})
				},

				_ => break Err(Error(iter.next()))
			},

			// function actor()
			Some(Token::KeywordFunction) => {
				let (name, arguments, body) = parse_function(iter, true)?;
				let (name, local) = (name.unwrap(), false);
				statements.push(Statement::Function {name, arguments, body, local})
			},

			// if actor
			Some(Token::KeywordIf) => statements.push(parse_if(iter)?),

			Some(Token::KeywordEnd) | None => break Ok(Block(statements)),
			Some(_) => break Err(Error(iter.next()))
		}
	}
}

pub fn parse_expression(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Expression> {
	match iter.peek() {
		// Literals
		Some(Token::Identifier(_)) => {
			let identifier = iter.identifier();
			parse_inner_expression(iter, Expression::Identifier(identifier))
		},
		Some(Token::Integer(_)) => {
			let integer = iter.integer();
			parse_inner_expression(iter, Expression::Integer(integer))
		},
		Some(Token::String(_)) => {
			let string = iter.string();
			parse_inner_expression(iter, Expression::String(string))
		},

		// Function Call
		Some(Token::OpenCurly) => {
			iter.eat();
			assert_eq!(iter.next(), Some(Token::CloseCurly));
			Ok(Expression::Table())
		},
		Some(Token::KeywordFunction) => {
			let (_, arguments, body) = parse_function(iter, false)?;
			Ok(Expression::Function {arguments, body})
		},
		Some(Token::LiteralNil) => {iter.eat(); Ok(Expression::Nil)},
		Some(Token::LiteralTrue) => {iter.eat(); Ok(Expression::True)},
		Some(Token::LiteralFalse) => {iter.eat(); Ok(Expression::False)},
		_ => Err(Error(iter.next()))
	}
}

pub fn parse_inner_expression(iter: &mut TokenIterator<impl Iterator<Item = Token>>,
		actor: Expression) -> Result<Expression> {
	match iter.peek() {
		// actor <binary operation>
		Some(Token::Equal | Token::NotEqual | Token::LessThan
				| Token::LessThanOrEqual | Token::GreaterThan
				| Token::GreaterThanOrEqual | Token::Add | Token::Subtract) =>
					Ok(Expression::BinaryOperation {
			left: Box::new(actor),
			operator: match iter.next().unwrap() {
				Token::Equal => BinaryOperator::Equal,
				Token::NotEqual => BinaryOperator::NotEqual,
				Token::LessThan => BinaryOperator::LessThan,
				Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
				Token::GreaterThan => BinaryOperator::GreaterThan,
				Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
				Token::Add => BinaryOperator::Add,
				_ => unreachable!()
			},
			right: Box::new(parse_expression(iter)?)
		}),

		// actor()
		Some(Token::OpenParen) => {
			let mut first = true;
			let arguments = from_fn(|| match iter.peek() {
				Some(Token::OpenParen) if first => {
					iter.eat(); first = false;
					if let Some(Token::CloseParen) = iter.peek()
						{iter.eat(); return None}
					Some(parse_expression(iter))
				},
				Some(Token::Comma) => {iter.eat(); Some(parse_expression(iter))},
				Some(Token::CloseParen) => {iter.eat(); None},
				_ => Some(Err(Error(iter.next())))
			}).try_collect()?;

			parse_inner_expression(iter, Expression::Call {
				function: Box::new(actor),
				arguments
			})
		},

		// actor.
		Some(Token::Period) => match iter.eat_peek() {
			// actor.identifier
			Some(Token::Identifier(_)) => {
				let index = Box::new(Expression::String(iter.identifier()));
				parse_inner_expression(iter, Expression::Index {
					index, indexee: Box::new(actor)
				})
			},

			// No other token but an identifier is expected here.
			_ => Err(Error(iter.next()))
		},

		// actor[]
		Some(Token::OpenBracket) => {
			iter.eat();

			let index = Box::new(parse_expression(iter)?);
			let result = parse_inner_expression(iter, Expression::Index {
				index, indexee: Box::new(actor)
			})?;
			match iter.next() {
				Some(Token::CloseBracket) => parse_inner_expression(iter, result),
				token => Err(Error(token))
			}
		},

		_ => Ok(actor)
	}
}

pub fn parse_if(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Statement> {
	expect!(iter.next(), Token::KeywordIf);
	let condition = parse_expression(iter)?;
	expect!(iter.next(), Token::KeywordThen);
	let then = parse(iter)?;

	let mut r#else = None;
	let else_ifs = from_fn(|| match iter.next() {
		Some(Token::KeywordEnd) => None,
		Some(Token::KeywordElseIf) => {
			let condition = iter_throw!(parse_expression(iter));
			iter_expect!(iter.next(), Token::KeywordThen);
			let then = iter_throw!(parse(iter));
			Some(Ok(ElseIf {condition, then}))
		},
		Some(Token::KeywordElse) => {
			r#else = Some(iter_throw!(parse(iter)));
			None
		},
		token => Some(Err(Error(token)))
	}).try_collect()?;

	Ok(Statement::If {
		condition,
		then,
		else_ifs,
		r#else
	})
}

pub fn parse_function(iter: &mut TokenIterator<impl Iterator<Item = Token>>,
		parse_name: bool) -> Result<(Option<String>, Vec<String>, Block)> {
	expect!(iter.next(), Token::KeywordFunction);
	let name = if parse_name {
		match iter.next() {
			Some(Token::Identifier(name)) => Some(name),
			token => return Err(Error(token))
		}
	} else {None};

	let mut first = true;
	let arguments = from_fn(|| match iter.peek() {
		Some(Token::OpenParen) if first => {
			iter.eat(); first = false;
			match iter.next() {
				Some(Token::Identifier(argument)) => Some(Ok(argument)),
				Some(Token::CloseParen) => None,
				token => Some(Err(Error(token)))
			}
		},
		Some(Token::Comma) => {
			iter.eat();
			match iter.next() {
				Some(Token::Identifier(argument)) => Some(Ok(argument)),
				token => Some(Err(Error(token)))
			}
		},
		Some(Token::CloseParen) => {iter.eat(); None},
		_ => Some(Err(Error(iter.next())))
	}).try_collect()?;
	let body = parse(iter)?;
	expect!(iter.next(), Token::KeywordEnd);

	Ok((name, arguments, body))
}

// TODO: Should we remove [crate::vm::BinaryOperation] and use this instead?
// Same goes for UnaryOperator and Operation.
#[derive(Debug)]
pub enum BinaryOperator {
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Add,
	Subtract,
	Multiply,
	Divide
}

impl Display for BinaryOperator {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Equal => write!(f, "=="),
			Self::NotEqual => write!(f, "~="),
			Self::LessThan => write!(f, "<"),
			Self::LessThanOrEqual => write!(f, "<="),
			Self::GreaterThan => write!(f, ">"),
			Self::GreaterThanOrEqual => write!(f, ">="),
			Self::Add => write!(f, "+"),
			Self::Subtract => write!(f, "-"),
			Self::Multiply => write!(f, "*"),
			Self::Divide => write!(f, "/"),
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

// TODO: Sort these so they make sense...
#[derive(Debug)]
pub enum Expression {
	Integer(i64),
	String(String),
	Identifier(String),
	Table(),
	Nil,
	True,
	False,
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
