use self::super::lexer::Token;
use itertools::Itertools;
use std::{
	error::Error as STDError,
	fmt::{Display, Formatter, Result as FMTResult},
	iter::{Peekable, from_fn},
	result::Result as STDResult
};

macro_rules! expression {
	($($(#[$($attrib:tt)*])* $name:ident($next:ident)
			{$($token:ident => $bin_op:ident),*}),*) => {
		$(
			$(#[$($attrib)*])*
			fn $name<I>(iter: &mut TokenIterator<I>) -> Result<Expression>
					where I: Iterator<Item = Token> {
				let mut expression = $next(iter)?;

				loop {
					match iter.peek() {
						$(Some(Token::$token) => {
							iter.eat();

							expression = Expression::BinaryOperation {
								left: Box::new(expression),
								operator: BinaryOperator::$bin_op,
								right: Box::new($next(iter)?)
							}
						}),*
						_ => break Ok(expression)
					}
				}
			}
		)*
	}
}

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

pub type Result<T> = STDResult<T, Error>;

#[derive(Debug)]
pub struct Error(pub Option<Token>);

impl STDError for Error {}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match &self.0 {
			Some(token) => write!(f, "unexpected '{}'", token),
			None => write!(f, "unexpected end of file")
		}
	}
}

/// A wrapped peekable iterator of tokens with specialized methods for parsing.
///
/// This is made for having a mutable reference to it be passed around between
/// parsing functions, and being peeked and consumed token by token via helper
/// methods.
pub struct TokenIterator<I>(pub Peekable<I>)
	where I: Iterator<Item = Token>;

impl<I> TokenIterator<I>
		where I: Iterator<Item = Token> {
	// Eats a token, disposing of it.
	fn eat(&mut self) {
		self.next();
	}

	/// Returns the next token, if any.
	pub fn next(&mut self) -> Option<Token> {
		loop {
			match self.0.next() {
				Some(Token::Comment(_)) => (),
				token => break token
			}
		}
	}

	/// Eats a token, then peeks the next one, if any.
	fn eat_peek(&mut self) -> Option<Token> {
		self.eat();
		self.peek()
	}

	/// Returns the next token, assuming it was peeked and matched as an
	/// identifier.
	fn identifier(&mut self) -> String {
		match self.next() {
			Some(Token::Identifier(identifier)) => identifier,
			_ => unreachable!()
		}
	}

	/// Returns the next token, assuming it was peeked and matched as a string.
	fn string(&mut self) -> String {
		match self.next() {
			Some(Token::String(string)) => string,
			_ => unreachable!()
		}
	}

	/// Returns the next token, assuming it was peeked and matched as an integer.
	fn integer(&mut self) -> i64 {
		match self.next() {
			Some(Token::Integer(integer)) => integer,
			_ => unreachable!()
		}
	}

	/// Peeks the next token, if any.
	fn peek(&mut self) -> Option<Token> {
		// God this borrow check bug is annoying.
		match self.0.peek().cloned() {
			Some(Token::Comment(_)) => self.eat_peek(),
			token => token
		}
	}
}

/// Parses a block of lua tokens.
pub fn parse_block(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
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

			// if
			Some(Token::KeywordIf) => statements.push(parse_if(iter)?),

			// for
			Some(Token::KeywordFor) => statements.push(parse_for(iter)?),

			// while
			// repeat
			Some(Token::KeywordWhile | Token::KeywordRepeat) =>
				statements.push(parse_while(iter)?),

			// return
			Some(Token::KeywordReturn) => {
				iter.eat();

				let mut values = Vec::new();
				loop {
					values.push(parse_expression(iter)?);
					match iter.peek() {
						Some(Token::Comma) => iter.eat(),
						_ => break
					}
				}

				statements.push(Statement::Return {values})
			},

			//Some(Token::KeywordEnd) | None => break Ok(Block(statements)),
			_ => break Ok(Block(statements))
			//Some(_) => break Err(Error(iter.next()))
		}
	}
}

/*
	Operator Precedence

	or
	and
	< > <= >= ~= ==
	|
	~
	&
	<< >>
	..
	+ -
	* / // %
	not # - ~
	^
*/

pub fn parse_expression<I>(iter: &mut TokenIterator<I>) -> Result<Expression>
		where I: Iterator<Item = Token> {
	parse_expression_logical_or(iter)
}

expression! {
	/// Test
	parse_expression_logical_or(parse_expression_logical_and) {
		KeywordOr => LogicalOr
	},
	parse_expression_logical_and(parse_expression_comparison) {
		KeywordAnd => LogicalAnd
	},
	parse_expression_comparison(parse_expression_bitwise_or) {
		Equal => Equal,
		NotEqual => NotEqual,
		LessThan => LessThan,
		GreaterThan => GreaterThan,
		LessThanOrEqual => LessThanOrEqual,
		GreaterThanOrEqual => GreaterThanOrEqual
	},
	parse_expression_bitwise_or(parse_expression_bitwise_xor) {
		BitwiseOr => BitwiseOr
	},
	parse_expression_bitwise_xor(parse_expression_bitwise_and) {
		BitwiseNotOrXOr => BitwiseXOr
	},
	parse_expression_bitwise_and(parse_expression_bitwise_shift) {
		BitwiseAnd => BitwiseAnd
	},
	parse_expression_bitwise_shift(parse_expression_concat) {
		ShiftLeft => ShiftLeft,
		ShiftRight => ShiftRight
	},
	parse_expression_term(parse_expression_factor) {
		Add => Add,
		Minus => Subtract
	},
	parse_expression_factor(parse_expression_unary) {
		Multiply => Multiply,
		Divide => Divide,
		FloorDivide => FloorDivide,
		Modulo => Modulo
	}
}

// Note the right associativity.
pub fn parse_expression_concat<I>(iter: &mut TokenIterator<I>)
		-> Result<Expression> where I: Iterator<Item = Token> {
	let mut expression = parse_expression_term(iter)?;

	loop {
		if let Some(Token::Concat) = iter.peek() {
			iter.eat();

			expression = Expression::BinaryOperation {
				left: Box::new(expression),
				operator: BinaryOperator::Concat,
				right: Box::new(parse_expression_concat(iter)?)
			}
		} else {break Ok(expression)}
	}
}

// Uses recursion over loops.
pub fn parse_expression_unary<I>(iter: &mut TokenIterator<I>)
		-> Result<Expression> where I: Iterator<Item = Token> {
	Ok(match iter.peek() {
		Some(Token::KeywordNot) => {
			iter.eat();

			Expression::UnaryOperation {
				operator: UnaryOperator::LogicalNot,
				operand: Box::new(parse_expression_unary(iter)?)
			}
		},

		Some(Token::Length) => {
			iter.eat();

			Expression::UnaryOperation {
				operator: UnaryOperator::Length,
				operand: Box::new(parse_expression_unary(iter)?)
			}
		},

		Some(Token::Minus) => {
			iter.eat();

			Expression::UnaryOperation {
				operator: UnaryOperator::Negate,
				operand: Box::new(parse_expression_unary(iter)?)
			}
		},

		Some(Token::BitwiseNotOrXOr) => {
			iter.eat();

			Expression::UnaryOperation {
				operator: UnaryOperator::BitwiseNot,
				operand: Box::new(parse_expression_unary(iter)?)
			}
		},

		_ => parse_expression_exponent(iter)?
	})
}

// Again, note the right associativity.
pub fn parse_expression_exponent<I>(iter: &mut TokenIterator<I>)
		-> Result<Expression> where I: Iterator<Item = Token> {
	let mut expression = parse_expression_primary(iter)?;

	loop {
		if let Some(Token::Exponent) = iter.peek() {
			iter.eat();
			expression = Expression::BinaryOperation {
				left: Box::new(expression),
				operator: BinaryOperator::Exponent,
				right: Box::new(parse_expression_exponent(iter)?)
			}
		} else {break Ok(expression)}
	}
}

pub fn parse_expression_primary<I>(iter: &mut TokenIterator<I>)
			-> Result<Expression> where I: Iterator<Item = Token> {
	Ok(match iter.peek() {
		// Literals
		Some(Token::Identifier(_)) => {
			let identifier = iter.identifier(); // Ugh
			parse_expression_inner(iter, Expression::Identifier(identifier))?
		},
		Some(Token::Integer(_)) => Expression::Integer(iter.integer()),
		Some(Token::String(_)) => Expression::String(iter.string()),

		// Simple literals
		Some(Token::LiteralNil) => {iter.eat(); Expression::Nil},
		Some(Token::LiteralTrue) => {iter.eat(); Expression::True},
		Some(Token::LiteralFalse) => {iter.eat(); Expression::False},

		// Parenthesis
		Some(Token::OpenParen) => {
			iter.eat();
			let expression = parse_expression(iter)?;
			expect!(iter.next(), Token::CloseParen);
			parse_expression_inner(iter, expression)?
		},

		// Complex literals
		Some(Token::OpenCurly) => parse_table(iter)?,
		Some(Token::KeywordFunction) => {
			let (_, arguments, body) = parse_function(iter, false)?;
			Expression::Function {arguments, body}
		},

		_ => return Err(Error(iter.next()))
	})
}

/// Only literals and expressions in parenthesis run this function after being
/// parsed.
pub fn parse_expression_inner<I>(iter: &mut TokenIterator<I>, actor: Expression)
		-> Result<Expression> where I: Iterator<Item = Token> {
	match iter.peek() {
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

			let function = Box::new(actor);
			parse_expression_inner(iter, Expression::Call {function, arguments})
		},

		// actor "string"
		Some(Token::String(_)) => {
			let arguments = vec![Expression::String(iter.string())];
			let function = Box::new(actor);
			parse_expression_inner(iter, Expression::Call {function, arguments})
		},

		// actor {"table"}
		Some(Token::OpenCurly) => {
			let arguments = vec![parse_table(iter)?];
			let function = Box::new(actor);
			parse_expression_inner(iter, Expression::Call {function, arguments})
		},

		// actor.
		Some(Token::Period) => match iter.eat_peek() {
			// actor.identifier
			Some(Token::Identifier(_)) => {
				let index = Box::new(Expression::String(iter.identifier()));
				parse_expression_inner(iter, Expression::Index {
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
			let result = parse_expression_inner(iter, Expression::Index {
				index, indexee: Box::new(actor)
			})?;
			match iter.next() {
				Some(Token::CloseBracket) => parse_expression_inner(iter, result),
				token => Err(Error(token))
			}
		},

		_ => Ok(actor)
	}
}

pub fn parse_table(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Expression> {
	expect!(iter.next(), Token::OpenCurly);

	let mut array = Vec::new();
	let mut key_value = Vec::new();
	let mut first = true;
	loop {
		match iter.peek() {
			// [actor] = value
			Some(Token::OpenBracket) => {
				iter.eat(); let key = parse_expression(iter)?;
				expect!(iter.next(), Token::CloseBracket);
				expect!(iter.next(), Token::Assign);
				let value = parse_expression(iter)?;

				match iter.next() {
					// [actor] = value,
					Some(Token::Comma) => key_value.push(KeyValue {key, value}),

					// [actor] = value}
					Some(Token::CloseCurly) => {
						key_value.push(KeyValue {key, value});
						break Ok(Expression::Table {array, key_value})
					},

					// Unexpected token.
					token => break Err(Error(token))
				}
			},

			// actor
			Some(Token::Identifier(_)) => {
				let key = Expression::String(iter.identifier());

				match iter.next() {
					// actor,
					Some(Token::Comma) => array.push(key),
		
					// actor = value
					Some(Token::Assign) => {
						let value = parse_expression(iter)?;
						match iter.next() {
							// actor = value,
							Some(Token::Comma) => key_value.push(KeyValue {key, value}),
		
							// actor = value}
							Some(Token::CloseCurly) => {
								key_value.push(KeyValue {key, value});
								break Ok(Expression::Table {array, key_value})
							},
		
							// Unexpected token.
							token => break Err(Error(token))
						}
					},
		
					// actor}
					Some(Token::CloseCurly) => {
						array.push(key);
						break Ok(Expression::Table {array, key_value})
					},
		
					// Unexpected token.
					token => break Err(Error(token))
				}
			},

			// }
			Some(Token::CloseCurly) if first =>
				{iter.eat(); break Ok(Expression::Table {array, key_value})},

			// expr
			_ => {
				array.push(parse_expression(iter)?);

				match iter.next() {
					// expr,
					Some(Token::Comma) => continue,

					// expr}
					Some(Token::CloseCurly) =>
						break Ok(Expression::Table {array, key_value}),

					// Unexpected token.
					token => break Err(Error(token))
				}
			}
		}

		first = false;
		
	}
}

pub fn parse_if(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Statement> {
	expect!(iter.next(), Token::KeywordIf);
	let condition = parse_expression(iter)?;
	expect!(iter.next(), Token::KeywordThen);
	let then = parse_block(iter)?;

	let mut r#else = None;
	let else_ifs = from_fn(|| match iter.next() {
		Some(Token::KeywordEnd) => None,
		Some(Token::KeywordElseIf) => {
			let condition = iter_throw!(parse_expression(iter));
			iter_expect!(iter.next(), Token::KeywordThen);
			let then = iter_throw!(parse_block(iter));
			Some(Ok(ElseIf {condition, then}))
		},
		Some(Token::KeywordElse) => {
			r#else = Some(iter_throw!(parse_block(iter)));
			iter_expect!(iter.next(), Token::KeywordEnd);
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

pub fn parse_for(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Statement> {
	expect!(iter.next(), Token::KeywordFor);
	let variable = match iter.next() {
		Some(Token::Identifier(argument)) => argument,
		token => return Err(Error(token))
	};

	match iter.next() {
		// for i = 1, i < 10, 2 do
		Some(Token::Assign) => {
			let first = parse_expression(iter)?;
			expect!(iter.next(), Token::Comma);
			let limit = parse_expression(iter)?;

			let step = match iter.next() {
				Some(Token::Comma) => {
					let step = parse_expression(iter)?;
					expect!(iter.next(), Token::KeywordDo);
					step
				},
				Some(Token::KeywordDo) => Expression::Integer(1),
				token => return Err(Error(token))
			};

			let r#do = parse_block(iter)?;
			expect!(iter.next(), Token::KeywordEnd);
			Ok(Statement::NumericFor {variable, first, limit, step, r#do})
		},

		// for item in iter do
		Some(Token::KeywordIn) => {
			let iterator = parse_expression(iter)?;
			expect!(iter.next(), Token::KeywordDo);
			let r#do = parse_block(iter)?;
			expect!(iter.next(), Token::KeywordEnd);
			Ok(Statement::GenericFor {variable, iterator, r#do})
		},

		token => Err(Error(token))
	}
}

pub fn parse_while(iter: &mut TokenIterator<impl Iterator<Item = Token>>)
		-> Result<Statement> {
	Ok(match iter.next() {
		// while condition do
		Some(Token::KeywordWhile) => {
			let condition = parse_expression(iter)?;
			expect!(iter.next(), Token::KeywordDo);
			let block = parse_block(iter)?;
			expect!(iter.next(), Token::KeywordEnd);
			Statement::While {condition, block, run_first: false}
		},

		// until condition
		Some(Token::KeywordRepeat) => {
			let block = parse_block(iter)?;
			expect!(iter.next(), Token::KeywordUntil);
			let condition = parse_expression(iter)?;
			Statement::While {condition, block, run_first: true}
		},

		// Unexpected token.
		token => return Err(Error(token))
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
	let body = parse_block(iter)?;
	expect!(iter.next(), Token::KeywordEnd);

	Ok((name, arguments, body))
}

#[derive(Clone, Debug)]
pub struct Block(pub Vec<Statement>);

impl Display for Block {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		self.0.iter().try_for_each(|stmt| write!(f, "{}\n", stmt))
	}
}

#[derive(Clone, Debug)]
pub enum Statement {
	// Control

	/// An if statement.
	If {
		/// The condition for the if statement.
		condition: Expression,

		/// The block of statements to be ran if the condition is met.
		then: Block,

		/// Other conditions and statements to evaluate if the main condition wasn't
		/// met.
		else_ifs: Vec<ElseIf>,

		/// The block of statements to be ran if no other conditions were met.
		r#else: Option<Block>
	},

	/// A for in loop.
	GenericFor {
		/// The variable to set to the value of each item being iterated.
		variable: String,

		/// The expression to evaluate to get the iterator.
		iterator: Expression,

		/// The block of statements to be ran every item.
		r#do: Block
	},

	/// A numeric for loop.
	NumericFor {
		/// The variable to set the first value to.
		variable: String,

		/// The first value.
		first: Expression,

		/// The limit.
		limit: Expression,

		/// The amount to step.
		step: Expression,

		/// The block of statements to be ran every time.
		r#do: Block
	},

	/// A while or repeat loop.
	While {
		/// The condition to check.
		condition: Expression,

		/// The block of statements to be ran every time.
		block: Block,

		/// Whether or not to run block first before checking condition.
		run_first: bool
	},

	/// Returns from a function.
	Return {
		/// The values to be returned.
		values: Vec<Expression>
	},

	// El assignment

	/// An assignment operator.
	Assign {
		/// The name of the value to assign to.
		actor: Expression,

		/// The value to assign.
		value: Expression,

		/// Whether or not this should be local.
		local: bool
	},

	// Expressions

	Call {
		function: Expression,
		arguments: Vec<Expression>
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
			// Control

			Self::If {condition, then, r#else, else_ifs} => {
				write!(f, "if {} then\n{}", condition, then)?;
				else_ifs.iter().try_for_each(|ElseIf {condition, then}|
					write!(f, "elseif {} then\n{}", condition, then))?;
				if let Some(r#else) = r#else {write!(f, "else\n{}", r#else)?}
				write!(f, "end")
			},

			Self::GenericFor {variable, iterator, r#do} =>
				write!(f, "for {} in {} do\n{}end", variable, iterator, r#do),
			Self::NumericFor {variable, first, limit, step, r#do} =>
				write!(f, "for {} = {}, {}, {} do\n{}end", variable, first, limit,
					step, r#do),
			Self::While {condition, block, run_first: false} =>
				write!(f, "while {} do\n{}end", condition, block),
			Self::While {condition, block, run_first: true} =>
				write!(f, "repeat\n{}until {}", block, condition),

			Self::Return {values} => {
				write!(f, "return ")?;
				values.iter().enumerate()
					.try_for_each(|(index, value)| if index == 0 {write!(f, "{}", value)}
						else {write!(f, ", {}", value)})
			},
			
			// Assignment

			Self::Assign {actor, value, local} => if *local
				{write!(f, "local {} = {}", actor, value)} else
				{write!(f, "{} = {}", actor, value)},

			// Expressions

			Self::Call {function, arguments} => {
				write!(f, "{}(", function)?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")")
			},
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

// TODO: Clone? Why?
#[derive(Clone, Debug)]
#[warn(clippy::large_enum_variant)]
pub enum Expression {
	// La identifier

	/// A reference to a stored value, an identifier.
	Identifier(String),

	// Singleton literals

	/// The literal nil value.
	Nil,

	/// The literal boolean true value.
	True,

	/// The literal boolean false value.
	False,

	// Literals

	/// A literal integer.
	Integer(i64),

	/// A literal string.
	String(String),

	// Complicated literals

	/// A literal table.
	Table {
		/// The array expressions of this table, expressions without a denotated
		/// key. These are typically evaluated after the key denoted expressions,
		/// meaning that `{1, [1] = "A"}` evaluates to `self[1]` being 1.
		array: Vec<Expression>,

		/// The key denoted expressions of this table. Identifiers without brackets
		/// used as keys are desugared into bracketed strings.
		key_value: Vec<KeyValue>
	},

	/// A literal function.
	Function {
		/// The name of the arguments.
		arguments: Vec<String>,

		/// The statements in the table.
		body: Block
	},

	// Operations

	/// A call expression of something.
	Call {
		/// The expression of the thing being called.
		function: Box<Expression>,

		/// The provided expression arguments to be passed to the function.
		arguments: Vec<Expression>
	},

	/// An index operation.
	Index {
		/// The expression of the thing being indexed.
		indexee: Box<Expression>,

		/// The index. Period notation is desugared into bracketed strings.
		index: Box<Expression>
	},

	/// A binary operation.
	BinaryOperation {
		left: Box<Expression>,
		operator: BinaryOperator,
		right: Box<Expression>
	},

	/// A unary operation.
	UnaryOperation {
		operator: UnaryOperator,
		operand: Box<Expression>
	}
}

impl Display for Expression {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			// Identifier

			Self::Identifier(identifier) => write!(f, "{}", identifier),

			// Singleton literals

			Self::Nil => write!(f, "nil"),
			Self::True => write!(f, "true"),
			Self::False => write!(f, "false"),

			// Literals

			Self::Integer(integer) => write!(f, "{}", integer),
			Self::String(string) => write!(f, "{:?}", string),

			// Complicated literals

			Self::Table {array, key_value} => {
				write!(f, "{{")?;
				
				let mut first = true;
				let mut is_first = || {let value = first; first = false; value};
				array.iter().try_for_each(|value| if is_first() {
					write!(f, "{}", value)
				} else {
					write!(f, ", {}", value)
				})?;
				key_value.iter().try_for_each(|KeyValue {key, value}| if is_first() {
					write!(f, "[{}] = {}", key, value)
				} else {
					write!(f, ", [{}] = {}", key, value)
				})?;

				write!(f, "}})")
			},

			Self::Function {arguments, body} => {
				write!(f, "function(")?;
				arguments.iter().enumerate().try_for_each(|(index, expr)| {
					if index == 0 {write!(f, "{}", expr)} else {write!(f, ", {}", expr)}
				})?;
				write!(f, ")\n{}end", body)
			},

			// Operations

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

			Self::UnaryOperation {operator: UnaryOperator::LogicalNot, operand} =>
				write!(f, "not {}", operand),

			Self::UnaryOperation {operator, operand} =>
				write!(f, "{}{}", operator, operand)
		}
	}
}

#[derive(Clone, Debug)]
pub struct ElseIf {
	pub condition: Expression,
	pub then: Block
}

#[derive(Clone, Debug)]
pub struct KeyValue {
	pub key: Expression,
	pub value: Expression
}

// TODO: Should we remove [crate::vm::BinaryOperation] and use this instead?
// Same goes for UnaryOperator and Operation.
#[derive(Clone, Copy, Debug)]
pub enum BinaryOperator {
	// Arithmetic
	Add,
	Subtract,
	Multiply,
	Divide,
	FloorDivide,
	Modulo,
	Exponent,

	// Bitwise
	BitwiseAnd,
	BitwiseOr,
	BitwiseXOr,
	ShiftLeft,
	ShiftRight,

	// Relational
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,

	// Logical
	LogicalAnd,
	LogicalOr,

	// Other
	Concat
}

impl Display for BinaryOperator {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			// Arithmetic
			Self::Add => write!(f, "+"),
			Self::Subtract => write!(f, "-"),
			Self::Multiply => write!(f, "*"),
			Self::Divide => write!(f, "/"),
			Self::FloorDivide => write!(f, "//"),
			Self::Modulo => write!(f, "%"),
			Self::Exponent => write!(f, "^"),

			// Bitwise
			Self::BitwiseAnd => write!(f, "&"),
			Self::BitwiseOr => write!(f, "|"),
			Self::BitwiseXOr => write!(f, "~"),
			Self::ShiftLeft => write!(f, "<<"),
			Self::ShiftRight => write!(f, ">>"),

			// Relational
			Self::Equal => write!(f, "=="),
			Self::NotEqual => write!(f, "~="),
			Self::LessThan => write!(f, "<"),
			Self::LessThanOrEqual => write!(f, "<="),
			Self::GreaterThan => write!(f, ">"),
			Self::GreaterThanOrEqual => write!(f, ">="),

			// Logical
			Self::LogicalAnd => write!(f, "and"),
			Self::LogicalOr => write!(f, "or"),

			// Other
			Self::Concat => write!(f, "..")
		}
	}
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperator {
	// Arithmetic
	Negate,

	// Bitwise
	BitwiseNot,

	// Logical
	LogicalNot,

	// Other
	Length
}

impl Display for UnaryOperator {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			// Arithmetic
			Self::Negate => write!(f, "-"),

			// Bitwise
			Self::BitwiseNot => write!(f, "~"),

			// Logical
			Self::LogicalNot => write!(f, "not"),

			// Other
			Self::Length => write!(f, "#")
		}
	}
}
