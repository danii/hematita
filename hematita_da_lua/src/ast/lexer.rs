use std::iter::Peekable;

/// Tokenizes a lua text file, character by character.
pub struct Lexer<T>
		where T: Iterator<Item = char> {
	pub source: Peekable<T>
}

impl<T> Lexer<T>
		where T: Iterator<Item = char> {
	/// Eats a character, disposing of it.
	fn eat(&mut self) {
		self.peeked_next();
	}

	/// Returns the next character, if any.
	/// Shortcut for accessing source with the same method.
	fn next(&mut self) -> Option<char> {
		self.source.next()
	}

	/// Returns the next character assuming, assuming that the character was
	/// already peeked, and did infact, exist.
	fn peeked_next(&mut self) -> char {
		self.next().unwrap()
	}

	/// Peeks the next character, if any.
	/// Shortcut for accessing source with the same method.
	fn peek(&mut self) -> Option<char> {
		self.source.peek().map(Clone::clone)
	}

	/// Parses through whitespace and comments, discarding it all, and returns
	/// the last peeked non whitespace character.
	fn parse_whitespace(&mut self) -> Option<char> {
		loop {
			match self.peek()? {
				' ' | '\n' | '\t' => self.eat(),
				character => break Some(character)
			}
		}
	}

	/// Parses an identifier into a token.
	fn parse_identifier(&mut self) -> Option<Token> {
		let mut identifier = String::new();

		loop {
			match self.peek() {
				Some('a'..='z' | 'A'..='Z') => identifier.push(self.peeked_next()),
				_ => break
			}
		}

		Some(match &identifier as &str {
			"and" => Token::KeywordAnd,
			"true" => Token::LiteralTrue,
			"false" => Token::LiteralFalse,
			"nil" => Token::LiteralNil,
			"break" => Token::KeywordBreak,
			"do" => Token::KeywordDo,
			"else" => Token::KeywordElse,
			"elseif" => Token::KeywordElseIf,
			"end" => Token::KeywordEnd,
			"for" => Token::KeywordFor,
			"function" => Token::KeywordFunction,
			"goto" => Token::KeywordGoto,
			"if" => Token::KeywordIf,
			"in" => Token::KeywordIn,
			"local" => Token::KeywordLocal,
			"not" => Token::KeywordNot,
			"or" => Token::KeywordOr,
			"repeat" => Token::KeywordRepeat,
			"return" => Token::KeywordReturn,
			"then" => Token::KeywordThen,
			"until" => Token::KeywordUntil,
			"while" => Token::KeywordWhile,
			_ => Token::Identifier(identifier)
		})
	}

	/// Parses a string. Assumes the first quote character was not consumed.
	fn parse_string(&mut self) -> Option<Token> {
		let delimiter = self.peeked_next();
		let mut string = String::new();

		loop {
			match self.peek()? {
				'\\' => match {self.eat(); self.peek()?} {
					'a' => {self.eat(); string.push('\x07')},
					'b' => {self.eat(); string.push('\x08')},
					'f' => {self.eat(); string.push('\x0C')},
					'n' => {self.eat(); string.push('\n')},
					'r' => {self.eat(); string.push('\r')},
					't' => {self.eat(); string.push('\t')},
					'v' => {self.eat(); string.push('A')},
					'\\' => {self.eat(); string.push('\x0B')},
					'"' => {self.eat(); string.push('\"')},
					'\'' => {self.eat(); string.push('\'')},
					'[' => {self.eat(); string.push('[')},
					']' => {self.eat(); string.push(']')},
					_ => todo!()
				},
				character if character == delimiter =>
					{self.eat(); break Some(Token::String(string))},
				_ => string.push(self.peeked_next())
			}
		}
	}

	/// Parses a number, or, potentially, the subtraction operator.
	fn parse_number(&mut self) -> Option<Token> {
		let mut number = String::new();
		
		loop {
			match self.peek() {
				// TODO: - is impossible now.
				Some('-') => if number.len() == 0 {
					number.push(self.peeked_next())
				} else {
					todo!()
				},
				Some('0'..='9') => number.push(self.peeked_next()),
				_ => break
			}
		}

		Some(if number == "-" {
			Token::Minus
		} else {
			// TODO: Handle unwrap!
			Token::Integer(number.parse().unwrap())
		})
	}
}

impl<T> Iterator for Lexer<T> where T: Iterator<Item = char> {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		Some(match self.parse_whitespace()? {
			// TODO: Error handling on complex cases.
			// Complex

			// Single character token Other Assign (=)
			// OR Double character token Relational Equal (==)
			'=' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::Equal},
				_ => Token::Assign
			},

			// Single character token Relational LessThan (<)
			// OR Double character token Relational LessThanOrEqual (<=)
			// OR Double character token Bitwise ShiftLeft (<<)
			'<' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::LessThanOrEqual},
				'<' => {self.eat(); Token::ShiftLeft},
				_ => Token::LessThan
			},

			// Single character token Relational GreaterThan (>)
			// OR Double character token Relational GreaterThanOrEqual (>=)
			// OR Double character token Bitwise ShiftRight (>>)
			'>' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::GreaterThanOrEqual},
				'>' => {self.eat(); Token::ShiftRight},
				_ => Token::GreaterThan
			},

			// Single character token Bitwise BitwiseNotOrXOr (~)
			// OR Double character token Relational NotEqual (~=)
			'~' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::NotEqual},
				_ => Token::BitwiseNotOrXOr
			},

			// Single character token Arithmetic Divide (/)
			// OR Double character token Arithmetic FloorDivide (//)
			'/' => match {self.eat(); self.peek().unwrap()} {
				'/' => {self.eat(); Token::FloorDivide},
				_ => Token::FloorDivide
			},

			// Single character token Other Period (.)
			// OR Double character token Other Concat (..)
			//   OR Triple character token Other VarArgs (...) // TODO
			'.' => match {self.eat(); self.peek().unwrap()} {
				'.' => {self.eat(); Token::Concat},
				_ => Token::Period
			},

			// Arithmetic
			'+' => {self.eat(); Token::Add},
			'-' => {self.eat(); Token::Minus},
			'*' => {self.eat(); Token::Multiply},
			// Divide and FloorDivide are complex tokens
			'%' => {self.eat(); Token::Modulo},
			'^' => {self.eat(); Token::Exponent},

			// Bitwise
			'&' => {self.eat(); Token::BitwiseAnd},
			'|' => {self.eat(); Token::BitwiseOr},
			// BitwiseNotOrXOr, ShiftLeft and ShiftRight are complex tokens

			// Relational
			// All relationals are complex tokens

			// Other
			// Other Assign is a complex token
			':' => {self.eat(); Token::Colon},
			',' => {self.eat(); Token::Comma},
			// Other Period is a complex token
			';' => {self.eat(); Token::SemiColon},
			// Other Concat is a complex token
			'#' => {self.eat(); Token::Length},

			// Sectioning
			'(' => {self.eat(); Token::OpenParen},
			')' => {self.eat(); Token::CloseParen},
			'{' => {self.eat(); Token::OpenCurly},
			'}' => {self.eat(); Token::CloseCurly},
			'[' => {self.eat(); Token::OpenBracket},
			']' => {self.eat(); Token::CloseBracket},

			// Literals
			'"' => self.parse_string().unwrap(), // Double quoted strings
			'\'' => self.parse_string().unwrap(), // Single quoted strings
			'0'..='9' => self.parse_number().unwrap(), // Numbers
			_ => self.parse_identifier().unwrap() // Most other literals
		})
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
	// Literals
	Identifier(String),
	Integer(i64),
	String(String),

	// Literal Values
	LiteralTrue,
	LiteralFalse,
	LiteralNil,

	// Arithmetic
	Add,
	Minus,
	Multiply,
	Divide,
	FloorDivide,
	Modulo,
	Exponent,

	// Bitwise
	BitwiseAnd,
	BitwiseOr,
	BitwiseNotOrXOr,
	ShiftLeft,
	ShiftRight,

	// Relational
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,

	// Other
	Assign,
	Colon,
	Comma,
	Period,
	SemiColon,
	Concat,
	Length,

	// Sectioning
	OpenParen,
	CloseParen,
	OpenCurly,
	CloseCurly,
	OpenBracket,
	CloseBracket,

	// Keywords
	KeywordAnd,
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
	KeywordNot,
	KeywordOr,
	KeywordRepeat,
	KeywordReturn,
	KeywordThen,
	KeywordUntil,
	KeywordWhile
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			// Literals
			Self::Identifier(identifier) => write!(f, "{}", identifier),
			Self::Integer(integer) => write!(f, "{}", integer),
			Self::String(string) => write!(f, "{:?}", string),

			// Literal Values
			Self::LiteralTrue => write!(f, "true"),
			Self::LiteralFalse => write!(f, "false"),
			Self::LiteralNil => write!(f, "nil"),

			// Arithmetic
			Self::Add => write!(f, "+"),
			Self::Minus => write!(f, "-"),
			Self::Multiply => write!(f, "*"),
			Self::Divide => write!(f, "/"),
			Self::FloorDivide => write!(f, "//"),
			Self::Modulo => write!(f, "%"),
			Self::Exponent => write!(f, "^"),

			// Bitwise
			Self::BitwiseAnd => write!(f, "&"),
			Self::BitwiseOr => write!(f, "|"),
			Self::BitwiseNotOrXOr => write!(f, "~"),
			Self::ShiftLeft => write!(f, "<<"),
			Self::ShiftRight => write!(f, ">>"),

			// Relational
			Self::Equal => write!(f, "=="),
			Self::NotEqual => write!(f, "~="),
			Self::LessThan => write!(f, "<"),
			Self::LessThanOrEqual => write!(f, "<="),
			Self::GreaterThan => write!(f, ">"),
			Self::GreaterThanOrEqual => write!(f, ">="),

			// Other
			Self::Assign => write!(f, "="),
			Self::Colon => write!(f, ":"),
			Self::Comma => write!(f, ","),
			Self::Period => write!(f, "."),
			Self::SemiColon => write!(f, ";"),
			Self::Concat => write!(f, ".."),
			Self::Length => write!(f, "#"),

			// Sectioning
			Self::OpenParen => write!(f, "("),
			Self::CloseParen => write!(f, ")"),
			Self::OpenCurly => write!(f, "{{"),
			Self::CloseCurly => write!(f, "}}"),
			Self::OpenBracket => write!(f, "["),
			Self::CloseBracket => write!(f, "]"),

			// Keywords
			Self::KeywordAnd => write!(f, "and"),
			Self::KeywordBreak => write!(f, "break"),
			Self::KeywordDo => write!(f, "do"),
			Self::KeywordElse => write!(f, "else"),
			Self::KeywordElseIf => write!(f, "elseif"),
			Self::KeywordEnd => write!(f, "end"),
			Self::KeywordFor => write!(f, "for"),
			Self::KeywordFunction => write!(f, "function"),
			Self::KeywordGoto => write!(f, "goto"),
			Self::KeywordIf => write!(f, "if"),
			Self::KeywordIn => write!(f, "in"),
			Self::KeywordLocal => write!(f, "local"),
			Self::KeywordNot => write!(f, "not"),
			Self::KeywordOr => write!(f, "or"),
			Self::KeywordRepeat => write!(f, "repeat"),
			Self::KeywordReturn => write!(f, "return"),
			Self::KeywordThen => write!(f, "then"),
			Self::KeywordUntil => write!(f, "until"),
			Self::KeywordWhile => write!(f, "while")
		}
	}
}
