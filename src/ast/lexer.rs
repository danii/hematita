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
			match self.peek()? {
				'a'..='z' | 'A'..='Z' => identifier.push(self.peeked_next()),
				_ => break
			}
		}

		Some(match &identifier as &str {
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
			"repeat" => Token::KeywordRepeat,
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
			match self.peek()? {
				'-' => if number.len() == 0 {
					number.push(self.peeked_next())
				} else {
					todo!()
				},
				'0'..='9' => number.push(self.peeked_next()),
				_ => break
			}
		}

		Some(if number == "-" {
			Token::Subtract
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
			'=' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::Equal},
				_ => Token::Assign
			},

			'<' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::LessThanOrEqual},
				_ => Token::LessThan
			},

			'>' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::GreaterThanOrEqual},
				_ => Token::GreaterThan
			},

			'~' => match {self.eat(); self.peek().unwrap()} {
				'=' => {self.eat(); Token::NotEqual},
				_ => todo!()
			},

			'+' => {self.eat(); Token::Add},
			'-' | '0'..='9' => self.parse_number().unwrap(),

			'.' => match {self.eat(); self.peek().unwrap()} {
				'.' => {self.eat(); Token::Concat},
				_ => Token::Period
			},

			':' => {self.eat(); Token::Colon},
			';' => {self.eat(); Token::SemiColon},
			'(' => {self.eat(); Token::OpenParen},
			')' => {self.eat(); Token::CloseParen},
			'{' => {self.eat(); Token::OpenCurly},
			'}' => {self.eat(); Token::CloseCurly},
			'[' => {self.eat(); Token::OpenBracket},
			']' => {self.eat(); Token::CloseBracket},
			',' => {self.eat(); Token::Comma},
			'"' => self.parse_string().unwrap(),
			'\'' => self.parse_string().unwrap(),
			'\\' => todo!(),
			_ => self.parse_identifier().unwrap()
		})
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
	// Literals
	Identifier(String),
	Integer(i64),
	String(String),

	// Punctuation
	Assign,
	Colon,
	Comma,
	Period,
	SemiColon,
	Equal,
	NotEqual,
	LessThan,
	LessThanOrEqual,
	GreaterThan,
	GreaterThanOrEqual,
	Add,
	Subtract,
	Concat,
	OpenParen,
	CloseParen,
	OpenCurly,
	CloseCurly,
	OpenBracket,
	CloseBracket,

	// Literal Values
	LiteralTrue,
	LiteralFalse,
	LiteralNil,

	// Keywords
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
