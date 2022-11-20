use std::{
	error::Error as STDError,
	fmt::{Display, Formatter, Result as FMTResult},
	iter::Peekable,
	result::Result as STDResult
};

pub type Result<T> = STDResult<T, Error>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Error {
	UnexpectedCharacter(char),
	IllegalEscapeCode(char),
	NumberTooLarge(Box<str>)
}

impl STDError for Error {}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> FMTResult {
		match self {
			Self::UnexpectedCharacter(character) =>
				write!(f, "unexpected symbol {:?}", character),
			Self::IllegalEscapeCode(character) =>
				write!(f, "invalid escape sequence '\\{}'", character),
			Self::NumberTooLarge(number) =>
				write!(f, "number too large {}", number)
		}
	}
}

/// Tokenizes a Lua text file, character by character.
///
/// The Lexer wraps some [`Iterator`] of [`char`]s, T, and can be iterated over,
/// producing [`Token`]s.
///
/// Examples
/// --------
/// Below is an example of parsing one line of Lua.
/// ```rust
/// # use hematita::ast::lexer::{Lexer, Token};
/// let mut lexer = Lexer {source: "print('hello world!')".chars().peekable()};
///
/// assert_eq!(lexer.next(), Some(Ok(Token::Identifier("print".to_owned()))));
/// assert_eq!(lexer.next(), Some(Ok(Token::OpenParen)));
/// assert_eq!(lexer.next(), Some(Ok(Token::String("hello world!".to_owned()))));
/// assert_eq!(lexer.next(), Some(Ok(Token::CloseParen)));
/// assert_eq!(lexer.next(), None);
/// ```
pub struct Lexer<T>
		where T: Iterator<Item = char> {
	pub source: Peekable<T>
}

/// The private Lexer API.
///
/// Includes many convience methods for internal implementation.
impl<T> Lexer<T>
		where T: Iterator<Item = char> {
	/// Eats a character, disposing of it.
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// # fn allow_return() -> Result<Token, ()> {
	/// # 	let mut lexer = Lexer {source: "(".chars().peekable()};
	/// // The expected type of this match statement is a unit. Calling
	/// // lexer.next() would do the same thing, but it would conflict with the
	/// // expected type of the match statement.
	/// match lexer.peek() {
	/// 	'(' => lexer.eat(),
	/// 	_ => return Err(())
	/// }
	///
	/// return Ok(Token::OpenParen)
	/// # }
	/// ```
	pub(crate) fn eat(&mut self) {
		self.peeked_next();
	}

	/// Returns the next character, if any. Shortcut for calling next on
	/// [`source`](Self::source).
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "hello".chars().peekable()};
	///
	/// assert_eq!(lexer.next(), Some('h'));
	/// assert_eq!(lexer.source.next(), Some('e'));
	/// ```
	#[must_use = "all characters should be consumed, if you already peeked this, you should use `eat`"]
	pub(crate) fn next(&mut self) -> Option<char> {
		self.source.next()
	}

	/// Returns the next character, assuming that the character was already
	/// peeked, and did infact, exist.
	///
	/// Examples
	/// --------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// # use std::mem::drop as consume;
	/// let mut lexer = Lexer {source: "hi".chars().peekable()};
	///
	/// match lexer.peek() {
	/// 	Some('h') => consume(lexer.peeked_next()),
	/// 	_ => ()
	/// }
	/// ```
	///
	/// Below is an example of misuse of this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```should_panic,ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// # use std::mem::drop as consume;
	/// let mut lexer = Lexer {source: "".chars().peekable()};
	///
	/// match lexer.peek() {
	/// 	None => consume(lexer.peeked_next()), // PANIC!
	/// 	_ => ()
	/// }
	/// ```
	pub(crate) fn peeked_next(&mut self) -> char {
		match self.next() {
			Some(next) => next,
			None => unreachable!("called peeked_next when there wasn't anything next")
		}
	}

	/// Peeks the next character, if any. Shortcut for calling peek on
	/// [`source`](Self::source), and then copying the result.
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "hello".chars().peekable()};
	///
	/// assert_eq!(lexer.peek(), Some('h'));
	/// assert_eq!(lexer.source.peek(), Some(&'h'));
	/// assert_eq!(lexer.next(), Some('h'));
	/// assert_eq!(lexer.peek(), Some('e'));
	/// ```
	pub(crate) fn peek(&mut self) -> Option<char> {
		self.source.peek().map(Clone::clone)
	}

	/// Parses and discards all whitespace, and returns the last peeked non
	/// whitespace character.
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "a b    cd".chars().peekable()};
	///
	/// assert_eq!(iter.parse_whitespace(), Some('a'));
	/// iter.eat();
	/// assert_eq!(iter.parse_whitespace(), Some('b'));
	/// iter.eat();
	/// assert_eq!(iter.parse_whitespace(), Some('c'));
	/// iter.eat();
	/// assert_eq!(iter.parse_whitespace(), Some('d'));
	/// ```
	pub(crate) fn parse_whitespace(&mut self) -> Option<char> {
		loop {
			match self.peek()? {
				' ' | '\n' | '\r' | '\t' => self.eat(),
				character => break Some(character)
			}
		}
	}

	/// Parses an identifier into a token.
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "hi if end ok".chars().peekable()};
	///
	/// assert_eq!(lexer.parse_identifier(),
	/// 	Some(Token::Identifier("hi".to_string())));
	/// lexer.parse_whitespace();
	/// assert_eq!(lexer.parse_identifier(), Some(Token::KeywordIf));
	/// lexer.parse_whitespace();
	/// assert_eq!(lexer.parse_identifier(), Some(Token::KeywordEnd));
	/// lexer.parse_whitespace();
	/// assert_eq!(lexer.parse_identifier(),
	/// 	Some(Token::Identifier("ok".to_string())));
	/// ```
	pub(crate) fn parse_identifier(&mut self) -> Token {
		let mut identifier = String::new();

		while let Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_') = self.peek()
			{identifier.push(self.peeked_next())}

		match &identifier as &str {
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
		}
	}

	/// Parses a string into a token. Assumes the first quote character *was not*
	/// consumed.
	///
	/// Example
	/// -------
	/// Below is the typical use case for this function.
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "'hi\\n'".chars().peekable()};
	///
	/// assert_eq!(lexer.parse_string(),
	/// 	Some(Token::String("hi\n".to_owned())));
	/// ```
	pub(crate) fn parse_string(&mut self) -> Option<Result<Token>> {
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
					'v' => {self.eat(); string.push('\x0B')},
					'\\' => {self.eat(); string.push('\\')},
					'"' => {self.eat(); string.push('"')},
					'\'' => {self.eat(); string.push('\'')},
					'[' => {self.eat(); string.push('[')},
					']' => {self.eat(); string.push(']')},
					character => break Some(Err(Error::IllegalEscapeCode(character)))
				},
				character if character == delimiter =>
					{self.eat(); break Some(Ok(Token::String(string)))},
				_ => string.push(self.peeked_next())
			}
		}
	}

	/// Parses a bracketed string into a token. Assumes the first character was a
	/// `[`, and *was* consumed.
	///
	/// Example
	/// -------
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "[[hi\\n]]".chars().peekable()};
	///
	/// assert_eq!(lexer.next(), Some('['));
	/// assert_eq!(lexer.parse_bracketed_string(),
	/// 	Some(Some(Token::String("hi\\n".to_owned()))));
	/// ```
	pub(crate) fn parse_bracketed_string(&mut self) -> Option<Token> {
		self.parse_bracketed().map(Token::String)
	}

	/// Parses a number into a token. Assumes the first character *was not*
	/// consumed.
	///
	/// Example
	/// -------
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "123".chars().peekable()};
	///
	/// assert_eq!(lexer.parse_number(), Some(Token::Integer(123)));
	/// ```
	pub(crate) fn parse_number(&mut self) -> Result<Token> {
		let mut number = String::new();
		while let Some('0'..='9') = self.peek()
			{number.push(self.peeked_next())}
		number.parse().map(Token::Integer)
			.map_err(|_| Error::NumberTooLarge(number.into_boxed_str()))
	}

	/// Parses a comment into a token. Assumes the first characters were `--`,
	/// and *were* consumed.
	///
	/// Example
	/// -------
	//  FIXME: https://github.com/rust-lang/rust/issues/67295
	/// ```ignore
	/// # use hematita::ast::lexer::{Lexer, Token};
	/// let mut lexer = Lexer {source: "-- comment!".chars().peekable()};
	///
	/// assert_eq!(lexer.next(), Some('-'));
	/// assert_eq!(lexer.next(), Some('-'));
	/// assert_eq!(lexer.parse_comment(),
	/// 	Some(Token::Comment(" comment!".to_owned())));
	/// ```
	pub(crate) fn parse_comment(&mut self) -> Option<Token> {
		match self.peek()? {
			'[' => self.parse_bracketed().map(Token::Comment),
			_ => {
				let mut comment = String::new();
				loop {
					match self.peek()? {
						'\n' => {self.eat(); break Some(Token::Comment(comment))},
						_ => comment.push(self.peeked_next())
					}
				}
			}
		}
	}

	/// Parses some bracketed item. Assumes the first character was a `[`, and
	/// *was* consumed.
	///
	/// This is internally used by both [`parse_bracketed_string`][string] and
	/// [`parse_comment`][comment]. For an example, see `parse_bracketed_string`'s
	/// examples.
	///
	/// [string]: Self::parse_bracketed_string
	/// [comment]: Self::parse_comment
	pub(crate) fn parse_bracketed(&mut self) -> Option<String> {
		let mut string = String::new();
		let length = {
			let mut length = 0usize;
			loop {
				match self.peek()? {
					'=' => {self.eat(); length += 1},
					'[' => {self.eat(); break length},
					_ => return None
				}
			}
		};

		let mut first = true;
		loop {
			match self.peek()? {
				']' => {
					string.push(self.peeked_next());
					let mut end_length = length;
					if loop {
						match self.peek()? {
							'=' => {
								string.push(self.peeked_next());
								match end_length.checked_sub(1) {
									Some(new) => end_length = new,
									None => break false
								}
							},
							']' => {
								string.push(self.peeked_next());
								if end_length == 0 {break true}
							},
							_ => {
								string.push(self.peeked_next());
								break false
							}
						}
					} {
						string.truncate(string.len() - length - 2);
						break Some(string)
					}
				},
				'\n' if first => self.eat(),
				_ => string.push(self.peeked_next())
			}

			first = false;
		}
	}
}

/// The main interface to the Lexer.
impl<T> Iterator for Lexer<T>
		where T: Iterator<Item = char> {
	type Item = Result<Token>;

	/// Parses a single token and returns it.
	///
	/// After all tokens have been parsed and the underlying character iterator
	/// is exhausted, *or* an error occurs, this will return `None` from
	/// thenforth.
	fn next(&mut self) -> Option<Result<Token>> {
		match self.parse_whitespace()? {
			// TODO: Error handling on complex cases.
			// Complex

			// Single character token Minus (-)
			// OR Multiple character Comment (--[[]])
			'-' => match {self.eat(); self.peek()} {
				Some('-') => {self.eat(); self.parse_comment().map(Ok)},
				_ => Some(Ok(Token::Minus))
			},

			// Single character token OpenBracket ([)
			// OR Multiple character token String ([[]])
			'[' => match {self.eat(); self.peek()} {
				Some('=' | '[') => self.parse_bracketed_string().map(Ok),
				_ => Some(Ok(Token::OpenBracket))
			},

			// Single character token Other Assign (=)
			// OR Double character token Relational Equal (==)
			'=' => match {self.eat(); self.peek()} {
				Some('=') => {self.eat(); Some(Ok(Token::Equal))},
				_ => Some(Ok(Token::Assign))
			},

			// Single character token Relational LessThan (<)
			// OR Double character token Relational LessThanOrEqual (<=)
			// OR Double character token Bitwise ShiftLeft (<<)
			'<' => match {self.eat(); self.peek()} {
				Some('=') => {self.eat(); Some(Ok(Token::LessThanOrEqual))},
				Some('<') => {self.eat(); Some(Ok(Token::ShiftLeft))},
				_ => Some(Ok(Token::LessThan))
			},

			// Single character token Relational GreaterThan (>)
			// OR Double character token Relational GreaterThanOrEqual (>=)
			// OR Double character token Bitwise ShiftRight (>>)
			'>' => match {self.eat(); self.peek()} {
				Some('=') => {self.eat(); Some(Ok(Token::GreaterThanOrEqual))},
				Some('>') => {self.eat(); Some(Ok(Token::ShiftRight))},
				_ => Some(Ok(Token::GreaterThan))
			},

			// Single character token Bitwise BitwiseNotOrXOr (~)
			// OR Double character token Relational NotEqual (~=)
			'~' => match {self.eat(); self.peek()} {
				Some('=') => {self.eat(); Some(Ok(Token::NotEqual))},
				_ => Some(Ok(Token::BitwiseNotOrXOr))
			},

			// Single character token Arithmetic Divide (/)
			// OR Double character token Arithmetic FloorDivide (//)
			'/' => match {self.eat(); self.peek()} {
				Some('/') => {self.eat(); Some(Ok(Token::FloorDivide))},
				_ => Some(Ok(Token::FloorDivide))
			},

			// Single character token Other Period (.)
			// OR Double character token Other Concat (..)
			//   OR Triple character token Other VarArgs (...) // TODO
			'.' => match {self.eat(); self.peek()} {
				Some('.') => {self.eat(); Some(Ok(Token::Concat))},
				_ => Some(Ok(Token::Period))
			},

			// Arithmetic
			'+' => {self.eat(); Some(Ok(Token::Add))},
			// Minus is a complex token
			'*' => {self.eat(); Some(Ok(Token::Multiply))},
			// Divide and FloorDivide are complex tokens
			'%' => {self.eat(); Some(Ok(Token::Modulo))},
			'^' => {self.eat(); Some(Ok(Token::Exponent))},

			// Bitwise
			'&' => {self.eat(); Some(Ok(Token::BitwiseAnd))},
			'|' => {self.eat(); Some(Ok(Token::BitwiseOr))},
			// BitwiseNotOrXOr, ShiftLeft and ShiftRight are complex tokens

			// Relational
			// All relationals are complex tokens

			// Other
			// Other Assign is a complex token
			':' => {self.eat(); Some(Ok(Token::Colon))},
			',' => {self.eat(); Some(Ok(Token::Comma))},
			// Other Period is a complex token
			';' => {self.eat(); Some(Ok(Token::SemiColon))},
			// Other Concat is a complex token
			'#' => {self.eat(); Some(Ok(Token::Length))},

			// Sectioning
			'(' => {self.eat(); Some(Ok(Token::OpenParen))},
			')' => {self.eat(); Some(Ok(Token::CloseParen))},
			'{' => {self.eat(); Some(Ok(Token::OpenCurly))},
			'}' => {self.eat(); Some(Ok(Token::CloseCurly))},
			// Sectioning OpenBracket is a complex token
			']' => {self.eat(); Some(Ok(Token::CloseBracket))},

			// Literals
			'"' => self.parse_string(), // Double quoted strings
			'\'' => self.parse_string(), // Single quoted strings
			'0'..='9' => Some(self.parse_number()), // Numbers
			// Most other literals
			'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.parse_identifier())),

			character => Some(Err(Error::UnexpectedCharacter(character)))
		}
	}

	/// Returns the bounds on the remaining length of the lexer.
	///
	/// There could be as little as zero more tokens, or as many as the number of
	/// characters in the underlying character iterator.
	fn size_hint(&self) -> (usize, Option<usize>) {
		(0, self.source.size_hint().1)
	}
}

/// A single syntactical unit of Lua code.
///
/// Represents identifiers, punctuation, and everythign in between.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
	// Comment

	/// A Lua comment. May or may not include new lines.
	///
	/// Example
	/// -------
	/// ```lua
	/// -- Hello world!
	/// --[[
	/// 	I am a multiline comment,
	/// 	and you are a cutie.
	/// ]]
	/// ```
	Comment(String),

	// Literals

	/// An identifier, excluding keywords.
	///
	/// Example
	/// -------
	/// Valid Identifiers
	/// - `whatLanguageDo`
	/// - `you_like_better`
	/// - `__luaOr_rust12345`
	///
	/// Invalid Identifiers
	/// - `69imMatureIPromise` - Cannot start with a digit
	/// - `what_am!i_supposed` - Includes a non word character
	/// - `toWriteHere我不知道` - Includes non latin characters
	Identifier(String),

	/// An integer.
	Integer(i64),

	/// A string.
	///
	/// Example
	/// -------
	/// Valid Strings
	/// - `"\tPerhaps, I should reference a song?\n"`
	/// - `'All my life I\'ve drowned in adrenaline\nNow my blood runs slow like a
	/// 	seditive'`
	/// - `[[Good song? No? Damn. :( \t\y\e\x\\stuff\vr"""')]]`
	/// - `[===[[[ ]]  ]=====] OH BTW THOSE LYRICS ARE NOT LICENSED]===]`
	///
	/// Invalid Strings
	/// - `"I could just write what the error is in here'` - Mismatched quotation
	/// - `'but instead i choose to write this \y'` - Invalid escape sequence
	/// - `[===[everybody asks what the coder doin`<br>
	/// 	`but nobody asks how is the coder doin]]` - Mismatched braces
	String(String),

	// Literal Values

	/// The `true` keyword.
	LiteralTrue,

	/// The `false` keyword.
	LiteralFalse,

	/// The `nil` keyword.
	LiteralNil,

	// Arithmetic

	/// The `+` symbol.
	Add,

	/// The `-` symbol.
	Minus,

	/// The `*` symbol.
	Multiply,

	/// The `/` symbol.
	Divide,

	/// The `//` symbol.
	FloorDivide,

	/// The `%` symbol.
	Modulo,

	/// The `^` symbol.
	Exponent,

	// Bitwise

	/// The `&` symbol.
	BitwiseAnd,

	/// The `|` symbol.
	BitwiseOr,

	/// The `~` symbol.
	BitwiseNotOrXOr,

	/// The `<<` symbol.
	ShiftLeft,

	/// The `>>` symbol.
	ShiftRight,

	// Relational

	/// The `==` symbol.
	Equal,

	/// The `~=` symbol.
	NotEqual,

	/// The `<` symbol.
	LessThan,

	/// The `<=` symbol.
	LessThanOrEqual,

	/// The `>` symbol.
	GreaterThan,

	/// The `>=` symbol.
	GreaterThanOrEqual,

	// Other

	/// The `=` symbol.
	Assign,

	/// The `:` symbol.
	Colon,

	/// The `,` symbol.
	Comma,

	/// The `.` symbol.
	Period,

	/// The `;` symbol.
	SemiColon,

	/// The `..` symbol.
	Concat,

	/// The `#` symbol.
	Length,

	// Sectioning

	/// The `(` symbol.
	OpenParen,

	/// The `)` symbol.
	CloseParen,

	/// The `{` symbol.
	OpenCurly,

	/// The `}` symbol.
	CloseCurly,

	/// The `[` symbol.
	OpenBracket,

	/// The `]` symbol.
	CloseBracket,

	// Keywords

	/// The `and` keyword.
	KeywordAnd,

	/// The `break` keyword.
	KeywordBreak,

	/// The `do` keyword.
	KeywordDo,

	/// The `else` keyword.
	KeywordElse,

	/// The `elseif` keyword.
	KeywordElseIf,

	/// The `end` keyword.
	KeywordEnd,

	/// The `for` keyword.
	KeywordFor,

	/// The `function` keyword.
	KeywordFunction,

	/// The `goto` keyword.
	KeywordGoto,

	/// The `if` keyword.
	KeywordIf,

	/// The `in` keyword.
	KeywordIn,

	/// The `local` keyword.
	KeywordLocal,

	/// The `not` keyword.
	KeywordNot,

	/// The `or` keyword.
	KeywordOr,

	/// The `repeat` keyword.
	KeywordRepeat,

	/// The `return` keyword.
	KeywordReturn,

	/// The `then` keyword.
	KeywordThen,

	/// The `until` keyword.
	KeywordUntil,

	/// The `while` keyword.
	KeywordWhile
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			// Comment
			Self::Comment(comment) => write!(f, "--[===[{}]===]", comment),

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
