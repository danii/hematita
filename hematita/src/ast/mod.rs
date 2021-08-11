pub mod lexer;
pub mod parser;
#[cfg(test)]
pub mod tests;

use self::{lexer::Error as LexerError, parser::Error as ParserError};
use std::{
	error::Error as STDError,
	fmt::{Display, Formatter, Result as FMTResult},
	result::Result as STDResult
};

pub type Result<T> = STDResult<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
	Lexer(LexerError),
	Parser(ParserError)
}

impl STDError for Error {}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Lexer(lexer) => lexer.fmt(f),
			Self::Parser(parser) => parser.fmt(f)
		}
	}
}

impl From<LexerError> for Error {
	fn from(lexer: LexerError) -> Self {
		Self::Lexer(lexer)
	}
}

impl From<ParserError> for Error {
	fn from(parser: ParserError) -> Self {
		Self::Parser(parser)
	}
}
