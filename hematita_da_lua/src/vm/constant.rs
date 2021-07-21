use self::super::Chunk;
use std::{fmt::{Display, Formatter, Result as FMTResult}, sync::Arc};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Constant {
	String(String),
	Integer(i64),
	Boolean(bool),
	Chunk(Arc<Chunk>)
}

impl Display for Constant {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::String(string) => write!(f, "{:?}", string),
			Self::Integer(integer) => write!(f, "{}", integer),
			Self::Boolean(boolean) => write!(f, "{}", boolean),
			Self::Chunk(chunk) => write!(f, "{}", chunk)
		}
	}
}

/// Data with a known type and value at compile time.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum KnownValue {
	String(String),
	Integer(i64),
	Boolean(bool),
	Nil
}

impl KnownValue {
	pub fn coerce_to_bool(&self) -> bool {
		match self {
			Self::Boolean(value) => *value,
			Self::Nil => false,
			_ => true
		}
	}
}

impl From<i64> for KnownValue {
	fn from(integer: i64) -> Self {
		Self::Integer(integer)
	}
}
