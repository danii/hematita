use self::super::Chunk;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Constant {
	String(String),
	Integer(i64),
	Boolean(bool),
	Chunk(Arc<Chunk>)
}

impl From<KnownValue> for Constant {
	fn from(known: KnownValue) -> Self {
		match known {
			KnownValue::String(string) => Self::String(string),
			KnownValue::Integer(integer) => Self::Integer(integer),
			KnownValue::Boolean(boolean) => Self::Boolean(boolean)
		}
	}
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum KnownValue {
	String(String),
	Integer(i64),
	Boolean(bool)
}

impl KnownValue {
	pub fn coerce_to_bool(self) -> bool {
		match self {
			Self::Boolean(value) => value,
			_ => true
		}
	}
}
