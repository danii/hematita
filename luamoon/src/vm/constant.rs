use self::super::{value::{Function, Value}, Chunk};
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Constant {
	String(String),
	Integer(i64),
	Boolean(bool),
	Chunk(Arc<Chunk>)
}

impl Constant {
	pub fn into_value(self) -> Value {
		match self {
			Self::String(string) => Value::String(string.into_boxed_str()),
			Self::Integer(integer) => Value::Integer(integer),
			Self::Boolean(boolean) => Value::Boolean(boolean),
			Self::Chunk(chunk) => Value::Function(Function {chunk}.arc())
		}
	}
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
