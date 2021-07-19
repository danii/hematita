pub use self::{super::Chunk, NillableValue::{Nil, NonNil}};
use std::{
	array::IntoIter as ArrayIntoIter,
	borrow::Borrow,
	collections::HashMap,
	fmt::{Debug, Display, Formatter, Result as FMTResult},
	hash::{Hash, Hasher},
	sync::{Arc, Mutex}
};

/// Represents a lua value.
// TODO: Add floats.
#[derive(Clone)]
pub enum Value {
	Integer(i64),
	String(Box<str>),
	Boolean(bool),
	Table(Arc<Table>),
	Function(Arc<Function>),
	NativeFunction(fn(Arc<Table>, Arc<Table>) -> Result<Arc<Table>, String>)
}

impl Value {
	pub fn new_string(string: impl AsRef<str>) -> Self {
		Self::String(string.as_ref().to_owned().into_boxed_str())
	}

	pub fn type_name(&self) -> &'static str {
		match self {
			Self::Integer(_) => "number",
			Self::String(_) => "string",
			Self::Boolean(_) => "boolean",
			Self::Table(_) => "table",
			Self::Function(_) | Self::NativeFunction(_) => "function"
		}
	}

	/// Coerces this value to a bool. The rules are as follows; If the value is
	/// not a boolean, then true is returned, otherwise the value of the bool
	/// is returned.
	pub fn coerce_to_bool(&self) -> bool {
		match self {
			Self::Boolean(value) => *value,
			_ => true
		}
	}

	/// Like [coerce_to_bool], but wraps the result in a value.
	pub fn coerce_to_boolean(&self) -> Value {
		Value::Boolean(self.coerce_to_bool())
	}

	pub fn integer(&self) -> Option<i64> {
		match self {
			Self::Integer(integer) => Some(*integer),
			_ => None
		}
	}

	pub fn string(&self) -> Option<&str> {
		match self {
			Self::String(string) => Some(string),
			_ => None
		}
	}

	pub fn boolean(&self) -> Option<bool> {
		match self {
			Self::Boolean(boolean) => Some(*boolean),
			_ => None
		}
	}

	pub fn table(&self) -> Option<&Arc<Table>> {
		match self {
			Self::Table(table) => Some(table),
			_ => None
		}
	}

	pub fn function(&self) -> Option<&Arc<Function>> {
		match self {
			Self::Function(function) => Some(function),
			_ => None
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Integer(integer) => write!(f, "{}", integer),
			Self::String(string) => write!(f, "{}", string),
			Self::Boolean(boolean) => write!(f, "{}", boolean),
			Self::Table(table) => write!(f, "{}", table),
			Self::Function(function) => write!(f, "{}", function),
			Self::NativeFunction(function) => write!(f, "function: {:p}", *function)
		}
	}
}

impl Debug for Value {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Integer(integer) => Debug::fmt(integer, f),
			Self::String(string) => Debug::fmt(string, f),
			Self::Boolean(boolean) => Debug::fmt(boolean, f),
			Self::Table(table) => Debug::fmt(table, f),
			Self::Function(function) => Debug::fmt(function, f),
			Self::NativeFunction(function) => function.fmt(f)
		}
	}
}

impl Eq for Value {}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Integer(a), Self::Integer(b)) => *a == *b,
			(Self::String(a), Self::String(b)) => *a == *b,
			(Self::Boolean(a), Self::Boolean(b)) => *a == *b,
			(Self::Function(a), Self::Function(b)) =>
				Arc::as_ptr(a) == Arc::as_ptr(b),
			(Self::Table(a), Self::Table(b)) =>
				Arc::as_ptr(a) == Arc::as_ptr(b),
			(Self::NativeFunction(a), Self::NativeFunction(b)) => a == b,
			_ => false
		}
	}
}

impl Hash for Value {
	fn hash<H>(&self, state: &mut H)
			where H: Hasher {
		match self {
			Self::Integer(integer) => integer.hash(state),
			Self::String(string) => string.hash(state),
			Self::Boolean(boolean) => boolean.hash(state),
			Self::Function(arc) => Arc::as_ptr(arc).hash(state),
			Self::Table(arc) => Arc::as_ptr(arc).hash(state),
			Self::NativeFunction(func) => func.hash(state)
		}
	}
}

impl From<i64> for Value {
	fn from(value: i64) -> Self {
		Self::Integer(value)
	}
}

/// Represents a lua value that may be nil. This type has a lot in common with
/// the [Option] type, but this type has purpose built methods and trait
/// implementations for handling lua nil values. Unlike option, NillableValue
/// can only hold [Value]s or references to them.
#[derive(Clone, Eq, Hash, PartialEq)]
pub enum NillableValue<V>
		where V: Borrow<Value> {
	/// Variant for when the value is not nil.
	NonNil(V),
	/// Variant for when the value is nil.
	Nil
}

impl<V> NillableValue<V>
		where V: Borrow<Value> {
	pub fn is_non_nil(&self) -> bool {
		matches!(self, NonNil(_))
	}

	/// Get the human readable name of the type of this value.
	pub fn type_name(&self) -> &'static str {
		match self {
			NonNil(value) => value.borrow().type_name(),
			Nil => "nil"
		}
	}

	/// Obtains the inner value by value, by cloning it.
	pub fn cloned(&self) -> NillableValue<Value> {
		match self {
			NonNil(value) => NonNil(value.borrow().clone()),
			Nil => Nil
		}
	}

	/// Convenience method for using [Into::into] or [From::from].
	pub fn option(self) -> Option<V> {
		self.into()
	}

	/// Like [Value::coerce_to_bool], but also handles nil cases. The rules are as
	/// follows; If the value is nil or false, false is returned, otherwise true
	/// is.
	pub fn coerce_to_bool(&self) -> bool {
		match self {
			NonNil(value) => value.borrow().coerce_to_bool(),
			Nil => false
		}
	}

	/// Like [coerce_to_bool], but wraps the result in a value.
	pub fn coerce_to_boolean(&self) -> Value {
		Value::Boolean(self.coerce_to_bool())
	}
}

impl<V> Display for NillableValue<V>
		where V: Borrow<Value> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			NillableValue::NonNil(value) => write!(f, "{}", value.borrow()),
			Nil => write!(f, "nil")
		}
	}
}

impl<V> Debug for NillableValue<V>
		where V: Borrow<Value> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			NillableValue::NonNil(value) => write!(f, "{:?}", value.borrow()),
			Nil => write!(f, "nil")
		}
	}
}

impl<V> Default for NillableValue<V>
		where V: Borrow<Value> {
	fn default() -> Self {
		Nil
	}
}

impl<V> From<Option<V>> for NillableValue<V>
		where V: Borrow<Value> {
	fn from(option: Option<V>) -> Self {
		match option {
			Some(value) => NonNil(value),
			None => Nil
		}
	}
}

impl<V> From<NillableValue<V>> for Option<V>
		where V: Borrow<Value> {
	fn from(nillable: NillableValue<V>) -> Self {
		match nillable {
			NonNil(value) => Some(value),
			Nil => None
		}
	}
}

pub trait IntoNillableValue<V>: Sized
		where V: Borrow<Value> {
	fn nillable(self) -> NillableValue<V>;
}

impl<T, V> IntoNillableValue<V> for T
		where T: Into<NillableValue<V>>, V: Borrow<Value> {
	fn nillable(self) -> NillableValue<V> {
		self.into()
	}
}

#[derive(Clone, Debug)]
pub enum MaybeUpValue {
	UpValue(Arc<Mutex<NillableValue<Value>>>),
	Normal(NillableValue<Value>)
}

impl MaybeUpValue {
	pub fn up_value(&mut self) -> &Arc<Mutex<NillableValue<Value>>> {
		match self {
			Self::UpValue(up_value) => up_value,
			Self::Normal(normal) => {
				let normal = Arc::new(Mutex::new(std::mem::replace(normal, Nil)));
				*self = Self::UpValue(normal);
				match self {
					Self::UpValue(up_value) => up_value,
					_ => unreachable!()
				}
			}
		}
	}
}

impl Default for MaybeUpValue {
	fn default() -> Self {
		Self::Normal(Nil)
	}
}

#[derive(Debug, Default)]
pub struct Table {
	pub data: Mutex<HashMap<Value, Value>>,
	pub metatable: Mutex<Option<Arc<Table>>>
}

impl Table {
	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	pub fn from_hashmap(data: HashMap<Value, Value>) -> Self {
		Self {data: Mutex::new(data), metatable: Mutex::new(None)}
	}

	pub fn index(&self, index: &Value) -> NillableValue<Value> {
		let data = self.data.lock().unwrap();
		data.get(index).nillable().cloned()
	}

	pub fn array<V, const N: usize>(data: [&NillableValue<V>; N]) -> Self
			where V: Borrow<Value> {
		let data = Mutex::new(ArrayIntoIter::new(data)
			.map(NillableValue::cloned).map(NillableValue::option).enumerate()
			.map(|(index, value)| (Value::Integer(index as i64), value))
			.filter_map(|(index, value)| value.map(|value| (index, value)))
			.collect::<HashMap<_, _>>());
		Table {data, ..Default::default()}
	}

	pub fn len(&self) -> i64 {
		self.data.lock().unwrap().iter()
			.filter_map(|(key, _)| key.integer())
			.fold(0, |result, index| result.max(index))
	}
}

impl Display for Table {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "table: {:p}", &self)
	}
}

#[derive(Debug)]
pub struct Function {
	pub up_values: Box<[Arc<Mutex<NillableValue<Value>>>]>,
	pub chunk: Arc<Chunk>
}

impl Function {
	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}
}

impl Display for Function {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "function: {:p}", &self)
	}
}

impl From<Chunk> for Function {
	fn from(chunk: Chunk) -> Self {
		Self {chunk: chunk.arc(), up_values: vec![].into_boxed_slice()}
	}
}
