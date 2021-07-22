pub use self::{super::{Chunk, VirtualMachine}, NillableValue::{Nil, NonNil}};
use std::{
	array::IntoIter as ArrayIntoIter,
	borrow::Borrow,
	collections::HashMap,
	fmt::{Debug, Display, Formatter, Result as FMTResult},
	hash::{Hash, Hasher},
	mem::take,
	ptr::{eq, hash},
	sync::{Arc, Mutex}
};

#[macro_export]
macro_rules! lua_value {
	($raw:literal) => {$crate::vm::value::Value::from($raw)};
	($($other:tt)*) => {Value::Table(lua_table! {$($other)*}.arc())}
}

#[macro_export]
macro_rules! lua_table {
	($($arm:tt)*) => {{
		#[allow(unused_assignments, unused_mut, unused_variables)]
		{
			use std::collections::HashMap;
			use $crate::{vm::value::{Table, Value}, lua_table_inner};

			let mut table = HashMap::<Value, Value>::new();
			let mut counter = 1;

			lua_table_inner!(table counter {$($arm)*});

			Table::from_hashmap(table)
		}
	}}
}

#[macro_export]
macro_rules! lua_table_inner {
	($table:ident $counter:ident {[$key:expr] = $value:expr $(, $($rest:tt)*)?}) => {
		{
			$table.insert(lua_table_inner!($key), lua_table_inner!($value));
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$key:ident = $value:expr $(, $($rest:tt)*)?}) => {
		{
			$table.insert(Value::from(stringify!($key)), lua_table_inner!($value));
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$value:expr $(, $($rest:tt)*)?}) => {
		{
			$table.insert(Value::from($counter), lua_table_inner!($value));
			$counter += 1;
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$($rest:tt)*}) => {};

	($value:literal) => {lua_value!($value)};
	($value:expr) => {$value}
}

pub type NativeFunction<'r> = &'r dyn Fn(Arc<Table>, &VirtualMachine)
	-> Result<Arc<Table>, String>;

/// Represents a lua value.
// TODO: Add floats.
#[derive(Clone)]
pub enum Value {
	Integer(i64),
	String(Box<str>),
	Boolean(bool),
	Table(Arc<Table>),
	Function(Arc<Function>),
	NativeFunction(NativeFunction<'static>)
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
			Self::NativeFunction(function) => write!(f, "function: {:p}", function)
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
			(Self::NativeFunction(a), Self::NativeFunction(b)) =>
				eq(*a as *const _ as *const u8, *b as *const _ as *const u8),
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
			Self::NativeFunction(func) => hash(func, state)
		}
	}
}

impl From<i64> for Value {
	fn from(value: i64) -> Self {
		Self::Integer(value)
	}
}

impl From<&str> for Value {
	fn from(value: &str) -> Self {
		Self::String(value.to_owned().into_boxed_str())
	}
}

impl From<Box<str>> for Value {
	fn from(value: Box<str>) -> Self {
		Self::String(value)
	}
}

impl From<String> for Value {
	fn from(value: String) -> Self {
		Self::String(value.into_boxed_str())
	}
}

impl From<bool> for Value {
	fn from(value: bool) -> Self {
		Self::Boolean(value)
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

impl From<i64> for NillableValue<Value> {
	fn from(value: i64) -> Self {
		NonNil(value.into())
	}
}

impl From<&str> for NillableValue<Value> {
	fn from(value: &str) -> Self {
		NonNil(value.into())
	}
}

impl From<Box<str>> for NillableValue<Value> {
	fn from(value: Box<str>) -> Self {
		NonNil(value.into())
	}
}

impl From<String> for NillableValue<Value> {
	fn from(value: String) -> Self {
		NonNil(value.into())
	}
}

impl From<bool> for NillableValue<Value> {
	fn from(value: bool) -> Self {
		NonNil(value.into())
	}
}

impl From<()> for NillableValue<Value> {
	fn from((): ()) -> Self {
		Nil
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

#[derive(Default)]
pub struct Table {
	pub data: Mutex<HashMap<Value, Value>>,
	pub metatable: Mutex<Option<Arc<Table>>>
}

impl Table {
	pub fn from_hashmap(data: HashMap<Value, Value>) -> Self {
		Self {data: Mutex::new(data), ..Default::default()}
	}

	pub fn array<V, const N: usize>(data: [&NillableValue<V>; N]) -> Self
			where V: Borrow<Value> {
		let data = Mutex::new(ArrayIntoIter::new(data)
			.map(NillableValue::cloned).map(NillableValue::option).enumerate()
			.map(|(index, value)| (Value::Integer(index as i64 + 1), value))
			.filter_map(|(index, value)| value.map(|value| (index, value)))
			.collect::<HashMap<_, _>>());
		Self {data, ..Default::default()}
	}

	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	#[inline]
	pub fn array_insert(&self, index: usize, mut value: NillableValue<Value>) {
		let len = self.len() as usize;
		let mut data = self.data.lock().unwrap();

		(index..=len.max(1))
			.for_each(|index| match take(&mut value) {
				NonNil(new) =>
					value = data.insert(Value::Integer(index as i64), new).nillable(),
				Nil =>
					value = data.remove(&Value::Integer(index as i64)).nillable()
			});
	}

	#[inline]
	pub fn array_remove(&self, index: usize) -> NillableValue<Value> {
		let len = self.len() as usize;
		let mut data = self.data.lock().unwrap();

		let mut value = Nil;
		(index..=len).rev()
			.for_each(|index| match take(&mut value) {
				NonNil(new) =>
					value = data.insert(Value::Integer(index as i64), new).nillable(),
				Nil =>
					value = data.remove(&Value::Integer(index as i64)).nillable()
			});
		value
	}

	#[inline]
	pub fn array_push(&self, value: NillableValue<Value>) {
		self.array_insert(self.len() as usize, value)
	}

	pub fn len(&self) -> i64 {
		self.data.lock().unwrap().iter()
			.filter_map(|(key, _)| key.integer())
			.fold(0, |result, index| result.max(index))
	}

	pub fn is_empty(&self) -> bool {
		self.data.lock().unwrap().iter()
			.any(|(key, _)| key.integer().is_some())
	}

	pub fn index(&self, index: &Value) -> NillableValue<Value> {
		let data = self.data.lock().unwrap();
		data.get(index).nillable().cloned()
	}
}

impl PartialEq for Table {
	fn eq(&self, other: &Table) -> bool {
		eq(self, other)
	}
}

impl Display for Table {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "table: {:p}", &*self)
	}
}

impl Debug for Table {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self.data.try_lock() {
			Ok(data) => {
				let mut first = true;
				let mut comma = || {
					if first {first = false; ""}
					else {", "}
				};

				write!(f, "{{")?;
				let mut array = data.iter()
					.filter_map(|(key, value)| if let Value::Integer(key) = key
						{Some((key, value))} else {None})
					.collect::<Vec<_>>();
				array.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
				if let Some((highest, _)) = array.last() {
					(1..=**highest)
						.map(|index| array.iter().find(|value| *value.0 == index)
							.map(|(_, value)| *value))
						.try_for_each(|value| write!(f, "{}{:?}", comma(), value.nillable()))?;
				}

				data.iter()
					.try_for_each(|(key, value)| match key {
						Value::Integer(_) => Ok(()),
						key => write!(f, "{}[{:?}] = {:?}", comma(), key, value)
					})?;

				write!(f, "}}")
			},
			Err(_) => write!(f, "{{<table is being accessed>}}")
		}
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

impl PartialEq for Function {
	fn eq(&self, other: &Function) -> bool {
		eq(self, other)
	}
}

impl Eq for Function {}

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
