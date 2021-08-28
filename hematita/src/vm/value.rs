pub use self::{super::{Chunk, VirtualMachine}, Nillable::{Nil, NonNil}};
use hashbrown::HashMap;
use std::{borrow::Borrow, fmt::{Debug, Display, Formatter, Result as FMTResult}, hash::{BuildHasher, Hash, Hasher}, mem::take, ptr::{eq, hash}, sync::{Arc, Mutex}};

macro_rules! value_conversions {
	(
		impl<$lf:tt $(; $($param:tt),*)?> for $convert:ident @ $for:ty $code:block
		$($rest:tt)*
	) => {
		impl<$lf $(, $($param),*)?> From<$for> for Value<$lf> {
			fn from($convert: $for) -> Value<$lf> {
				$code
			}
		}

		value_conversions! {$($rest)*}
	};
	() => {}
}

macro_rules! nillable_conversions {
	(
		impl<$lf:tt $(; $($param:tt),*)?> all
		for $convert:ident @ $for:ty $code:block $($rest:tt)*
	) => {
		impl<$lf $(, $($param),*)?> IntoNillable<$lf> for $for {
			#[inline]
			fn nillable(self) -> Nillable<$lf> {
				let $convert = self;
				$code
			}
		}

		impl<$lf $(, $($param),*)?> From<$for> for Nillable<$lf> {
			#[inline]
			fn from($convert: $for) -> Self {
				$code
			}
		}

		nillable_conversions! {$($rest)*}
	};
	(
		impl<$lf:tt $(; $($param:tt),*)?>
		for $convert:ident @ $for:ty $code:block $($rest:tt)*
	) => {
		impl<$lf $(, $($param),*)?> IntoNillable<$lf> for $for {
			#[inline]
			fn nillable(self) -> Nillable<$lf> {
				let $convert = self;
				$code
			}
		}

		nillable_conversions! {$($rest)*}
	};
	() => {}
}

#[macro_export]
macro_rules! lua_value {
	($raw:literal) => {$crate::vm::value::Value::from($raw)};
	($($other:tt)*) => {$crate::vm::value::Value::Table(lua_table! {$($other)*}.arc())}
}

#[macro_export]
macro_rules! lua_table {
	($($arm:tt)*) => {{
		#[allow(unused_assignments, unused_mut, unused_variables, unused_imports)]
		{
			use $crate::{
				vm::value::{IntoNillable, Nillable::NonNil, Table, Value},
				lua_table_inner, lua_value
			};
			use hashbrown::HashMap;
			use std::{default::Default, sync::Mutex};

			let mut table = HashMap::<Value, Value>::new();
			let mut counter = 1;

			lua_table_inner!(table counter {$($arm)*});

			Table {data: Mutex::new(table), ..Default::default()}
		}
	}}
}

#[macro_export]
macro_rules! lua_table_inner {
	($table:ident $counter:ident {[$key:expr] = $value:expr $(, $($rest:tt)*)?}) => {
		{
			if let NonNil(value) = IntoNillable::nillable(lua_table_inner!($value).clone()) {
				match IntoNillable::nillable(lua_table_inner!($key).clone()) {
					NonNil(key) => $table.insert(key, value),
					_ => panic!("attempt to use a nil value as a key in lua_table macro")
				};
			}
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$key:ident = $value:expr $(, $($rest:tt)*)?}) => {
		{
			if let NonNil(value) = IntoNillable::nillable(lua_table_inner!($value).clone()) {
				$table.insert(Value::from(stringify!($key)), value);
			}
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$value:expr $(, $($rest:tt)*)?}) => {
		{
			if let NonNil(value) = IntoNillable::nillable(lua_table_inner!($value).clone()) {
				$table.insert(Value::from($counter), value);
			}
			$counter += 1;
		}

		lua_table_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {$($rest:tt)*}) => {};

	($value:literal) => {lua_value!($value)};
	($value:expr) => {$value}
}

#[macro_export]
macro_rules! lua_tuple {
	($($arm:tt)*) => {{
		#[allow(unused_assignments, unused_mut, unused_variables, unused_imports)]
		{
			use $crate::{
				vm::value::{IntoNillable, Nillable::NonNil, Table, Value},
				lua_tuple_inner, lua_value
			};
			use hashbrown::HashMap;
			use std::{default::Default, sync::Mutex};

			let mut table = HashMap::<Value, Value>::new();
			let mut counter = 0;

			lua_tuple_inner!(table counter {$($arm)*});
			table.insert(Value::Integer(0), Value::Integer(counter));

			Table {data: Mutex::new(table), ..Default::default()}
		}
	}}
}

#[macro_export]
macro_rules! lua_tuple_inner {
	($table:ident $counter:ident {$value:expr $(, $($rest:tt)*)?}) => {
		{
			$counter += 1;
			if let NonNil(value) = IntoNillable::nillable(lua_tuple_inner!($value).clone()) {
				$table.insert(Value::Integer($counter), value);
			}
		}

		lua_tuple_inner!($table $counter {$($($rest)*)?});
	};
	($table:ident $counter:ident {}) => {};

	($value:literal) => {lua_value!($value)};
	($value:expr) => {$value}
}

pub trait UserData: Send + Sync {
	fn type_name(&self) -> &'static str;
}

pub type NativeFunction<'n> = &'n (dyn Fn(Arc<Table<'n>>, &VirtualMachine<'n>)
	-> Result<Arc<Table<'n>>, String> + Send + Sync);

/// Represents a lua value.
// TODO: Add floats.
#[derive(Clone)]
pub enum Value<'n> {
	Integer(i64),
	String(Box<str>),
	Boolean(bool),
	Table(Arc<Table<'n>>),
	UserData {
		data: &'n dyn UserData,
		meta: Option<Arc<Table<'n>>>
	},
	Function(Arc<Function<'n>>),
	NativeFunction(NativeFunction<'n>)
}

impl<'n> Value<'n> {
	pub fn new_string(string: impl AsRef<str>) -> Self {
		Self::String(string.as_ref().to_owned().into_boxed_str())
	}

	pub fn type_name(&self) -> &'static str {
		match self {
			Self::Integer(_) => "number",
			Self::String(_) => "string",
			Self::Boolean(_) => "boolean",
			Self::Table(_) => "table",
			Self::UserData {data, ..} => data.type_name(),
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
	pub fn coerce_to_boolean<'nn>(&self) -> Value<'nn> {
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

	pub fn table(&self) -> Option<&Arc<Table<'n>>> {
		match self {
			Self::Table(table) => Some(table),
			_ => None
		}
	}

	pub fn function(&self) -> Option<&Arc<Function<'n>>> {
		match self {
			Self::Function(function) => Some(function),
			_ => None
		}
	}
}

impl Display for Value<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Integer(integer) => write!(f, "{}", integer),
			Self::String(string) => write!(f, "{}", string),
			Self::Boolean(boolean) => write!(f, "{}", boolean),
			Self::Table(table) => write!(f, "{}", table),
			Self::UserData {..} => todo!(),
			Self::Function(function) => write!(f, "{}", function),
			Self::NativeFunction(function) => write!(f, "function: {:p}", *function)
		}
	}
}

impl Debug for Value<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Self::Integer(integer) => Debug::fmt(integer, f),
			Self::String(string) => Debug::fmt(string, f),
			Self::Boolean(boolean) => Debug::fmt(boolean, f),
			Self::Table(table) => Debug::fmt(table, f),
			Self::UserData {..} => todo!(),
			Self::Function(function) => Debug::fmt(function, f),
			Self::NativeFunction(function) => write!(f, "function: {:p}", function)
		}
	}
}

impl Eq for Value<'_> {}

impl<'l, 'r> PartialEq<Value<'r>> for Value<'l> {
	fn eq(&self, other: &Value<'r>) -> bool {
		match (self, other) {
			(Self::Integer(a), Value::Integer(b)) => *a == *b,
			(Self::String(a), Value::String(b)) => *a == *b,
			(Self::Boolean(a), Value::Boolean(b)) => *a == *b,
			(Self::Function(a), Value::Function(b)) =>
				eq(Arc::as_ptr(a) as *const u8, Arc::as_ptr(b) as *const u8),
			(Self::Table(a), Value::Table(b)) =>
				eq(Arc::as_ptr(a) as *const u8, Arc::as_ptr(b) as *const u8),
			(Self::NativeFunction(a), Value::NativeFunction(b)) =>
				eq(*a as *const _ as *const u8, *b as *const _ as *const u8),
			_ => false
		}
	}
}

impl Hash for Value<'_> {
	fn hash<H>(&self, state: &mut H)
			where H: Hasher {
		match self {
			Self::Integer(integer) => integer.hash(state),
			Self::String(string) => string.hash(state),
			Self::Boolean(boolean) => boolean.hash(state),
			Self::Table(arc) => Arc::as_ptr(arc).hash(state),
			Self::UserData {data, ..} => hash(data, state),
			Self::Function(arc) => Arc::as_ptr(arc).hash(state),
			Self::NativeFunction(func) => hash(func, state)
		}
	}
}

value_conversions! {
	impl<'n> for value @ i64 {Value::Integer(value)}
	impl<'n; 'r> for value @ &'r str {Value::String(value.into())}
	impl<'n> for value @ Box<str> {Value::String(value)}
	impl<'n> for value @ String {Value::String(value.into_boxed_str())}
	impl<'n> for value @ bool {Value::Boolean(value)}
	impl<'n> for value @ Table<'n> {Value::Table(value.arc())}
	impl<'n> for value @ Arc<Table<'n>> {Value::Table(value)}
}

/// Represents a lua value that may be nil. This type has a lot in common with
/// the [Option] type, but this type has purpose built methods and trait
/// implementations for handling lua nil values. Unlike option, NillableValue
/// can only hold [Value]s or references to them.
#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Nillable<'n> {
	/// Variant for when the value is not nil.
	NonNil(Value<'n>),
	/// Variant for when the value is nil.
	Nil
}

impl<'n> Nillable<'n> {
	/// Get the human readable name of the type of this value.
	pub fn type_name(&self) -> &'static str {
		match self {
			NonNil(value) => value.borrow().type_name(),
			Nil => "nil"
		}
	}

	/// Convenience method for using [Into::into] or [From::from].
	pub fn option(self) -> Option<Value<'n>> {
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
	pub fn coerce_to_boolean<'nn>(&self) -> Value<'nn> {
		Value::Boolean(self.coerce_to_bool())
	}

	pub fn is_nil(&self) -> bool {
		matches!(self, Nil)
	}

	pub fn is_non_nil(&self) -> bool {
		matches!(self, NonNil(_))
	}
}

impl Display for Nillable<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Nillable::NonNil(value) => write!(f, "{}", value.borrow()),
			Nil => write!(f, "nil")
		}
	}
}

impl Debug for Nillable<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		match self {
			Nillable::NonNil(value) => write!(f, "{:?}", value.borrow()),
			Nil => write!(f, "nil")
		}
	}
}

impl Default for Nillable<'_> {
	fn default() -> Self {
		Nil
	}
}

pub trait IntoNillable<'n>: Sized {
	fn nillable(self) -> Nillable<'n>;
}

nillable_conversions! {
	// From Option

	impl<'n> all for value @ Option<Value<'n>> {
		match value {
			Some(value) => NonNil(value),
			None => Nil
		}
	}

	impl<'n; 'r> for value @ Option<&'r Value<'n>> {
		match value {
			Some(value) => NonNil(value.clone()),
			None => Nil
		}
	}

	// From Self or Value

	impl<'n> for value @ Nillable<'n> {value}
	impl<'n> all for value @ Value<'n> {NonNil(value)}

	// From Into<Value>

	impl<'n> all for value @ i64 {NonNil(value.into())}
	impl<'n; 'r> all for value @ &'r str {NonNil(value.into())}
	impl<'n> all for value @ Box<str> {NonNil(value.into())}
	impl<'n> all for value @ String {NonNil(value.into())}
	impl<'n> all for value @ bool {NonNil(value.into())}
	impl<'n> all for value @ Table<'n> {NonNil(value.into())}
	impl<'n> all for value @ Arc<Table<'n>> {NonNil(value.into())}
	impl<'n> all for _value @ () {Nil}
}

impl<'n> From<Nillable<'n>> for Option<Value<'n>> {
	fn from(nillable: Nillable<'n>) -> Self {
		match nillable {
			NonNil(value) => Some(value),
			Nil => None
		}
	}
}

#[derive(Clone, Debug)]
pub enum MaybeUpValue<'n> {
	UpValue(Arc<Mutex<Nillable<'n>>>),
	Normal(Nillable<'n>)
}

impl<'n> MaybeUpValue<'n> {
	pub fn up_value(&mut self) -> &Arc<Mutex<Nillable<'n>>> {
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

impl Default for MaybeUpValue<'_> {
	fn default() -> Self {
		Self::Normal(Nil)
	}
}

#[derive(Default)]
pub struct Table<'n> {
	pub data: Mutex<HashMap<Value<'n>, Value<'n>>>,
	pub metatable: Mutex<Option<Arc<Table<'n>>>>
}

impl<'n> Table<'n> {
	/// Inserts a value into this table as if it was an array.
	#[inline]
	pub fn array_insert(&self, index: i64, mut value: Nillable<'n>) {
		let len = self.array_len();
		let mut data = self.data.lock().unwrap();

		(index..=(len.max(1) + 1))
			.for_each(|index| match take(&mut value) {
				NonNil(new) =>
					value = data.insert(Value::Integer(index), new).nillable(),
				Nil =>
					value = data.remove(&Value::Integer(index)).nillable()
			});
	}

	#[inline]
	pub fn array_remove(&self, index: i64) -> Nillable<'n> {
		let len = self.array_len();
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
	pub fn array_push(&self, value: Nillable<'n>) {
		self.array_insert(self.array_len(), value)
	}

	pub fn array_len(&self) -> i64 {
		self.data.lock().unwrap().iter()
			.filter_map(|(key, _)| key.integer())
			.fold(0, |result, index| result.max(index))
	}

	pub fn array_is_empty(&self) -> bool {
		self.data.lock().unwrap().iter()
			.any(|(key, _)| key.integer().is_some())
	}

	/// Inserts a value into this table as if it was a tuple.
	#[inline]
	pub fn tuple_insert(&self, index: i64, mut value: Nillable<'n>) {
		let len = self.tuple_len();
		let mut data = self.data.lock().unwrap();
		data.insert(Value::Integer(0), Value::Integer(len + 1));

		(index..=(len.max(1) + 1))
			.for_each(|index| match take(&mut value) {
				NonNil(new) =>
					value = data.insert(Value::Integer(index), new).nillable(),
				Nil =>
					value = data.remove(&Value::Integer(index)).nillable()
			});
	}

	pub fn tuple_len(&self) -> i64 {
		self.data.lock().unwrap().get(&Value::Integer(0))
			.unwrap().integer().unwrap()
	}

	pub fn index<'qn>(&self, index: &Value<'qn>) -> Nillable<'n> {
		// std::collections::HashMap::get's signature is overly strict. It requires
		// that Q lives as long as K does, but it only uses Q as long as the call.
		// To get around this, we *could* use the nightly feature `hash_raw_entry`,
		// or we could just require the hash library the standard library uses under
		// the hood, hashbrown. Obviously the less painful solution was taken.
		// TODO: Should we drop hashbrown as a dependency when `hash_raw_entry` is
		// stablized? Or is it better to have something that can be upgraded
		// according to semver?

		let data = self.data.lock().unwrap();
		let mut hasher = data.hasher().build_hasher();
		index.hash(&mut hasher);
		data.raw_entry().from_hash(hasher.finish(), |check| index == check)
			.map(|(_, value)| value).cloned().nillable()
		//todo!()
	}

	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}
}

impl<'n> PartialEq for Table<'n> {
	fn eq(&self, other: &Table<'n>) -> bool {
		eq(self, other)
	}
}

impl Display for Table<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "table: {:p}", &*self)
	}
}

impl Debug for Table<'_> {
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
pub struct Function<'n> {
	pub up_values: Box<[Arc<Mutex<Nillable<'n>>>]>,
	pub chunk: Arc<Chunk>
}

impl Function<'_> {
	pub fn arc(self) -> Arc<Self> {
		Arc::new(self)
	}
}

impl<'n> PartialEq for Function<'n> {
	fn eq(&self, other: &Function<'n>) -> bool {
		eq(self, other)
	}
}

impl Eq for Function<'_> {}

impl Display for Function<'_> {
	fn fmt(&self, f: &mut Formatter) -> FMTResult {
		write!(f, "function: {:p}", &self)
	}
}

impl From<Chunk> for Function<'_> {
	fn from(chunk: Chunk) -> Self {
		Self {chunk: chunk.arc(), up_values: vec![].into_boxed_slice()}
	}
}

#[cfg(test)]
mod tests {
	use self::super::{Function, Table, Value};
	use hashbrown::HashMap;
	use std::{hash::{BuildHasher, Hash, Hasher}, ptr::eq as ptr_eq, sync::Mutex};

	macro_rules! assert_table_eq {
		($left:expr, $right:expr) => {{
			let left = $left;
			let right = $right;
			if !table_matches(&left, &right) {
				panic!("left != right\nleft: {:?}\nright: {:?}", &left, &right)
			}
		}}
	}

	macro_rules! assert_value_eq {
		($left:expr, $right:expr) => {{
			let left = $left;
			let right = $right;
			if !value_matches(&left, &right) {
				panic!("left != right\nleft: {:?}\nright: {:?}", &left, &right)
			}
		}}
	}

	fn unrelated_get<'m, 'qn, 'n>(map: &'m HashMap<Value<'n>, Value<'n>>,
			index: &Value<'qn>) -> Option<&'m Value<'n>> {
		let mut hasher = map.hasher().build_hasher();
		index.hash(&mut hasher);
		map.raw_entry().from_hash(hasher.finish(), |check| index == check)
			.map(|(_, value)| value)
	}

	fn value_matches(a: &Value, b: &Value) -> bool {
		match (a, b) {
			(Value::Integer(a), Value::Integer(b)) =>
				a == b,
			(Value::String(a), Value::String(b)) =>
				a == b,
			(Value::Boolean(a), Value::Boolean(b)) =>
				a == b,
			(Value::Table(a), Value::Table(b)) =>
				table_matches(a, b),
			(Value::UserData {data: data_a, meta: Some(meta_a)},
					Value::UserData {data: data_b, meta: Some(meta_b)}) =>
				ptr_eq(data_a, data_b) && table_matches(meta_a, meta_b),
			(Value::UserData {data: data_a, meta: None},
					Value::UserData {data: data_b, meta: None}) =>
				ptr_eq(data_a, data_b),
			(Value::Function(a), Value::Function(b)) =>
				function_matches(a, b),
			(Value::NativeFunction(a), Value::NativeFunction(b)) =>
				ptr_eq(*a as *const _ as *const u8, *b as *const _ as *const u8),
			_ => false
		}
	}

	fn table_matches(a: &Table, b: &Table) -> bool {
		let Table {data: data_a, metatable: meta_a} = a;
		let Table {data: data_b, metatable: meta_b} = b;
		let (data_a, meta_a) = (data_a.lock().unwrap(), meta_a.lock().unwrap());
		let (data_b, meta_b) = (data_b.lock().unwrap(), meta_b.lock().unwrap());

		data_a.len() == data_b.len() &&
			data_a.iter().all(|(key, a)| match unrelated_get(&data_b, key) {
				Some(b) => value_matches(a, b),
				None => false
			}) &&
			match (&*meta_a, &*meta_b) {
				(Some(a), Some(b)) => table_matches(a, b),
				(None, None) => true,
				_ => false
			}
	}

	fn function_matches(_: &Function, _: &Function) -> bool {
		false
	}

	#[test]
	fn test_lua_value_literal() {
		assert_eq!(lua_value!(12), Value::Integer(12));
		assert_eq!(lua_value!("epic sauce"), Value::String("epic sauce".into()));
		assert_eq!(lua_value!(true), Value::Boolean(true));
	}

	#[test]
	fn test_lua_value_table() {
		let raw_string = "this is a string i think";
		let string = Value::String("this is in a quantum state of being a string, and being an enum".into());
		let raw_value = true;
		let value = Value::Boolean(false);

		assert_value_eq!(
			lua_value! {1, "hello", item = true, [raw_string] = value, 9,
				[string] = raw_value, 4},
			Value::Table(lua_table! {1, "hello", item = true, [raw_string] = value, 9,
				[string] = raw_value, 4}.arc())
		)
	}

	#[test]
	fn test_lua_tuple() {
		let raw_bool = true;
		let bool = Value::Boolean(false);
		let function = Value::NativeFunction(&|args, _: &_| Ok(args));

		assert_table_eq!(
			{
				let mut data = HashMap::new();
				data.insert(Value::Integer(0), Value::Integer(5));
				data.insert(Value::Integer(1), Value::Integer(8));
				data.insert(Value::Integer(2), Value::Boolean(raw_bool));
				data.insert(Value::Integer(3), Value::String("hello".into()));
				data.insert(Value::Integer(4), bool.clone());
				data.insert(Value::Integer(5), function.clone());
				Table {data: Mutex::new(data), ..Default::default()}
			},
			lua_tuple![8, raw_bool, "hello", bool, function]
		)
	}
}
