use crate::ast::parser::AssignmentTarget;

use self::super::{
	ast::parser::{BinaryOperator, Block, Expression, KeyValue, Statement},
	vm::{
		constant::{Constant, KnownValue},
		Chunk, BinaryOperation, OpCode, UnaryOperation
	}
};
use std::{collections::HashMap, convert::TryInto, iter::once};

#[macro_export]
macro_rules! insert_byte_code {
	($into:ident {$($code:tt)*}) => {{
		use $crate::byte_code;
		$into.opcodes.extend(byte_code! {$($code)*});
	}}
}

pub fn compile_block(block: &Block) -> Chunk {
	let mut compiler = Generator::new();
	compiler.compile(block);
	compiler.finish()
}

pub fn compile_function(block: &Block, arguments: &[String],
		up_values: HashMap<String, (usize, bool)>, method: bool) -> Chunk {
	let mut compiler = Generator {up_values, ..Generator::new()};
	compiler.compile_function_header(arguments, method);
	compiler.compile(block);
	compiler.finish()
}

#[derive(Clone, Debug)]
enum CompileResult {
	Evaluated(KnownValue),
	Register(usize, bool)
}

impl CompileResult {
	fn register(self, compiler: &mut Generator) -> usize {
		match self {
			Self::Evaluated(known) => compiler.compile_known(known),
			Self::Register(register, tuple) => {
				if tuple {compiler.unwrap_tuple(register)}
				register
			}
		}
	}
}

#[derive(Debug)]
struct Generator {
	/// The constant pool.
	///
	/// The constant pool is a staic list of primitive values that can be loaded
	/// during runtime. During compile time, this list is typically accessed
	/// indirectly via the [CompileResult] and [KnownValue] types.
	constants: Vec<Constant>,
	opcodes: Vec<OpCode<'static>>,

	up_values: HashMap<String, (usize, bool)>,

	// TODO: currently only written to
	registers: Vec<Option<KnownValue>>,
	evaluated_variables: HashMap<String, KnownValue>,
	variables_to_registers: HashMap<String, usize>
}

impl Generator {
	fn new() -> Self {
		Self::default()
	}

	fn opcode(&mut self, opcode: OpCode<'static>) {
		self.opcodes.push(opcode);
	}

	fn register(&mut self) -> usize {
		self.registers.push(None);
		self.registers.len() - 1
	}

	fn compile_known(&mut self, value: impl Into<KnownValue>) -> usize {
		let value = value.into();

		if let Some(register) = self.registers.iter()
				.position(|known| known.as_ref().map(|known| known == &value)
					.unwrap_or(false))
			{return register}

		let register = self.register();
		self.registers[register] = Some(value.clone());
		let value = match value {
			KnownValue::Nil => {
				self.opcode(OpCode::LoadConst {constant: u16::MAX, register});
				return register
			},
			KnownValue::Integer(value) => Constant::Integer(value),
			KnownValue::String(value) => Constant::String(value),
			KnownValue::Boolean(value) => Constant::Boolean(value)
		};

		let constant = self.constants.iter()
			.position(|constant| constant == &value)
			.unwrap_or_else(|| {
				self.constants.push(value);
				self.constants.len() - 1
			}) as u16;
		self.opcode(OpCode::LoadConst {constant, register});
		register
	}

	fn finish(self) -> Chunk {
		let Self {constants, opcodes, registers, up_values, ..} = self;

		let registers = registers.len() + 1; // 0th register is always arguments.

		let up_values =
			(0..up_values.values().filter_map(|&(a, b)| (!b).then(|| a))
				.max().map(|v| v + 1).unwrap_or(0)).map(|a| (a, false))
			.chain((0..up_values.values().filter_map(|&(a, b)| b.then(|| a))
				.max().map(|v| v + 1).unwrap_or(0)).map(|a| (a, true)))
			.collect::<Vec<_>>();

		Chunk {constants, opcodes, registers, up_values}
	}

	fn up_value_id(&self, (up_value, use_up_value): (usize, bool)) -> usize {
		match use_up_value {
			true => up_value + self.up_values.values()
				.filter_map(|(up_value, use_up_value)|
					(!use_up_value).then(|| *up_value))
				.max().unwrap_or(0),
			false => up_value
		}
	}

	fn unwrap_tuple(&mut self, tuple: usize) {
		let index = self.compile_known(1);
		self.opcode(OpCode::IndexRead {index, indexee: tuple, destination: tuple});
	}

	fn compile(&mut self, block: &Block) {
		let Block(block) = block;
		let mut current_statement = 0;

		loop {
			if current_statement >= block.len() {return}

			match &block[current_statement] {
				// Control

				// TODO: else_ifs
				Statement::If {condition, then, r#else, ..} =>
						match self.compile_expression(condition) {
					// We know what condition is now.
					CompileResult::Evaluated(result) => {
						if result.coerce_to_bool() {
							// If the condition is known to be true, we can skip compiling
							// any other block except then.
							self.compile(then);
						} else if let Some(r#else) = r#else {
							// Or, if it's known to be false, we can do the same thing but
							// with r#else...
							self.compile(r#else);
						}
					},
					// We don't know what condition is.
					CompileResult::Register(variable, tuple) => match r#else {
						Some(r#else) => {
							// Jump If local To 'then
							// Block r#else
							// Jump To 'done
							// 'then
							// Block then
							// 'done

							if tuple {self.unwrap_tuple(variable)}

							let jump_then = self.opcodes.len();
							self.opcode(OpCode::NoOp);
							self.compile(r#else);
							let jump_done = self.opcodes.len();
							self.opcode(OpCode::NoOp);

							self.opcodes[jump_then] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
							// TODO: Everytime we add a jump opcode we have to add this or
							// else internal state gets funky. Need a better long term
							// solution..
							self.registers.iter_mut().for_each(|value| *value = None);
							self.compile(then);
							self.opcodes[jump_done] = OpCode::Jump {
								r#if: None,
								operation: self.opcodes.len() as u64
							};
							self.registers.iter_mut().for_each(|value| *value = None);
						},
						None => {
							// UnaryOperation not local To local
							// Jump If local To 'skip
							// Block then
							// 'skip

							self.opcode(OpCode::UnaryOperation {
								operand: variable,
								operation: UnaryOperation::LogicalNot,
								destination: variable
							});
							let jump = self.opcodes.len();
							self.opcode(OpCode::NoOp);
							self.compile(then);

							self.registers.iter_mut().for_each(|value| *value = None);
							self.opcodes[jump] = OpCode::Jump {
								r#if: Some(variable),
								operation: self.opcodes.len() as u64
							};
						}
					}
				},

				Statement::GenericFor {variable, iterator, r#do} => {
					self.prepare_side_effects();

					let top = self.opcodes.len() as u64;
					let function = self.compile_expression(iterator).register(self);
					let arguments = self.register();

					self.opcode(OpCode::Create {destination: arguments});
					//let index = self.compile_known(2 as i64);
					//self.opcode(OpCode::IndexWrite {indexee: arguments, index, value})

					let destination = self.register();
					let index = self.compile_known(1);
					self.opcode(OpCode::Call {function, arguments, destination});
					self.opcode(OpCode::IndexRead {indexee: destination,
						index, destination});
					let condition = self.register();
					let right = self.compile_known(KnownValue::Nil);
					self.opcode(OpCode::BinaryOperation {left: destination, right,
						operation: BinaryOperation::Equal, destination: condition});
					let done = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					self.variables_to_registers.insert(variable.clone(), destination);
					self.compile(r#do);
					self.opcode(OpCode::Jump {operation: top, r#if: None});

					self.opcodes[done] = OpCode::Jump {
						operation: self.opcodes.len() as u64, r#if: Some(condition)};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::NumericFor {variable, first, step, limit, r#do} => {
					self.prepare_side_effects();

					let value = self.compile_expression(first).register(self);
					let limit = self.compile_expression(limit).register(self);
					let step = self.compile_expression(step).register(self);
					let condition = self.register();

					let value = {
						let new = self.register();
						self.opcode(OpCode::ReAssign {actor: value, destination: new});
						new
					};

					let operation = self.opcodes.len() as u64;
					self.registers.iter_mut().for_each(|value| *value = None);

					self.variables_to_registers.insert(variable.clone(), value);
					self.compile(r#do);
					self.opcode(OpCode::BinaryOperation {left: value, right: limit,
						operation: BinaryOperation::Equal, destination: condition});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);
					self.opcode(OpCode::BinaryOperation {left: value, right: step,
						operation: BinaryOperation::Add, destination: value});
					self.opcode(OpCode::Jump {operation, r#if: None});

					self.opcodes[jump] = OpCode::Jump {r#if: Some(condition),
						operation: self.opcodes.len() as u64};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::While {block, condition, run_first: false} => {
					self.prepare_side_effects();

					let operation = self.opcodes.len() as u64;
					let operand = self.compile_expression(condition).register(self);
					self.opcode(OpCode::UnaryOperation {operand, destination: operand,
						operation: UnaryOperation::LogicalNot});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);
					
					self.compile(block);
					self.opcode(OpCode::Jump {operation, r#if: None});
					self.opcodes[jump] = OpCode::Jump {operation: self.opcodes.len() as u64, r#if: Some(operand)};
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::While {block, condition, run_first: true} => {
					self.prepare_side_effects();

					let operation = self.opcodes.len() as u64;
					self.compile(block);

					let operand = self.compile_expression(condition).register(self);
					self.opcode(OpCode::UnaryOperation {operand, destination: operand,
						operation: UnaryOperation::LogicalNot});
					self.opcode(OpCode::Jump {operation, r#if: Some(operand)});
					self.registers.iter_mut().for_each(|value| *value = None);
				},

				Statement::Return {values} => {
					let destination = self.register();
					self.opcode(OpCode::Create {destination});
					values.iter().enumerate().for_each(|(index, value)| {
						let index = self.compile_known(index as i64 + 1);
						let variable = self.compile_expression(value).register(self);

						self.opcode(OpCode::IndexWrite {indexee: destination, index, value: variable});
					});

					let zero = self.compile_known(0i64);
					let count = self.compile_known(values.len() as i64);
					self.opcode(OpCode::IndexWrite {indexee: destination, index: zero, value: count});

					self.opcode(OpCode::Return {result: destination});
				},

				// Assignment

				Statement::Assign {variables, values} => {
					let mut variables = once(&variables.0).chain(variables.1.iter());
					let mut names = Vec::new();
					let mut value = {
						let values: Vec<_> = values.iter()
							.map(|value| self.compile_expression(value)).collect();
						iter_tuple(values.into_iter())
					};

					while let Some(target) = variables.next() {
							match (target, value(self)) {
						(AssignmentTarget::Identifier(identifier),
								CompileResult::Evaluated(value)) => match (
							self.evaluated_variables.get(identifier),
							self.variables_to_registers.get(identifier),
							self.up_values.get(identifier)
						) {
							(Some(_), _, _) => names.push((identifier.clone(),
								CompileResult::Evaluated(value))),
							(None, Some(&register), _) => {
								// TODO: We could probably add destination to compile_known?
								let old = self.compile_known(value.clone());
								insert_byte_code!(self {reas old, register});
								self.registers[register] = Some(value);
							},
							(None, None, Some(&up_value)) => {
								let register = self.compile_known(value);
								let up_value = self.up_value_id(up_value);
								insert_byte_code!(self {suv register, ^up_value})
							},
							(None, None, None) => {
								let global = Box::leak(identifier.clone().into_boxed_str());
								let register = self.compile_known(value);
								self.opcode(OpCode::SaveGlobal {register, global});
								// FIXME: Requires GATs
								//insert_byte_code!(self {sglb register, {&*global}});
							},
						},
						(AssignmentTarget::Identifier(identifier),
								CompileResult::Register(old, tuple)) => {
							if tuple {self.unwrap_tuple(old)}
							match (
								self.evaluated_variables.get(identifier),
								self.variables_to_registers.get(identifier),
								self.up_values.get(identifier)
							) {
								(Some(_), _, _) => names.push((identifier.clone(),
									CompileResult::Register(old, false))),
								// TODO: This is what destination in compile_expression was
								// for.
								(None, Some(&new), _) =>
									insert_byte_code!(self {reas old, new}),
								(None, None, Some(&up_value)) => {
									let up_value = self.up_value_id(up_value);
									insert_byte_code!(self {suv old, ^up_value})
								},
								(None, None, None) => {
									let global = Box::leak(identifier.clone().into_boxed_str());
									self.opcode(OpCode::SaveGlobal {register: old, global});
									// FIXME: Requires GATs
									//insert_byte_code!(self {sglb register, {&*global}});
								}
							}
						}
						(AssignmentTarget::Index {indexee, index},
								CompileResult::Evaluated(value)) => {
							let indexee = self.compile_expression(indexee).register(self);
							let index = self.compile_expression(index).register(self);
							let register = self.compile_known(value);
							insert_byte_code!(self {idxw indexee, index, register});
						},
						(AssignmentTarget::Index {indexee, index},
								CompileResult::Register(value, tuple)) => {
							if tuple {self.unwrap_tuple(value)}
							let indexee = self.compile_expression(indexee).register(self);
							let index = self.compile_expression(index).register(self);
							insert_byte_code!(self {idxw indexee, index, value});
						}
					}}

					names.into_iter()
						.for_each(|(name, value)| match value {
							CompileResult::Register(value, _) => {
								self.evaluated_variables.remove(&name);
								self.variables_to_registers.insert(name, value);
							},
							CompileResult::Evaluated(value) => {
								// TODO: Next line may not be necessary?
								self.variables_to_registers.remove(&name);
								self.evaluated_variables.insert(name, value);
							}
						});
				},

				Statement::LocalAssign {variables, values} => {
					let mut variables = once(&variables.0).chain(variables.1.iter());
					let mut names = Vec::new();
					let mut value = {
						let values: Vec<_> = values.iter()
							.map(|value| self.compile_expression(value)).collect();
						iter_tuple(values.into_iter())
					};

					while let Some(name) = variables.next() {match value(self) {
						CompileResult::Evaluated(value) => {
							// We don't have to assign this variable immediately, because
							// it's known.
							names.push((name.clone(),
								CompileResult::Evaluated(value)));
						},
						CompileResult::Register(register, tuple) => {
							// identifier already has it's own register. We don't want to
							// be able to change it's data if we write to the new
							// variable, so we make a new register, and reassign.

							let destination = self.register();
							if tuple {self.unwrap_tuple(register)}
							insert_byte_code!(self {reas register, destination});
							names.push((name.clone(),
								CompileResult::Register(destination, false)));
						}
					}}

					names.into_iter()
						.for_each(|(name, value)| match value {
							CompileResult::Register(value, _) => {
								self.evaluated_variables.remove(&name);
								self.variables_to_registers.insert(name, value);
							},
							CompileResult::Evaluated(value) => {
								// TODO: Next line may not be necessary?
								self.variables_to_registers.remove(&name);
								self.evaluated_variables.insert(name, value);
							}
						});
				},
				/*
				(true, Expression::Identifier(name),
								CompileResult::Evaluated(value)) => {
							// We don't have to assign this variable immediately, because
							// it's known.
							clea.push((name.clone(), CompileResult::Evaluated(value)));
						},
						(true, Expression::Identifier(name),
								CompileResult::Register(actor, tuple)) => {
							// identifier already has it's own register. We don't want to
							// be able to change it's data if we write to the new
							// variable, so we make a new register, and reassign.
							if tuple {self.unwrap_tuple(actor)}
							let destination = self.register();
							self.opcode(OpCode::ReAssign {actor, destination});

							clea.push((name.clone(), CompileResult::Register(destination, false)));
						},
						(true, _, _) => panic!(),*/

				// Expressions

				Statement::FunctionCall {function, arguments} =>
					drop(self.compile_call(function, arguments)),

				Statement::MethodCall {class, method, arguments} =>
					drop(self.compile_method_call(class, method, arguments)),

				Statement::Function
						{name: (name_first, name_rest), arguments, body} => {
					self.prepare_side_effects();

					let register = self.register();
					let up_values = self.variables_to_registers.iter()
						.map(|(key, &value)| (key.clone(), (value, false)))
						.chain(self.up_values.iter()
							.map(|(key, &(value, _))| (key.clone(), (value, true))))
						.collect();
					let function = compile_function(body, arguments, up_values, false);
					self.constants.push(Constant::Chunk(function.arc()));
					let constant = self.constants.len() as u16 - 1;
					self.opcode(OpCode::LoadConst {constant, register});

					if !name_rest.is_empty() {
						let indexee = self.compile_expression(
							&Expression::Identifier(name_first.clone())).register(self);
						name_rest.iter().take(name_rest.len() - 1).for_each(|part| {
							let index = self.compile_known(KnownValue::String(part.clone()));
							self.opcode(OpCode::IndexRead {
								indexee, index, destination: indexee});
						});
						let index = self.compile_known(KnownValue::String(
							name_rest[name_rest.len() - 1].clone()));
						self.opcode(OpCode::IndexWrite {index, indexee, value: register});
					} else {
						// TODO: Assigning is hard... :(
						self.opcode(OpCode::SaveGlobal {register,
							global: Box::leak(name_first.clone().into_boxed_str())});
					}
				},

				Statement::Method
						{class: (class_first, class_rest), name, arguments, body} => {
					self.prepare_side_effects();

					let register = self.register();
					let up_values = self.variables_to_registers.iter()
						.map(|(key, &value)| (key.clone(), (value, false)))
						.chain(self.up_values.iter()
							.map(|(key, &(value, _))| (key.clone(), (value, true))))
						.collect();
					let function = compile_function(body, arguments, up_values, true);
					self.constants.push(Constant::Chunk(function.arc()));
					let constant = self.constants.len() as u16 - 1;
					self.opcode(OpCode::LoadConst {constant, register});

					let indexee = self.compile_expression(
						&Expression::Identifier(class_first.clone())).register(self);
					class_rest.iter().for_each(|part| {
						let index = self.compile_known(KnownValue::String(part.clone()));
						self.opcode(OpCode::IndexRead {
							indexee, index, destination: indexee});
					});
					let index = self.compile_known(KnownValue::String(name.clone()));
					self.opcode(OpCode::IndexWrite {index, indexee, value: register});
				},

				Statement::LocalFunction {name, arguments, body} => {
					self.prepare_side_effects();

					let register = self.register();
					let up_values = self.variables_to_registers.iter()
						.map(|(key, &value)| (key.clone(), (value, false)))
						.chain(self.up_values.iter()
							.map(|(key, &(value, _))| (key.clone(), (value, true))))
						.collect();
					let function = compile_function(body, arguments, up_values, false);
					self.constants.push(Constant::Chunk(function.arc()));
					let constant = self.constants.len() as u16 - 1;
					self.opcode(OpCode::LoadConst {constant, register});
					self.variables_to_registers.insert(name.clone(), register);
				}
			}

			current_statement += 1;
		}
	}

	fn compile_expression(&mut self, expression: &Expression) -> CompileResult {
		match expression {
			// Identifier

			Expression::Identifier(identifier) =>
					match self.evaluated_variables.get(identifier) {
				// If we evaluated a value for identifier, return it.
				Some(known) => CompileResult::Evaluated(known.clone()),
				// Otherwise, check the local scope.
				None => match self.variables_to_registers.get(identifier) {
					// If it's in local scope, return it's register.
					Some(&register) => CompileResult::Register(register, false),
					// Otherwise, check up values.
					None => match self.up_values.get(identifier) {
						Some(&up_value) => {
							let register = self.register();
							self.opcode(OpCode::LoadUpValue {up_value: self.up_value_id(up_value), register});
							CompileResult::Register(register, false)
						},
						// Otherwise, load from global scope.
						None => {
							let register = self.register();
							self.opcode(OpCode::LoadGlobal {global: Box::leak(
								identifier.clone().into_boxed_str()), register});
							CompileResult::Register(register, false)
						}
					}
				}
			},

			// Singleton literals

			// These all just return evaluated.
			Expression::Nil => CompileResult::Evaluated(KnownValue::Nil),
			Expression::True => CompileResult::Evaluated(KnownValue::Boolean(true)),
			Expression::False => CompileResult::Evaluated(KnownValue::Boolean(false)),

			// Literals

			// Same with these.
			Expression::Integer(integer) =>
				CompileResult::Evaluated(KnownValue::Integer(*integer)),
			Expression::String(string) =>
				CompileResult::Evaluated(KnownValue::String(string.clone())),

			// Complex literals

			Expression::Table {array, key_value} => {
				let register = self.register();
				self.opcode(OpCode::Create {destination: register});

				key_value.iter().for_each(|KeyValue {key, value}| {
					let key = self.compile_expression(key).register(self);
					let value = self.compile_expression(value).register(self);

					self.opcode(OpCode::IndexWrite {index: key, value, indexee: register});
				});
				array.iter().enumerate().for_each(|(index, value)| {
					let value = self.compile_expression(value).register(self);
					let temporary = self.compile_known(index as i64 + 1);
					self.opcode(OpCode::IndexWrite {index: temporary, value, indexee: register});
				});

				CompileResult::Register(register, false)
			},

			Expression::Function {arguments, body} => {
				self.prepare_side_effects();
				let up_values = self.variables_to_registers.iter()
					.map(|(key, &value)| (key.clone(), (value, false)))
					.chain(self.up_values.iter()
						.map(|(key, &(value, _))| (key.clone(), (value, true))))
					.collect();
				let function = compile_function(body, arguments, up_values, false);
				self.constants.push(Constant::Chunk(function.arc()));
				let constant = self.constants.len() as u16 - 1;
				let destination = self.register();

				self.opcode(OpCode::LoadConst {constant, register: destination});
				CompileResult::Register(destination, false)
			},

			// Operators

			Expression::Call {function, arguments} => {
				let register = self.compile_call(function, arguments);
				CompileResult::Register(register, true)
			},

			Expression::MethodCall {class, method, arguments} => {
				let register = self.compile_method_call(class, method, arguments);
				CompileResult::Register(register, true)
			},

			Expression::Index {indexee, index} => {
				let destination = self.register();
				let indexee = self.compile_expression(indexee).register(self);
				let index = self.compile_expression(index).register(self);
				self.opcode(OpCode::IndexRead {indexee, index, destination});
				CompileResult::Register(destination, false)
			},

			Expression::BinaryOperation {left, right,
					operator: BinaryOperator::LogicalAnd} =>
						match self.compile_expression(left) {
				CompileResult::Evaluated(left) =>
					if !left.coerce_to_bool() {CompileResult::Evaluated(left)}
					else {self.compile_expression(right)},
				CompileResult::Register(left, tuple) => {
					// unop not {left} {boolean}
					// cjmp {done} {boolean}
					// <right compiled to {right}>
					// reas {right} {left}
					// <location of done>

					if tuple {self.unwrap_tuple(left)}

					let boolean = self.register();
					self.opcode(OpCode::UnaryOperation {operand: left,
						operation: UnaryOperation::LogicalNot, destination: boolean});
					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					let right = self.compile_expression(right).register(self);
					self.opcode(OpCode::ReAssign {actor: right, destination: left});
					let operation = self.opcodes.len() as u64;
					self.opcodes[jump] = OpCode::Jump {operation, r#if: Some(boolean)};
					self.registers.iter_mut().for_each(|value| *value = None);

					CompileResult::Register(left, false)
				}
			},

			Expression::BinaryOperation {left, right,
					operator: BinaryOperator::LogicalOr} =>
						match self.compile_expression(left) {
				CompileResult::Evaluated(left) =>
					if left.coerce_to_bool() {CompileResult::Evaluated(left)}
					else {self.compile_expression(right)},
				CompileResult::Register(left, tuple) => {
					// cjmp {done} {left}
					// <right compiled to {right}>
					// reas {right} {left}
					// <location of done>

					if tuple {self.unwrap_tuple(left)}

					let jump = self.opcodes.len();
					self.opcode(OpCode::NoOp);

					let right = self.compile_expression(right).register(self);
					self.opcode(OpCode::ReAssign {actor: right, destination: left});
					let operation = self.opcodes.len() as u64;
					self.opcodes[jump] = OpCode::Jump {operation, r#if: Some(left)};
					self.registers.iter_mut().for_each(|value| *value = None);

					CompileResult::Register(left, false)
				}
			},

			Expression::BinaryOperation {left, operator, right} => {
				let left = self.compile_expression(left).register(self);
				let right = self.compile_expression(right).register(self);
				let destination = self.register();
				self.opcode(OpCode::BinaryOperation {left, right, destination,
					operation: (*operator).try_into().unwrap()});
				CompileResult::Register(destination, false)
			},

			Expression::UnaryOperation {operator, operand} => {
				let operand = self.compile_expression(operand).register(self);
				let destination = self.register();
				self.opcode(OpCode::UnaryOperation {operand, destination,
					operation: (*operator).into()});
				CompileResult::Register(destination, false)
			}
		}
	}

	fn compile_tuple<'i, T>(&mut self, tuple: T) -> usize
			where T: Iterator<Item = &'i dyn CompileOrCompiled> {
		let mut indexee = usize::MAX;
		let mut tuple = tuple.into_iter().peekable();
		let mut index = 0usize;

		loop {
			match (tuple.next().map(|compileable| compileable.compile(self)),
					tuple.peek().is_some()) {
				// If the last argument is a tuple, and it's the *only* argument, use
				// it as the tuple.
				(Some(CompileResult::Register(register, true)), false)
						if index == 0 => break register,

				// If the last argument is a tuple, assign each item from it to the new
				// tuple.
				(Some(CompileResult::Register(other, true)), false) => {
					let mut load = |value| {
						let constant = self.constants.iter()
							.position(|constant| constant == &value)
							.unwrap_or_else(|| {
								self.constants.push(value);
								self.constants.len() - 1
							}) as u16;
						let register = self.register();
						self.opcode(OpCode::LoadConst {constant, register});
						register
					};

					let index = load(Constant::Integer(index as i64));
					let other_index = load(Constant::Integer(0i64));
					let one = load(Constant::Integer(1i64));
					let zero = load(Constant::Integer(0i64));
					self.prepare_side_effects();

					let temporary = self.register();
					let operation = self.opcodes.len() as u64;
					self.opcode(OpCode::BinaryOperation {left: index, right: one,
						operation: BinaryOperation::Add, destination: index});
					self.opcode(OpCode::BinaryOperation {left: other_index, right: one,
						operation: BinaryOperation::Add, destination: other_index});
					self.opcode(OpCode::IndexRead
						{indexee: other, index: other_index, destination: temporary});
					self.opcode(OpCode::IndexWrite {indexee, index, value: temporary});
					self.opcode(OpCode::IndexRead
						{indexee: other, index: zero, destination: temporary});
					self.opcode(OpCode::BinaryOperation {left: temporary,
						right: other_index, operation: BinaryOperation::NotEqual,
							destination: temporary});
					self.opcode(OpCode::Jump {operation, r#if: Some(temporary)});
					self.opcode(OpCode::IndexWrite {indexee, index: zero, value: index});

					break indexee
				},

				(Some(value), _) => {
					if index == 0 {
						indexee = self.register();
						self.opcode(OpCode::Create {destination: indexee});
					}

					index += 1;
					let index = self.compile_known(index as i64);
					let value = value.register(self);
					self.opcode(OpCode::IndexWrite {indexee, index, value});
				},

				(None, _) => {
					if index == 0 {
						indexee = self.register();
						self.opcode(OpCode::Create {destination: indexee});
					}

					let value = self.compile_known(index as i64);
					let index = self.compile_known(0);
					self.opcode(OpCode::IndexWrite {indexee, index, value});
					break indexee
				}
			}
		}
	}

	fn compile_call(&mut self, function: &Expression, arguments: &[Expression])
			-> usize {
		let function = self.compile_expression(function).register(self);
		let arguments = self.compile_tuple(
			arguments.iter().map(CompileOrCompiled::r#dyn));
		let destination = self.register();
		self.opcode(OpCode::Call {function, arguments, destination});
		destination
	}

	fn compile_method_call(&mut self, class: &Expression, method: &str,
			arguments: &[Expression]) -> usize {
		let class = self.compile_expression(class);
		let arguments = self.compile_tuple(once(class.r#dyn())
			.chain(arguments.iter().map(CompileOrCompiled::r#dyn)));
		let method = self.compile_known(KnownValue::String(method.to_owned()));

		let class = class.register(self);
		let destination = self.register();
		self.opcode(OpCode::IndexRead {indexee: class, index: method, destination});
		self.opcode(OpCode::Call {function: destination, arguments, destination});
		destination
	}

	fn compile_function_header(&mut self, arguments: &[String], method: bool) {
		let indexee = 0; // Function arguments...

		method.then(|| "self".to_string()).iter().chain(arguments.iter())
				.enumerate().for_each(|(index, argument)| {
			let index = self.compile_known(index as i64 + 1);
			let value = self.register();
			self.opcode(OpCode::IndexRead {index, indexee, destination: value});
			self.variables_to_registers.insert(argument.clone(), value);
		});
	}

	fn prepare_side_effects(&mut self) {
		// We need to realize all values...
		#[allow(clippy::needless_collect)] // Needed by borrow checker.
		let evaluated = self.evaluated_variables.iter()
			.map(|(name, value)| (name.clone(), value.clone()))
			.collect::<Vec<_>>();
		evaluated.into_iter()
			.for_each(|(name, value)| {
				let register = self.compile_known(value);
				self.variables_to_registers.insert(name, register);
				self.registers[register] = None;
			});
	}
}

impl Default for Generator {
	fn default() -> Self {
		Self {
			constants: Vec::default(),
			evaluated_variables: HashMap::default(),
			opcodes: Vec::default(),
			registers: vec![None],
			up_values: HashMap::default(),
			variables_to_registers: HashMap::default()
		}
	}
}

fn iter_tuple<I>(values: I) -> impl FnMut(&mut Generator) -> CompileResult
		where I: ExactSizeIterator<Item = CompileResult> {
	enum State<I>
			where I: ExactSizeIterator<Item = CompileResult> {
		Values(I),
		Call {
			tuple: usize,
			index: i64,
			temporary: usize
		}
	}

	let mut this = State::Values(values);
	move |compiler| match &mut this {
		State::Values(values) => match values.next() {
			Some(CompileResult::Register(tuple, true)) if values.len() == 0 => {
				let temporary = compiler.register();
				this = State::Call {tuple, index: 1, temporary};
				let index = compiler.compile_known(1);
				insert_byte_code!(compiler {idxr tuple, index, temporary});
				CompileResult::Register(temporary, false)
			},
			Some(result) => result,
			None => CompileResult::Register(compiler.register(), false)
		},
		&mut State::Call {tuple, ref mut index, temporary} => {
			*index += 1;
			let index = compiler.compile_known(*index);
			insert_byte_code!(compiler {idxr tuple, index, temporary});
			CompileResult::Register(temporary, false)
		}
	}
}

// Silly trait because silly borrow checker problems. Perhaps this could be used
// more in normal code?
trait CompileOrCompiled {
	fn compile(&self, compiler: &mut Generator) -> CompileResult;

	fn r#dyn(&self) -> &dyn CompileOrCompiled
			where Self: Sized {
		self as &dyn CompileOrCompiled
	}
}

impl CompileOrCompiled for CompileResult {
	fn compile(&self, _: &mut Generator) -> CompileResult {
		self.clone()
	}
}

impl CompileOrCompiled for Expression {
	fn compile(&self, compiler: &mut Generator) -> CompileResult {
		compiler.compile_expression(self)
	}
}
