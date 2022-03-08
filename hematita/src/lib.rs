#![forbid(
	// Rationale: A Lua interpeter must be reliable, and should not be vulnerable
	// to sandbox escape vulnerabilities.
	unsafe_code
)]
#![warn(
	// Rationale: Panics should be avoided in favor of returning a Result, and
	// situations where panics are intentional should be well documented
	// (requiring an allow attribute to signal complete documentation).
	clippy::unwrap_used,
	clippy::panic,

	// Rationale: These should not be in production code.
	clippy::todo,
	clippy::unimplemented
)]
#![allow(
	// Rationale: Tabs are superior, don't at me.
	clippy::tabs_in_doc_comments,

	// Rationale: Drop is a valid way to ensure an expression returns a unit. It
	// can be used like JavaScript's void keyword.
	clippy::drop_copy
)]
//! Hematita Da Lua is an interpreter for the scripting language Lua, written
//! entirely in 100% safe Rust. Hematita is the portugese word for hematite, a
//! type of iron oxide, or rust, and lua is the portugese word for moon.
//! 'Hematita Da Lua' is a pin on what this project is, and the discovery that
//! [iron on the moon is rusting][moon-rust].
//!
//! This project is made of four primary components, each one more complicated
//! than the last.
//! - [ast::lexer] - The Lua lexer, responsible for tokenizing Lua source files
//! - [ast::parser] - The Lua parser, responsible for creating statements and
//!   expressions from tokens
//! - [compiler] - The compiler, responsible for turning parsed statements into
//!   bytecode that the virtual machine can understand
//! - [vm] - The virtual machine, responsible for executing bytecode vaguely
//!   representing Lua source code
//! On top of the four primary components, a Lua standard library is also
//! provided within [lua_lib].
//!
//! Examples
//! --------
//! Executing Lua source text is fairly straight forward, and only requires
//! stringing together each segment of the interpreter.
//! ```rust
//! use hematita::{ast::{lexer, parser}, compiler, vm, lua_lib, lua_tuple};
//! 
//! // Ready our Lua source code.
//! let source = "print(\"Hello, World!\")";
//! // Create a lexer (just a token iterator) from the characters of our source
//! // code.
//! let lexer = lexer::Lexer {source: source.chars().peekable()}.peekable();
//! // Parse from the lexer a block of statements.
//! let parsed = parser::parse_block(&mut parser::TokenIterator(lexer)).unwrap();
//! // Compile bytecode from the block of statements.
//! let compiled = compiler::compile_block(&parsed);
//!
//! // Prepare the global scope.
//! let global = lua_lib::standard_globals();
//! // Create the virtual machine...
//! let virtual_machine = vm::VirtualMachine::new(global);
//! // And run the byte code.
//! virtual_machine.execute(&compiled.into(), lua_tuple![].arc()).unwrap();
//! ```
//! Note that the `arguments` argument in [VirtualMachine::execute][vm-execute]
//! go unused for bytecode generated from [compile][compiler-compile], as top
//! level code doesn't have a concept of function arguments. If for any reason
//! you want to compile code with local arguments (not recommended over using
//! the global scope), you can use
//! [compile_function][compiler-compile_function].
//!
//! [moon-rust]: https://www.nasa.gov/feature/jpl/the-moon-is-rusting-and-researchers-want-to-know-why
//! [vm-execute]: crate::vm::VirtualMachine::execute
//! [compiler-compile]: crate::compiler::compile
//! [compiler-compile_function]: crate::compiler::compile_function

pub mod ast;
pub mod compiler;
pub mod lua_lib;
pub mod vm;

/// Macro implementation detail, do not use.
///
/// This does *NOT* follow the crates semantic version, using this attribute
/// *WILL* break your crate when *(not a matter of if)* we change this.
#[doc(hidden)]
pub use hashbrown as __priv_macro_rexport__hashbrown;

/// Macro implementation detail, do not use.
///
/// This does *NOT* follow the crates semantic version, using this attribute
/// *WILL* break your crate when *(not a matter of if)* we change this.
#[doc(hidden)]
pub use std as __priv_macro_rexport__std;
